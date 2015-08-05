#!/usr/bin/perl
=cut

MakePl - Portable drop-in build system
https://github.com/quietfanatic/make-pl
2013-10-05

USAGE: See the README in the above repo.

=====LICENSE=====

The MIT License (MIT)

Copyright (c) 2013 Lewis Wall

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

=================

=cut

package MakePl;

use v5.10;
use strict qw(subs vars);
use warnings; no warnings 'once';
use utf8;
binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
use Carp 'croak';
use subs qw(cwd chdir);

our @EXPORT = qw(
    make rule phony subdep defaults include config option
    targets exists_or_target
    slurp splat slurp_utf8 splat_utf8
    run which
    cwd chdir canonpath abs2rel rel2abs
);

$ENV{PWD} //= do { require Cwd; Cwd::cwd() };

# GLOBALS
    my $original_base = cwd;  # Set once only.
    our $this_is_root = 1;  # This is set to 0 when recursing.
    our $current_file;  # Which make.pl we're processing
    my $this_file = rel2abs(__FILE__);
    my $make_was_called = 0;
# RULES AND STUFF
    my @rules;  # All registered rules
    my %phonies;  # Targets that aren't really files
    my %targets;  # List rules to build each target
    my %subdeps;  # Registered subdeps by file
    my @auto_subdeps;  # Functions that generate subdeps
    my %autoed_subdeps;  # Minimize calls to the above
    my $defaults;  # undef or array ref
# SYSTEM INTERACTION
    my %modtimes;  # Cache of file modification times
# CONFIGURATION
    my %configs;  # Set of registered config names, for cosmetic purposes only
    my %builtin_options;  # Defined later
    my %custom_options;  # Kept only for the help message
    my %options;  # Cache of command-line options
    my $force = 0; # Flags set from options
    my $verbose = 0;
    my $simulate = 0;
    my $touch = 0;
    my $jobs = 1;

# START, INCLUDE, END

    sub import {
        my $self = shift;
        my ($package, $file, $line) = caller;
        $current_file = rel2abs($file);
         # Export symbols
        my @args = (@_ == 0 or grep $_ eq ':all' || $_ eq ':ALL', @_)
            ? @EXPORT
            : @_;
        for my $f (@args) {
            grep $_ eq $f, @EXPORT or croak "No export '$f' in MakePl.";
            *{$package.'::'.$f} = \&{$f};
        }
         # Change to directory of the calling file
        $current_file =~ /^(.*)[\/\\]/ or die "path returned by rel2abs wasn't abs. ($current_file)";
        chdir $1;
         # Also import strict and warnings.
        strict->import();
        warnings->import();
    }

     # Prevent double-inclusion; can't use %INC because it does relative paths.
    my %included = (rel2abs($0) => 1);
    sub include {
        for (@_) {
            my $file = canonpath($_);
             # Error on specific files, but skip directories.
            -e $file or croak "Cannot include $file because it doesn't exist";
            if (-d $file) {
                my $makepl = "$file/make.pl";
                next unless -e $makepl;
                $file = $makepl;
            }
            my $real = rel2abs($file);
             # Just like a C include, a subdep is warranted.
            push @{$subdeps{$real}}, { base => MakePl::cwd, to => [$real], from => [$current_file] };
             # Skip already-included files
            next if $included{$real};
            $included{$real} = 1;
             # Make new project.
            local $this_is_root = 0;
            local $current_file;
            do {
                package main;
                my $old_cwd = MakePl::cwd;
                do $file;  # This file will do its own chdir
                MakePl::chdir $old_cwd;
            };
            $@ and die status($@);
            if (!$make_was_called) {
                die "\e[31m✗\e[0m $file did not end with 'make;'\n";
            }
            $make_was_called = 0;
            $defaults = undef;
        }
    }

    sub directory_prefix {
        my ($d, $base) = @_;
        $d //= cwd;
        $base //= $original_base;
        $d =~ s/\/*$//;
        $base =~ s/\/*$//;
        return $d eq $base
            ? ''
            : '[' . abs2rel($d, $base) . '/] ';
    }
    sub status {
        say directory_prefix(), @_;
        return "\n";  # Marker to hand to die
    }

    sub do_rule {
        my ($rule) = @_;
        if (!$simulate and defined $rule->{recipe}) {
            if ($touch) {
                for (@{$rule->{to}}) {
                    utime(undef, undef, $_);
                }
            }
            else {
                $rule->{recipe}->($rule->{to}, $rule->{from});
            }
        }
    }

    sub make () {
        if ($make_was_called) {
            say "\e[31m✗\e[0m make was called twice in the same project.";
            exit 1;
        }
        $make_was_called = 1;
        if ($this_is_root) {
             # Finish processing the command line
             # Recognize builtin options and complain at unrecognized ones
            my @args;
            eval {
                my $double_minus;
                for (@ARGV) {
                    if ($double_minus) {
                        push @args, $_;
                    }
                    elsif ($_ eq '--') {
                        $double_minus = 1;
                    }
                    elsif (/^--(no-)?([^=]*)(?:=(.*))?$/) {
                        my ($no, $name, $val) = ($1, $2, $3);
                        if (exists $custom_options{$name}) {
                             # We already processed this
                        }
                        elsif (my $opt = $builtin_options{$name}) {
                            if (ref $opt->{ref} eq 'SCALAR') {
                                ${$opt->{ref}} = $val // ($no ? 0 : 1);
                            }
                            else {
                                $opt->{ref}($val // ($no ? 0 : 1));
                            }
                        }
                        else {
                            say "\e[31m✗\e[0m Unrecognized option --$name.  Try --help to see available options.";
                            exit 1;
                        }
                    }
                    else {
                        push @args, $_;
                    }
                }
            };
            if ($@) {
                warn $@ unless "$@" eq "\n";
                say "\e[31m✗\e[0m Nothing was done due to command-line error.";
                exit 1;
            }
             # Make a plan to build the selected or default targets
            my $plan = init_plan();
            eval {
                if (@args) {
                    grep plan_target($plan, rel2abs($_, $original_base)), @args;
                }
                elsif ($defaults) {
                    grep plan_target($plan, $_), @$defaults;
                }
                else {
                    plan_rule($plan, $rules[0]);
                }
            };
            if ($@) {
                warn $@ unless "$@" eq "\n";
                say "\e[31m✗\e[0m Nothing was done due to error.";
                exit 1;
            }
             # Execute the plan.
            my @program = @{$plan->{program}};
            if (not @rules) {
                say "\e[32m✓\e[0m Nothing was done because no rules have been declared.";
            }
            elsif (not grep defined($_->{recipe}), @program) {
                say "\e[32m✓\e[0m All up to date.";
            }
            else {
                eval {
                    if ($jobs > 1) {
                        my %jobs;
                        $SIG{INT} = sub {
                            kill 2, $_ for keys %jobs;
                            die "interrupted\n";
                        };
                        $SIG{__DIE__} = sub {
                            kill 2, $_ for keys %jobs;
                            die $_[0];
                        };
                        my $do_wait;
                        $do_wait = sub {
                            keys(%jobs) > 0 or do {
                                die "Tried to wait on no jobs -- internal planner error?\n", join "\n", map show_rule($_), @program;
                            };
                            my $child = wait;
                            if ($child == -1) {
                                die "Unexpectedly lost children!\n";
                            }
                            if ($?) {
                                print readline($jobs{$child}{output});
                                close $jobs{$child}{output};
                                delete $jobs{$child};
                                 # Wait for more children
                                $do_wait->() if %jobs;
                                die "\n";
                            }
                            $jobs{$child}{done} = 1;
                            print readline($jobs{$child}{output});
                            close $jobs{$child}{output};
                            delete $jobs{$child};
                        };
                        while (@program || %jobs) {
                            $do_wait->() if keys(%jobs) >= $jobs;
                            my $rule;
                            for (0..$#program) {
                                next unless $program[$_]{options}{fork};
                                 # Don't run program if its deps haven't been finished
                                next if grep !$_->{done}, @{$program[$_]{follow}};
                                $rule = splice @program, $_, 1;
                                last;
                            }
                            if (defined $rule) {
                                chdir $rule->{base};
                                status $rule->{config} ? "⚒ " : "⚙ ", show_rule($rule);
                                delazify($rule);
                                pipe($rule->{output}, my $OUTPUT) or die "pipe failed: $!\n";
                                binmode $rule->{output}, ':utf8';
                                binmode $OUTPUT, ':utf8';
                                if (my $child = fork // die "Failed to fork: $!\n") {
                                     # parent
                                    $jobs{$child} = $rule;
                                }
                                else {  # child
                                     # Don't fall out of the eval {} out there
                                    $SIG{__DIE__} = sub { warn @_; exit 1; };
                                    close STDOUT;
                                    open STDOUT, '>&', $OUTPUT or die "Could not reopen STDOUT: $!\n";
                                    close STDERR;
                                    open STDERR, '>&', $OUTPUT or die "Could not reopen STDERR: $!\n";
                                    do_rule($rule);
                                    exit 0;
                                }
                                close $OUTPUT;
                            }
                            elsif (%jobs) {
                                $do_wait->();
                            }
                            else {  # Do a non-parallel job
                                my $rule = shift @program;
                                chdir $rule->{base};
                                status $rule->{config} ? "⚒ " : "⚙ ", show_rule($rule);
                                delazify($rule);
                                do_rule($rule);
                                $rule->{done} = 1;
                            }
                        }
                    }
                    else {
                        for my $rule (@program) {
                            chdir $rule->{base};
                            status $rule->{config} ? "⚒ " : "⚙ ", show_rule($rule);
                            delazify($rule);
                            do_rule($rule);
                        }
                    }
                };
                if ("$@" eq "interrupted\n") {
                    say "\e[31m✗\e[0m Interrupted.";
                    exit 1;
                }
                elsif ($@) {
                    warn $@ unless "$@" eq "\n";
                    say "\e[31m✗\e[0m Did not finish due to error.";
                    exit 1;
                }
                if ($simulate) {
                    say "\e[32m✓\e[0m Simulation finished.";
                }
                elsif ($touch) {
                    say "\e[32m✓\e[0m File modtimes updated.";
                }
                else {
                    say "\e[32m✓\e[0m Done.";
                }
            }
            exit 0;
        }
        1;
    }

     # Fuss if make wasn't called
    END {
        if ($? == 0 and !$make_was_called) {
            my $file = abs2rel($current_file, $original_base);
            warn "\e[31m✗\e[0m $file did not end with 'make;'\n";
        }
    }

# RULES AND DEPENDENCIES

    sub create_rule {
        my ($to, $from, $recipe, $options, $package, $file, $line) = @_;
        ref $recipe eq 'CODE' or !defined $recipe or croak "Non-code recipe given to rule";
        my $rule = {
            caller_file => $current_file,
            caller_line => $line,
            base => cwd,
            to => [arrayify($to)],
            from => lazify($from),
            deps => undef,  # Generated from from
            recipe => $recipe,
            options => $options,
            check_stale => undef,
            config => 0,
             # Intrusive state for planning and execution phases
            planned => 0,
            follow => [],
            done => 0,
            output => undef,
        };
        push @rules, $rule;
        for (@{$rule->{to}}) {
            push @{$targets{rel2abs($_)}}, $rule;
        }
    }

    sub rule ($$$;$) {
        create_rule($_[0], $_[1], $_[2], $_[3] // {}, caller);
    }

    sub phony ($;$$$) {
        my ($to, $from, $recipe, $options) = @_;
        for (arrayify($to)) {
            $phonies{rel2abs($_)} = 1;
        }
        create_rule($to, $from, $recipe, $options // {}, caller) if defined $from;
    }

    sub subdep ($;$) {
        my ($to, $from) = @_;
        if (ref $to eq 'CODE') {  # Auto
            push @auto_subdeps, {
                base => cwd,
                code => $to
            };
        }
        elsif (defined $from) {  # Manual
            my $subdep = {
                base => cwd,
                to => [arrayify($to)],
                from => lazify($from),
            };
            for (@{$subdep->{to}}) {
                push @{$subdeps{rel2abs($_)}}, $subdep;
            }
        }
        else {
            croak 'subdep must be called with two arguments unless the first is a CODE ref';
        }
    }

    sub defaults {
        push @$defaults, map rel2abs($_), @_;
    }

    sub targets {
        return keys %targets;
    }

    sub exists_or_target {
        return (-e $_[0] or exists $targets{rel2abs($_[0])});
    }

    sub arrayify {
        return ref $_[0] eq 'ARRAY' ? @{$_[0]} : $_[0];
    }
    sub lazify {
        my ($dep) = @_;
        return ref $dep eq 'CODE' ? $dep : [arrayify($dep)];
    }
    sub delazify {
         # Works on subdeps too
        my ($rule) = @_;
        if (ref $rule->{from} eq 'CODE') {
            $rule->{from} = [$rule->{from}(@{$rule->{to}})];
        }
    }

    sub get_auto_subdeps {
        return map {
            my $target = $_;
            @{$autoed_subdeps{$target} //= [
                map {
                    chdir $_->{base};
                    my @got = $_->{code}($target);
                    if (grep !defined, @got) {
                        warn "Warning: function that generated auto subdeps for $target returned an undefined value\n";
                    }
                    realpaths(grep defined, @got);
                } @auto_subdeps
            ]}
        } @_;
    }
    sub push_new {
        my ($deps, @new) = @_;
        push @$deps, grep {
            my $d = $_;
            not grep $d eq $_, @$deps;
        } @new;
    }
    sub resolve_deps {
        my ($rule) = @_;
        return if defined $rule->{deps};
         # Get the realpaths of all dependencies and their subdeps
        chdir $rule->{base};
        delazify($rule);
         # Depend on the build script and this module too.
        my @deps = (realpaths(@{$rule->{from}}), $rule->{caller_file}, $this_file);
         # Using this style of loop because @deps will keep expanding.
        for (my $i = 0; $i < @deps; $i++) {
            defined $deps[$i] or die "Undef dependency given to rule at $rule->{caller_file} line $rule->{caller_line}\n";
            push_new(\@deps, get_auto_subdeps($deps[$i]));
            for my $subdep (@{$subdeps{$deps[$i]}}) {
                chdir $subdep->{base};
                delazify($subdep);
                push_new(\@deps, realpaths(@{$subdep->{from}}));
            }
        }
        chdir $rule->{base};
        $rule->{deps} = [@deps];
    }

    sub show_rule ($) {
        if ($verbose) {
            resolve_deps($_[0]);
            return "@{$_[0]{to}} ← " . join ' ', map abs2rel($_), @{$_[0]{deps}};
        }
        else {
            my @froms = grep !$configs{rel2abs($_)}, @{$_[0]{from}};
            @froms or @froms = @{$_[0]{from}};
            return "@{$_[0]{to}} ← " . join ' ', @froms;
        }
    }
    sub debug_rule ($) {
        return "$_[0]{caller_file}:$_[0]{caller_line}: " . directory_prefix($_[0]{base}) . show_rule($_[0]);
    }

    sub target_is_default ($) {
        if (defined $defaults) {
            my $is = grep $_ eq $_[0], @$defaults;
            return $is;
        }
        else {
            my $rule = $rules[0];
            defined $rule or return 0;
            for (@{$rule->{to}}) {
                if (rel2abs($_, $rule->{base}) eq $_[0]) {
                    return 1;
                }
            }
            return 0;
        }
    }

# CONFIGURATION

    sub corrupted { return "\e[31m✗\e[0m Corrupted config file $_[0]$_[1]; please delete it and try again.\n"; }
    sub read_config {
        my ($file, $str) = @_;
        my ($val, $rest) = read_thing($file, $str);
        $rest eq '' or die corrupted($file, " (extra junk at end)");
        return $val;
    }
    sub read_thing {
        my ($file, $s) = @_;
        my $string_rx = qr/"((?:\\\\|\\"|[^\\"])*)"/s;
        if ($s =~ s/^\{//) {  # Hash
            my %r;
            unless ($s =~ s/^}//) {
                while (1) {
                    $s =~ s/^$string_rx://
                        or die corrupted($file, " (didn't find key after {)");
                    my $key = $1;
                    $key =~ s/\\([\\"])/$1/g;
                    (my $val, $s) = read_thing($file, $s);
                    $r{$key} = $val;
                    next if $s =~ s/^,//;
                    last if $s =~ s/^}//;
                    die corrupted($file, " (unrecognized char in hash)");
                }
            }
            return (\%r, $s);
        }
        elsif ($s =~ s/^\[//) {  # Array
            my @r;
            unless ($s =~ s/^]//) {
                while (1) {
                    (my $val, $s) = read_thing($file, $s);
                    push @r, $val;
                    next if $s =~ s/^,//;
                    last if $s =~ s/^]//;
                    die corrupted($file, " (unrecognized char in array)");
                }
            }
            return (\@r, $s);
        }
        elsif ($s =~ /^"/) {  # String
            $s =~ s/^$string_rx//
                or die corrupted($file, " (malformed string or something)");
            my $r = $1;
            $r =~ s/\\([\\"])/$1/g;
            return ($r, $s);
        }
        elsif ($s =~ s/^null//) {
            return (undef, $s);
        }
        else {
            die corrupted($file, " (unknown character in term position)");
        }
    }
    sub show_thing {
        my ($thing) = @_;
        if (not defined $thing) {
            return 'null';
        }
        elsif (ref $thing eq 'HASH') {
            my $r = '{';
            $r .= join ',', map {
                my $k = $_;
                $k =~ s/([\\"])/\\$1/g;
                "\"$k\":" . show_thing($thing->{$_});
            } sort keys %$thing;
            return $r . '}';
        }
        elsif (ref $thing eq 'ARRAY') {
            return '[' . (join ',', map show_thing($_), @$thing) . ']';
        }
        elsif (ref $thing eq '') {
            $thing =~ s/([\\"])/\\$1/g;
            return "\"$thing\"";
        }
        else {
            croak "Cannot serialize object of ref type '" . ref $thing . "'";
        }
    }

    sub config {
        my ($filename, $var, $routine) = @_;
        grep ref $var eq $_, qw(SCALAR ARRAY HASH)
            or croak "config's second argument is not a SCALAR, ARRAY, or HASH ref (It's a " . ref($var) . " ref)";
        !defined $routine or ref $routine eq 'CODE'
            or croak "config's third argument is not a CODE ref";
        my ($package, $file, $line) = caller;
        my $rule = {
            base => cwd,
            to => [$filename],
            from => [],
            deps => undef,
            check_stale => sub { stale_config($filename, $var); },
            recipe => sub { gen_config($filename, $var, $routine); },
            caller_file => $current_file,
            caller_line => $line,
            config => 1,
            options => {},
            stale => 0,
             # Intrusive state for planning and execution phases
            planned => 0,
            follow => [],
            executed => 0,
        };
        push @rules, $rule;
        push @{$targets{rel2abs($filename)}}, $rule;
        $configs{rel2abs($filename)} = 1;
         # Read into $var immediately
        if (-e $filename) {
            my $str = slurp_utf8($filename);
            chomp $str;
            my $val = read_config($filename, $str);
            if (ref $var eq 'SCALAR') {
                $$var = $val;
            }
            elsif (ref $var eq 'ARRAY') {
                ref $val eq 'ARRAY' or die corrupted($filename, " (expected ARRAY, got " . ref($val) . ")");
                @$var = @$val;
            }
            elsif (ref $var eq 'HASH') {
                ref $val eq 'HASH' or die corrupted($filename, " (expected HASH, got " . ref($val) . ")");
                %$var = %$val;
            }
        }
    }

    sub stale_config ($$) {
        my ($filename, $var) = @_;
        return 1 unless -e $filename;
        my $old = slurp_utf8($filename);
        chomp $old;
        my $new = show_thing(ref $var eq 'SCALAR' ? $$var : $var);
        return $new ne $old;
    }

    sub gen_config ($$$) {
        my ($filename, $var, $routine) = @_;
        $routine->() if defined $routine;
        my $new = show_thing(ref $var eq 'SCALAR' ? $$var : $var);
        splat_utf8($filename, "$new\n");
    }

    sub option ($$;$) {
        my ($name, $ref, $desc) = @_;
        if (ref $name eq 'ARRAY') {
            &option($_, $ref, $desc) for @$name;
            return;
        }
        elsif (ref $ref eq 'SCALAR' or ref $ref eq 'CODE') {
            $custom_options{$name} = {
                ref => $ref,
                desc => $desc,
                custom => 1
            };
            delete $builtin_options{$name};
        }
        else {
            croak "Second argument to option is not a SCALAR or CODE ref";
        }
         # Immediately find option.
        unless (%options) {
            for (@ARGV) {
                if ($_ eq '--') {
                    last;
                }
                elsif (/^--no-([^=]+)$/) {
                    $options{$1} = 0;
                }
                elsif (/^--([^=]+)(?:=(.*))?$/) {
                    $options{$1} = $2 // 1;
                }
            }
        }
        if (exists $options{$name}) {
            if (ref $ref eq 'SCALAR') {
                $$ref = $options{$name};
            }
            elsif (ref $ref eq 'CODE') {
                $ref->($options{$name});
            }
        }
    }

    %builtin_options = (
        help => {
            ref => sub {
                my (%nonfinal, %suggested, %nonsuggested, %default);
                for my $rule (@rules) {
                    resolve_deps($rule);
                    $nonfinal{$_} = 1 for @{$rule->{deps}};
                    if (defined $rule->{options}{suggested}) {
                        for (@{$rule->{to}}) {
                            if ($rule->{options}{suggested}) {
                                $suggested{rel2abs($_, $rule->{base})} = 1;
                            }
                            else {
                                $nonsuggested{rel2abs($_, $rule->{base})} = 1;
                            }
                        }
                    }
                }
                if (defined $defaults) {
                    for (@$defaults) {
                        $default{$_} = 1;
                    }
                }
                elsif (@rules) {
                    for (@{$rules[0]{to}}) {
                        $default{rel2abs($_, $rules[0]{base})} = 1;
                    }
                }
                 # Gradually narrow down criteria for suggestion
                my @suggested = grep {
                    ($default{$_} or $phonies{$_} or !$nonfinal{$_}) and not $nonsuggested{$_}
                } targets;
                if (@suggested > 12) {
                    @suggested = grep {
                        $default{$_} or !$nonfinal{$_} or $suggested{$_}
                    } @suggested;
                    if (@suggested > 12) {
                        @suggested = grep {
                            $default{$_} or $phonies{$_} or $suggested{$_}
                        } @suggested;
                        if (@suggested > 12) {
                            @suggested = grep {
                                $default{$_} or $suggested{$_}
                            } @suggested;
                        }
                    }
                }
                say "\e[31m✗\e[0m Usage: $0 <options> <targets>";
                if (%custom_options) {
                    say "Custom options:";
                    for (sort keys %custom_options) {
                        say "    ", $custom_options{$_}{desc} // "--$_";
                    }
                }
                if (%builtin_options) {
                    say "General options:";
                    for (sort keys %builtin_options) {
                        say "    $builtin_options{$_}{desc}";
                    }
                }
                say "Suggested targets:";
                for (sort @suggested) {
                    say "    ", abs2rel($_), target_is_default($_) ? " (default)" : "";
                }
                exit 1;
            },
            desc => "--help - show this help message",
            custom => 0
        },
        'list-targets' => {
            ref => sub {
                say "\e[31m✗\e[0m All targets:";
                for (sort keys %targets) {
                    say "    ", abs2rel($_), target_is_default($_) ? " (default)" : "";
                }
                exit 1;
            },
            desc => "--list-targets - list all declared targets",
            custom => 0
        },
        force => {
            ref => \$force,
            desc => '--force - Skip modification time checks and always run the rules',
            custom => 0
        },
        verbose => {
            ref => \$verbose,
            desc => '--verbose - Show sub-dependencies and shell commands',
            custom => 0
        },
        simulate => {
            ref => \$simulate,
            desc => '--simulate - Show rules that would be run but don\'t run them',
            custom => 0
        },
        touch => {
            ref => \$touch,
            desc => '--touch - Update existing files\' modtimes instead of running the rules',
            custom => 0
        },
        jobs => {
            ref => \$jobs,
            desc => '--jobs=<number> - Run this many parallel jobs if the rules support it',
            custom => 0
        },
    );

# SYSTEM INTERACTION

    sub cwd () {
        return $ENV{PWD};
    }
    sub chdir ($) {
        my $new = rel2abs($_[0]);
        if ($new ne cwd) {
            CORE::chdir $new or die "Failed to chdir to $new: $!\n";
            $ENV{PWD} = $new;
        }
    }
    sub fexists {
        defined $_[0] or Carp::confess "Undefined argument passed to fexists.";
        return 0 if $phonies{$_[0]};
        return -e $_[0];
    }
    sub modtime {
        return $modtimes{$_[0]} //= (fexists($_[0]) ? (stat $_[0])[9] : 0);
    }

    sub show_command (@) {
        my (@command) = @_;
        for (@command) {
            if (/\s/) {
                $_ =~ s/'/'\\''/g;
                $_ = "'$_'";
            }
        }
        return "\e[96m" . (join ' ', @command) . "\e[0m";
    }

    sub run (@) {
        if ($verbose) {
            say show_command(@_);
        }
        system(@_) == 0 or do {
            my @command = @_;
             # As per perldoc -f system
            if ($? == -1) {
                status("☢ Couldn't start command: $!");
            }
            elsif (($? & 127) == 2) {
                die "interrupted\n";
            }
            elsif ($? & 127) {
                status(sprintf "☢ Command died with signal %d, %s coredump",
                   ($? & 127),  ($? & 128) ? 'with' : 'without');
            }
            else {
                status(sprintf "☢ Command exited with value %d", $? >> 8);
            }
            die status("☢ Failed command: " . show_command(@_));
        }
    }

    sub realpaths (@) {
        return map rel2abs($_), @_;
    }

    sub canonpath {
        $_[0] eq '.' and return $_[0];
        if (index($_[0], '\\') == -1
        and index($_[0], '//') == -1
        and index($_[0], '/.') == -1
        and index($_[0], '/', length($_[0])-1) != length($_[0])-1) {
            return $_[0];
        }
        my $p = $_[0];
        $p =~ tr/\\/\//;
        1 while $p =~ s/\/(?:\.?|(?!\.\.\/)[^\/]*\/\.\.)(?=\/|$)//;
        return $p;
    }

    sub rel2abs {
        my ($rel, $base) = @_;
        $base //= cwd;
        return canonpath(rindex($rel, '/', 0) == 0 ? $rel : "$base/$rel");
    }
    sub abs2rel {
        my ($abs, $base) = @_;
        $abs = canonpath($abs);
        rindex($abs, '/', 0) == 0 or return $abs;
        $base = defined($base) ? canonpath($base) : cwd;
        if ($abs eq $base) {
            return '.';
        }
        if (rindex($abs, $base . '/', 0) == 0) {
            return substr($abs, length($base) + 1);
        }
        return $abs;
    }

    sub iofail { $_[0] or croak $_[1]; undef }

    sub slurp {
        my ($file, $bytes, $fail) = @_;
        $fail //= 1;
        open my $F, '<', $file or return iofail $fail, "Failed to open $file for reading: $! in call to slurp";
        my $r;
        if (defined $bytes) {
            defined read($F, $r, $bytes) or return iofail $fail, "Failed to read $file: $! in call to slurp";
        }
        else {
            local $/; $r = <$F>;
            defined $r or return $fail, "Failed to read $file: $! in call to slurp";
        }
        close $F or return $fail, "Failed to clode $file: $! in call to slurp";
        return $r;
    }
    sub splat {
        my ($file, $string, $fail) = @_;
        $fail //= 1;
        defined $string or return iofail $fail, "Cannot splat undef to $file";
        open my $F, '>', $file or return iofail $fail, "Failed to open $file for writing: $! in call to splat";
        print $F $string or return iofail $fail, "Failed to write to $file: $! in call to splat";
        close $F or return iofail $fail, "Failed to close $file: $! in call to close";
    }
    sub slurp_utf8 {
        require Encode;
        return Encode::decode_utf8(slurp(@_));
    }
    sub splat_utf8 {
        require Encode;
        splat($_[0], Encode::encode_utf8($_[1]), $_[2]);
    }

    sub which {
        my ($cmd) = @_;
        for (split /[:;]/, $ENV{PATH}) {
            my $f = "$_/$cmd";
            return $f if -x $f;
            if (exists $ENV{PATHEXT}) {
                for my $ext (split /;/, $ENV{PATHEXT}) {
                    my $f = "$_/$cmd$ext";
                    return $f if -x $f;
                }
            }
        }
        return undef;
    }

# PLANNING

    sub init_plan {
        return {  # We had and might have more real stuff here
            stack => [],
            program => []
        };
    }

    sub plan_target {
        my ($plan, $target) = @_;
         # Make sure the file exists or there's a rule for it
        unless ($targets{$target} or fexists($target)) {
            my $rel = abs2rel($target, $original_base);
            my $mess = "☢ Cannot find or make $rel ($target)" . (@{$plan->{stack}} ? ", required by\n" : "\n");
            for my $rule (reverse @{$plan->{stack}}) {
                $mess .= "\t" . debug_rule($rule) . "\n";
            }
            die status($mess);
        }
         # In general, there should be only rule per target, but there can be more.
        return grep plan_rule($plan, $_), @{$targets{$target}};
    }

    sub plan_rule {
        my ($plan, $rule) = @_;
         # Register dependency for parallel scheduling.
        if (@{$plan->{stack}}) {
            push @{$plan->{stack}[-1]{follow}}, $rule;
        }
         # detect loops
        if (not defined $rule->{planned}) {
            my $mess = "☢ Dependency loop\n";
            for my $old (reverse @{$plan->{stack}}) {
                $mess .= "\t" . debug_rule($old) . "\n";
                die status($mess) if $rule eq $old;  # reference compare
            }
            Carp::confess $mess . "\t...oh wait, false alarm.  Which means there's a bug in make.pm.\nDetected";
        }
        elsif ($rule->{planned}) {
            return $rule->{stale};  # Already planned
        }
         # Commit to planning
        push @{$plan->{stack}}, $rule;
        $rule->{planned} = undef;  # Mark that we're currently planning this

        resolve_deps($rule);
         # always recurse to plan_target
        my $stale = grep plan_target($plan, $_), @{$rule->{deps}};
         # chdir precisely now.
        chdir $rule->{base};
        $stale ||= $force;
        $stale ||= $rule->{check_stale}() if defined $rule->{check_stale};
        $stale ||= grep {
            my $abs = rel2abs($_);
            !fexists($abs) or grep modtime($abs) < modtime($_), @{$rule->{deps}};
        } @{$rule->{to}};
        if ($stale) {
            push @{$plan->{program}}, $rule;
        }
        else {
            $rule->{done} = 1;  # Don't confuse parallel scheduler.
        }
         # Done planning this rule
        $rule->{planned} = 1;
        $rule->{stale} = $stale;
        pop @{$plan->{stack}};
        return $stale;
    }

# RUNNING THIS FILE DIRECTLY

 # Generate a make.pl scaffold.
if ($^S == 0) {  # We've been called directly
    $make_was_called = 1;  # Not really but supresses warning
    if (@ARGV > 1 or (defined $ARGV[0] and $ARGV[0] =~ /-?-h(?:elp)?/)) {
        say "\e[31m✗\e[0m Usage: perl $0 <directory (default: .)>";
        exit 1;
    }
    my $loc = defined $ARGV[0] ? canonpath($ARGV[0]) : cwd;
    $loc = "$loc/make.pl" if -d $loc;
    if (-e $loc) {
        say "\e[31m✗\e[0m Did not generate $loc because it already exists.";
        exit 1;
    }
    my $dir = $loc =~ /^(.*)\/[^\/]*$/ ? $1 : cwd;
    my $path_to_pm = abs2rel(rel2abs(__FILE__), $dir);
    $path_to_pm =~ s/\/?MakePl\.pm$//;
    $path_to_pm =~ s/'/\\'/g;
    my $pathext = $path_to_pm eq '' ? '' : ".'/$path_to_pm'";
    local $/;
    my $out = <DATA>;
    $out =~ s/◀PATHEXT▶/$pathext/;
    open my $MAKEPL, '>:utf8', $loc or die "Failed to open $loc for writing: $!\n";
    print $MAKEPL $out or die "Failed to write to $loc: $!\n";
    chmod 0755, $MAKEPL or warn "Failed to chmod $loc: $!\n";
    close $MAKEPL or die "Failed to close $loc: $!\n";
    say "\e[32m✓\e[0m Generated $loc.";
}

1;

__DATA__
#!/usr/bin/perl
use lib do {__FILE__ =~ /^(.*)[\/\\]/; ($1||'.')◀PATHEXT▶};
use MakePl;

 # Sample rules
rule \$program, \$main, sub {
    run "gcc -Wall \\Q\$main\\E -o \\Q\$program\\E";
};
rule 'clean', [], sub { unlink \$program; };

make;
