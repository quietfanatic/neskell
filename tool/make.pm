#!/usr/bin/perl
package make;
use strict;
use warnings;
use feature qw(switch say);
use autodie;
no autodie 'chdir';
use Exporter qw(import);
use Carp qw(croak);
use Cwd qw(cwd realpath);
use File::Spec::Functions qw(:ALL);

our @EXPORT_OK = qw(workflow rule rules phony subdep defaults include chdir targetmatch run);
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

 # A "target" is a reference to either a file or a phony.
 # A "rule" has one or more "to" targets and zero or more "from" targets.

 # Target structures should be implicitly created when mentioned.  In rules they
 # will be stored relative to the rule's base.  Everywhere else they will be stored
 # with absolute paths.  Rule bases are stored absolute but displayed relative to the
 # original base.

 # This variable is only defined inside a workflow definition.
our %workflow;
 # This is set to 0 when recursing.
our $this_is_root = 1;
 # Set once only.
our $original_base = cwd;
 # Prevent double-inclusion; can't use %INC because it does relative paths.
our @included = realpath($0);
 # A cache of file modification times.  It's probably safe to keep until exit.
my %modtimes;

##### DEFINING WORKFLOWS

sub workflow (&) {
    %workflow and croak "workflow was called inside a workflow (did you use 'do' instead of 'include'?)";
    my ($definition) = @_;
    my ($package, $file, $line) = caller;
    %workflow = (
        caller_package => $package,
        caller_file => $file,
        caller_line => $line,
        rules => [],
        targets => {},
        subdeps => {},
        auto_subdeps => [],
        autoed_subdeps => {},
        phonies => {},
        defaults => undef,
    );
     # Get directory of the calling file, which may not be cwd
    my @vdf = splitpath(rel2abs($file));
    my $base = catpath($vdf[0], $vdf[1], '');
    my $old_cwd = cwd;
    Cwd::chdir $base;
        $definition->();
    Cwd::chdir $old_cwd;
    if ($this_is_root) {
        exit(!run_workflow(@ARGV));
    }
}


### DECLARING RULES

 # caller abstracted out because phony() delegates to this as well.
sub rule_with_caller ($$$$$$) {
    my ($package, $file, $line, $to, $from, $recipe) = @_;
    ref $recipe eq 'CODE' or croak "Non-code recipe given to rule";
    my $rule = {
        base => cwd,
        to => [arrayify($to)],
        from => lazify($from),
        recipe => $recipe,
        caller_file => $file,
        caller_line => $line,
        planned => 0,  # Intrusive state for the planning phase
    };
    push @{$workflow{rules}}, $rule;
    for my $to (@{$rule->{to}}) {
        push @{$workflow{targets}{realpath($to)}}, $rule;
    }
}
sub rule ($$$) {
    %workflow or croak "rule was called outside of a workflow";
    my ($to, $from, $recipe) = @_;
    my ($package, $file, $line) = caller;
    rule_with_caller($package, $file, $line, $to, $from, $recipe);
}
sub phony ($;$$) {
    %workflow or croak "phony was called outside of a workflow";
    @_ == 2 and croak "phony was given 2 arguments, but it must have either 1 or 3";
    my ($phony, $from, $recipe) = @_;
    for my $p (arrayify($phony)) {
        $workflow{phonies}{realpath($p)} = 1;
    }
    if (defined $from) {
        my ($package, $file, $line) = caller;
        rule_with_caller($package, $file, $line, $phony, $from, $recipe);
    }
}
sub rules ($$) {
    %workflow or croak "rules was called outside of a workflow";
    my ($tofroms, $recipe) = @_;
    ref $tofroms eq 'ARRAY' or croak "First argument to rules wasn't an ARRAY ref";
    for (@{$tofroms}) {
        @$_ == 2 or croak "Each of the elements of the first argument to rules must have two elements";
        my ($package, $file, $line) = caller;
        rule_with_caller($package, $file, $line, $_->[0], $_->[1], $recipe);
    }
}
sub subdep ($;$) {
    %workflow or croak "subdep was called outside of a workflow";
    my ($to, $from) = @_;
    if (ref $to eq 'CODE') {
        push @{$workflow{auto_subdeps}}, {
            base => cwd,
            code => $to
        };
    }
    elsif (defined $from) {
        my $subdep = {
            base => cwd,
            to => [arrayify($to)],
            from => lazify($from),
        };
        for my $to (@{$subdep->{to}}) {
            my $rp = realpath($to);
            push @{$workflow{subdeps}{$rp}}, $subdep;
        }
    }
    else {
        croak 'subdep must be called with two arguments unless the first is a CODE ref';
    }
}
sub arrayify {
    return ref $_[0] eq 'ARRAY' ? @{$_[0]} : $_[0];
}
sub lazify {
    my ($dep) = @_;
    return ref $dep eq 'CODE' ? $dep : [arrayify($dep)];
}
sub delazify {
    my ($dep, @args) = @_;
    return ref $dep eq 'CODE' ? $dep->(@args) : @$dep;
}


##### OTHER DECLARATIONS

sub defaults {
    push @{$workflow{defaults}}, map realpath($_), @_;
}
sub include {
    for (@_) {
        my $file = $_;
        -e $file or croak "Cannot include $file because it doesn't exist.";
        if (-d $file) {
            my $makepl = catfile($file, 'make.pl');
            next unless -e $makepl;
            $file = $makepl;
        }
         # Skip already-included files
        my $real = realpath($file);
        next if grep $real eq $_, @included;
        push @included, $real;

        my $this_workflow = \%workflow;
        local $this_is_root = 0;
        local %workflow;
        do {
            package main;
            do $file;
            $@ and die_status $@;
        };
        return unless %workflow;  # Oops, it wasn't a make.pl, but we did it anyway
         # merge workflows
        push @{$this_workflow->{rules}}, @{$workflow{rules}};
        for (keys %{$workflow{targets}}) {
            push @{$this_workflow->{targets}{$_}}, @{$workflow{targets}{$_}};
        }
        $this_workflow->{phonies} = {%{$this_workflow->{phonies}}, %{$workflow{phonies}}};
        for (keys %{$workflow{subdeps}}) {
            push @{$this_workflow->{subdeps}{$_}}, @{$workflow{subdeps}{$_}};
        }
    }
}

sub chdir (;$) {
    goto &Cwd::chdir;  # Re-export, basically
}

##### UTILITIES

sub targetmatch {
    my ($rx) = @_;
    return grep $_ =~ $rx, map abs2rel($_), keys %{$workflow{targets}};
}

sub run (@) {
    require IPC::System::Simple;
    eval { IPC::System::Simple::system(@_) };
    if ($@) {
        warn $@;
        my @command = @_;
        ref $_[0] eq 'ARRAY' and shift @command;
        for (@command) {
            if (/\s/) {
                $_ =~ s/'/'\\''/g;
                $_ = "'$_'";
            }
        }
        status("☢ Command failed: @command\n");
        die "\n";
    }
}
sub realpaths (@) {
    return map {
        my $r = realpath($_);
        unless (defined $r) {
            my $abs = rel2abs($_);
            croak "\"$abs\" doesn't seem to be a real path";
        }
        $r;
    } @_;
}

##### PRINTING ETC.

sub directory_prefix {
    my ($d, $base) = @_;
    $d //= cwd;
    $base //= $original_base;
    return $d eq $base
        ? ''
        : '[' . abs2rel($d, $base) . '/] ';
}
sub status {
    say directory_prefix(), @_;
}
sub die_status {
    status @_;
    die "\n";
}
sub show_rule ($) {
    return "@{$_[0]{to}} ← " . join ' ', delazify($_[0]{from}, $_[0]{to});
}
sub debug_rule ($) {
    return "$_[0]{caller_file}:$_[0]{caller_line}: " . directory_prefix($_[0]{base}) . show_rule($_[0]);
}

##### FILE INSPECTION UTILITIES
 # These work with absolute paths.

sub fexists {
    return 0 if $workflow{phonies}{$_[0]};
    return -e $_[0];
}
sub modtime {
    return $modtimes{$_[0]} //= (fexists($_[0]) ? (stat $_[0])[9] : 0);
}

 # This routine is stale.
#sub stale {
#    my $target = $_[0];
#    return 1 if (!fexists($_[0]));
#    for (@{$workflow{targets}{$target}}) {
#        for (@{$_->{from}}) {
#            return 1 if stale($_) or modtime($target) < modtime($_);
#        }
#    }
#}

##### PLANNING

sub init_plan {
    return {  # We had and might have more real stuff here
        stack => [],
        program => []
    };
}

sub plan_target {
    my ($plan, $target) = @_;
     # Make sure the file exists or there's a rule for it
    my $rel = abs2rel($target, $original_base);
    unless ($workflow{targets}{$target} or fexists($target)) {
        my $mess = "☢ Cannot find or make $rel" . (@{$plan->{stack}} ? ", required by\n" : "\n");
        for my $rule (@{$plan->{stack}}) {
            $mess .= "\t" . debug_rule($rule) . "\n";
        }
        die_status $mess;
    }
     # In general, there should be only rule per target, but there can be more.
    return grep plan_rule($plan, $_), @{$workflow{targets}{$target}};
}

sub get_auto_subdeps {
    my $old_cwd = cwd;
    my @r = map {
        my $target = $_;
        @{$workflow{autoed_subdeps}{$target} //= [
            map {
                Cwd::chdir $_->{base};
                realpaths($_->{code}($target));
            } @{$workflow{auto_subdeps}}
        ]}
    } @_;
    Cwd::chdir $old_cwd;
    return @r;
}

sub add_subdeps {
    my @deps = @_;
    my $old_cwd = cwd;
     # Using this style of loop because @deps will keep expanding.
    for (my $i = 0; $i < @deps; $i++) {
        push @deps, grep { my $d = $_; not grep $d eq $_, @deps } get_auto_subdeps($deps[$i]);
        for my $subdep (@{$workflow{subdeps}{$deps[$i]}}) {
            Cwd::chdir $subdep->{base};
            $subdep->{from} = [delazify($subdep->{from}, $subdep->{to})];
            push @deps, grep { my $d = $_; not grep $d eq $_, @deps } realpaths(@{$subdep->{from}});
        }
    }
    Cwd::chdir $old_cwd;
    return @deps;
}

sub resolve_deps {
    my ($rule) = @_;
     # Get the realpaths of all dependencies and their subdeps
    Cwd::chdir $rule->{base};
    $rule->{from} = [delazify($rule->{from}, $rule->{to})];
    return add_subdeps(realpaths(@{$rule->{from}}));
}

sub plan_rule {
    my ($plan, $rule) = @_;
    Cwd::chdir $rule->{base};
     # detect loops
    if (not defined $rule->{planned}) {
        my $mess = "☢ Dependency loop\n";
        for my $old (reverse @{$plan->{stack}}) {
            $mess .= "\t" . debug_rule($old) . "\n";
            die_status $mess if $rule eq $old;  # reference compare
        }
        Carp::confess $mess . "\t...oh wait, false alarm.  Which means there's a bug in make.pm.\nDetected";
    }
    elsif ($rule->{planned}) {
        return 1;  # Already planned, but we'll still cause updates
    }
    push @{$plan->{stack}}, $rule;
    $rule->{planned} = undef;  # Mark that we're currently planning this

     # Now is when we officially collapse lazy dependencies and stuff like that
    my @deps = resolve_deps($rule);
     # always recurse to plan_target
    my $stale = grep plan_target($plan, $_), @deps;
    $stale ||= grep {
        my $abs = realpath(rel2abs($_, $rule->{base}));
        !fexists($abs) or grep modtime($abs) < modtime($_), @deps;
    } @{$rule->{to}};
    push @{$plan->{program}}, $rule if $stale;
     # Done planning this rule
    $rule->{planned} = 1;
    pop @{$plan->{stack}};
    return $stale;
}

sub plan_workflow(@) {
    my (@args) = @_;
    my $plan = init_plan();
    if (@args) {
        grep plan_target($plan, realpath($_)), @args;
    }
    elsif ($workflow{defaults}) {
        grep plan_target($plan, $_), @{$workflow{defaults}};
    }
    else {
        plan_rule($plan, $workflow{rules}[0]);
    }
    return @{$plan->{program}};
}

##### RUNNING

sub run_workflow {
    my (@args) = @_;
    if (not @{$workflow{rules}}) {
        say "\e[32m✓\e[0m Nothing was done because no rules have been declared.";
        return 1;
    }
    my @program = eval { plan_workflow(@args) };
    if ($@) {
        warn $@ unless $@ eq "\n";
        say "\e[31m✗\e[0m Nothing was done due to error.";
        return 0;
    }
    if (not @program) {
        say "\e[32m✓\e[0m All up to date.";
        return 1;
    }
    my $old_cwd = cwd;
    for my $rule (@program) {
        Cwd::chdir rel2abs($rule->{base});
        status "⚙ ", show_rule($rule);
        eval { $rule->{recipe}->($rule->{to}, $rule->{from}) };
        if ($@) {
            warn $@ unless $@ eq "\n";
            say "\e[31m✗\e[0m Did not finish due to error.";
            Cwd::chdir $old_cwd;
            return 0;
        }
    }
    say "\e[32m✓\e[0m Done.";
    Cwd::chdir $old_cwd;
    return 1;
}



1;
