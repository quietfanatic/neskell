#!/usr/bin/perl
use lib do {__FILE__ =~ /^(.*)[\/\\]/; ($1||'.').'/tool'};
use MakePl;

my $here;
my @modules;

sub module {
    my ($name) = @_;
    rule "$name/$name.nes", "$name/$name.hs", sub {
        run "runghc -ilib -i'$name' '$name/$name.hs' > '$name/$name.nes'";
    };
    push @modules, "$name/$name.nes";
}
 # Compile modules for future efficiency
for (glob "*/*.hs */*/*.hs") {
    $_ =~ /^(.*)\.hs$/;
    rule "$1.o", "$1.hs", sub {
        run "ghc -ilib $_[1][0]"
    };
}

 # Provide subdeps in the files themselves
subdep sub {
    my ($file) = @_;
    $file =~ /\.hs$/ or return ();
    my $base = ($file =~ /(.*?)[^\\\/]*$/ and $1);
    my @imports = (slurp $file, 2048) =~ /import\s+(?:qualified\s+)?([A-Za-z0-9_.]+)/g;
    my @deps;
    for my $f (@imports) {
        $f =~ s/\./\//g;
        if (exists_or_target "$base$f.hs") {
            push @deps, "$base$f.o";
        }
        elsif (exists_or_target "lib/$f.hs") {
            push @deps, "lib/$f.o";
        }
    }
    return @deps;
};

subdep 'controllertest/controllertest.hs', [qw(controllertest/sprites.png controllertest/background.bin)];
subdep 'agame/agame.hs', [qw(agame/sprites.png agame/background.png)];
module 'soundtest';
module 'controllertest';
module 'agame';

phony 'build', [@modules];

phony 'clean', [], sub {
    unlink glob '*/*.nes */*.exe */*.hi */*.o */*/*.hi */*/*.o';
};

defaults 'build';

make;


