#!/usr/bin/perl

use strict;
use warnings;
use FindBin;
use if !$^S, lib => "$FindBin::Bin/tool";
use Make_pl;
use autodie;

my $here;
my @modules;

workflow {

    sub doths { "$_[0]/$_[0].hs" }
    sub dotnes { "$_[0]/$_[0].nes" }
    sub dotexe { "$_[0]/$_[0].exe" }

    sub module {
        my ($name) = @_;
        rule dotexe($name), doths($name), sub {
            run 'ghc', '-ilib', "-i$name", '-fno-warn-deprecated-flags', doths($name), '-o', dotexe($name);
        };
        rule dotnes($name), dotexe($name), sub {
            run(dotexe($name) . ' > ' . dotnes($name));
        };
        push @modules, dotnes($name);
    }

    subdep 'lib/ASM.hs', 'lib/Assembler.hs';
    subdep 'lib/ASM6502.hs', 'lib/ASM.hs';
    subdep 'lib/NES.hs', 'lib/ASM6502.hs';
    subdep 'lib/NES/Reservations.hs', 'lib/NES.hs';
    subdep 'lib/NES/ASoundEngine.hs', 'lib/NES/Reservations.hs';
    subdep doths('soundtest'), 'lib/NES/ASoundEngine.hs';
    subdep doths('controllertest'), [qw(lib/NES.hs lib/NES/Reservations.hs lib/NES/ImageLoader.hs)];
    subdep dotexe('controllertest'), [qw(controllertest/sprites.png controllertest/background.bin)];
    subdep 'agame/Actors.hs', 'lib/NES/Reservations.hs';
    subdep doths('agame'), [qw(agame/Actors.hs lib/NES/Reservations.hs)];
    subdep dotexe('agame'), [qw(agame/sprites.png agame/background.png)];
    module 'soundtest';
    module 'controllertest';
    module 'agame';

    phony 'build', [@modules], sub { };

    phony 'clean', [], sub {
        unlink glob '*/*.nes */*.exe */*.hi */*.o */*/*.hi */*/*.o';
    };

    defaults 'build';

}


