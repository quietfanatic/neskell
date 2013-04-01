#!/usr/bin/perl

use strict;
use warnings;
use FindBin;
BEGIN { %make:: or do { require "$FindBin::Bin/tool/make.pm"; make->import(':all') } }
use autodie qw(:all);

my $here;

workflow {

    sub doths { "$_[0]/$_[0].hs" }
    sub dotnes { "$_[0]/$_[0].nes" }
    sub dotexe { "$_[0]/$_[0].exe" }

    sub module {
        my ($name) = @_;
        rule dotexe($name), doths($name), sub {
            system 'ghc', '-ilib', '-fno-warn-deprecated-flags', doths($name), '-o', dotexe($name);
        };
        rule dotnes($name), dotexe($name), sub {
            system(dotexe($name) . ' > ' . dotnes($name));
        };
    }

    subdep 'lib/ASM.hs', 'lib/Assembler.hs';
    subdep 'lib/ASM6502.hs', 'lib/ASM.hs';
    subdep 'lib/NES.hs', 'lib/ASM6502.hs';
    subdep 'lib/NES/Reservations.hs', 'lib/NES.hs';
    subdep 'lib/NES/ASoundEngine.hs', 'lib/NES/Reservations.hs';
    subdep doths('soundtest'), 'lib/NES/ASoundEngine.hs';
    subdep doths('controllertest'), [qw(lib/NES.hs lib/NES/Reservations.hs lib/NES/ImageLoader.hs)];
    subdep dotexe('controllertest'), [qw(controllertest/sprites.png controllertest/background.bin)];
    module 'soundtest';
    module 'controllertest';

    phony 'build', [dotnes('soundtest'), dotnes('controllertest')], sub { };

    phony 'clean', [], sub {
        unlink glob '*/*.nes */*.exe */*.hi */*.o */*/*.hi */*/*.o';
    };

    defaults 'build';

}


