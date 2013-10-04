#!/usr/bin/perl
use lib do {__FILE__ =~ /^(.*)[\/\\]/; ($1||'.').'/tool'};
use MakePl;

my $here;
my @modules;

sub doths { "$_[0]/$_[0].hs" }
sub dotnes { "$_[0]/$_[0].nes" }

sub module {
    my ($name) = @_;
    rule dotnes($name), doths($name), sub {
        run "runghc -ilib -i'$name' '$name/$name.hs' > '$name/$name.nes'";
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
subdep doths('controllertest'), [qw(controllertest/sprites.png controllertest/background.bin)];
subdep 'agame/Actors.hs', 'lib/NES/Reservations.hs';
subdep doths('agame'), [qw(agame/Actors.hs lib/NES/Reservations.hs)];
subdep doths('agame'), [qw(agame/sprites.png agame/background.png)];
module 'soundtest';
module 'controllertest';
module 'agame';

phony 'build', [@modules], sub { };

phony 'clean', [], sub {
    unlink glob '*/*.nes */*.exe */*.hi */*.o */*/*.hi */*/*.o';
};

defaults 'build';

make;


