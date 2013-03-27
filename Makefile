
build : soundtest/soundtest.nes controllertest/controllertest.nes

soundtest/soundtest.nes : soundtest/soundtest.hs lib/*
	ghc -ilib soundtest/soundtest.hs -o soundtest/soundtest.exe && soundtest/soundtest.exe > soundtest/soundtest.nes
controllertest/controllertest.nes : controllertest/controllertest.hs controllertest/sprites.bin controllertest/background.bin lib/*
	ghc -ilib controllertest/controllertest.hs -o controllertest/controllertest.exe && controllertest/controllertest.exe > controllertest/controllertest.nes

.PHONY: clean

clean:
	rm lib/*.o lib/*.hi
	rm soundtest/soundtest.o soundtest/soundtest.hi soundtest/soundtest.nes
	rm controllertest/controllertest.o controllertest/controllertest.hi controllertest/controllertest.nes
