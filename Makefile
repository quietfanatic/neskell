
soundtest/soundtest.nes : soundtest/soundtest.hs lib/*
	ghc -ilib soundtest/soundtest.hs -o soundtest/soundtest.exe && soundtest/soundtest.exe > soundtest/soundtest.nes

.PHONY: clean

clean:
	rm lib/*.o lib/*.hi
	rm soundtest/soundtest.o soundtest/soundtest.hi soundtest/soundtest.nes
