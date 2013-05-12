all: test

test: Nurikabe_Test
	./Nurikabe_Test

%.o: %.hs
	ghc -c $<

Nurikabe_Test: Nurikabe_Test.hs
	ghc $< -o Nurikabe_Test

clean:
	rm -f *.hi *.ho *.o Nurikabe_Test


