all: main.hs Nurikabe.hs
	ghc -O2 --make main.hs


test: Nurikabe_Test.hs Nurikabe.hs
	runhaskell Nurikabe_Test.hs

clean:
	rm -f *.hi *.ho *.o Nurikabe_Test main


