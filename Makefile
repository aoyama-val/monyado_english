all: nyanko

nyanko: nyanko.hs
	ghc nyanko.hs
	rm -rf *.hi *.o

clean:
	rm -rf *.hi *.o nyanko doc

doc:
	rm -rf ./doc
	haddock -h nyanko.hs -o ./doc

test:
	doctest nyanko.hs
