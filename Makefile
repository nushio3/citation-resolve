all: build doc

.PHONY: init build doc test

init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build:
	cabal build

test: build
	cabal test

doc:
	cabal haddock --hyperlink-source
