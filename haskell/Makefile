.PHONY: test

prepare:
	cabal update

build: prepare
	cabal build

test: prepare
	cabal test

clean:
	cabal clean

publish: clean prepare
	cabal sdist
	# TODO: upload

format:
	# TODO

lint:
	cabal check
	hlint src manifest

docs:
	# TODO

show-docs: docs
	# TODO
