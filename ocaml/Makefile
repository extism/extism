VERSION?=0.0.0

format:
	dune build @fmt --auto-promote

prepare:
	opam install .. --deps-only

publish:
	opam publish -v $(VERSION) -t $(VERSION) ..

build:
	dune build

test:
	dune test