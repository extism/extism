VERSION?=0.2.0

build:
	dune build

test:
	dune test
format:
	dune build @fmt --auto-promote

prepare:
	opam install .. --deps-only

publish:
	opam publish -v $(VERSION) -t $(VERSION) ..
