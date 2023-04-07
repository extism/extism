VERSION?=0.2.0
TAG?=0.3.0

build:
	dune build

test:
	dune test
format:
	dune build @fmt --auto-promote

prepare:
	opam install .. --deps-only

publish:
	opam publish -v $(VERSION) https://github.com/extism/extism/archive/refs/tags/v$(TAG).tar.gz ..
