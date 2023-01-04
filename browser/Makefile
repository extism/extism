.PHONY: test

prepare:
	npm install

build:
	npm run build

test: prepare
	npm run test

clean:
	echo "No clean implemented"

publish: clean prepare build
	npm publish

format:
	npx prettier --write src

lint:
	npx prettier --check src

docs:
	npx typedoc --out doc src

show-docs: docs
	open doc/index.html
