.PHONY: test

prepare:
	npm install

test: prepare
	npm run test

clean:
	echo "No clean implemented"

publish: clean prepare
	npm publish

format:
	npx prettier --write src

lint:
	npx prettier --check src

docs:
	npx typedoc --out doc src

show-docs: docs
	open doc/index.html