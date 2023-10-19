.PHONY: test

prepare:
	npm install

test: prepare
	npm run example
	npm run test

clean:
	echo "No clean implemented"

publish: clean prepare
	npm publish

format:
	npx prettier --write .

lint:
	npx prettier --check .

docs:
	npx typedoc --out doc src

show-docs: docs
	open doc/index.html
