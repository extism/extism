.PHONY: test

prepare:
	poetry install

test: prepare
	poetry run python -m unittest discover

clean:
	rm -rf dist/*

publish: clean prepare
	poetry build
	poetry run twine upload dist/extism-*.tar.gz

format:
	poetry run black extism/ tests/ example.py

lint:
	poetry run black --check extism/ tests/ example.py

docs:
	poetry run pdoc --force --html extism

show-docs: docs
	open html/extism/index.html
