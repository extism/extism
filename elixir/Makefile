.PHONY: test

prepare:
	mix deps.get
	mix compile

test: prepare
	mix test

clean:
	mix clean

publish: clean prepare
	mix hex.build
	mix hex.publish --yes

format:
	mix format

lint:
	mix format --check-formatted

docs:
	mix docs

show-docs: docs
	open doc/index.html
