
.PHONY: prepare test

prepare:
	bundle install
	bundle binstubs --all

test: prepare
	bundle exec rake test

clean:
	rm extism-*.gem

publish: clean prepare
	gem build extism.gemspec
	gem push extism-*.gem

lint:
	bundle exec rufo --check .

format:
	bundle exec rufo .

docs:
	bundle exec yard

show-docs: docs
	open doc/index.html