.PHONY: build
build:
	$(CC) -g -o example example.c -lextism

.PHONY: static
static:
	$(CC) -g -o example example.c -l:libextism.a -lm

.PHONY: clean
clean:
	rm -f main
