.PHONY: build
build:
	$(CC) -g -o main main.c -lextism -L .

.PHONY: static
static:
	$(CC) -g -o main main.c -l:libextism.a -lm -L .

.PHONY: clean
clean:
	rm -f main