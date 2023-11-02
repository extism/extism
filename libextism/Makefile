.PHONY: build
build:
	$(CC) -g -o example example.c -lextism

.PHONY: static
static:
	$(CC) -g -o example example.c -l:libextism.a -lm

# if needed, set PKG_CONFIG_PATH= to the directory with extism*.pc installed
LDFLAGS=`pkg-config --libs extism`
.PHONY: pkg-config
pkg-config:
	$(CC) -g -o example example.c $(LDFLAGS)

LDFLAGS_STATIC=`pkg-config --static --libs extism-static`
.PHONY: pkg-config-static
pkg-config-static:
	$(CC) -g -o example example.c $(LDFLAGS_STATIC)

.PHONY: clean
clean:
	rm -f example
