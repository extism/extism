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

# This produces an entirely static binary
# You probably need to specifically build libextism for musl first,
# For x86_64 in the parent directory:
# make RUST_TARGET=x86_64-unknown-linux-musl && sudo make RUST_TARGET=x86_64-unknown-linux-musl install
.PHONY: musl-static
musl-static:
	musl-gcc -static -g -o example example.c $(LDFLAGS_STATIC)

.PHONY: clean
clean:
	rm -f example
