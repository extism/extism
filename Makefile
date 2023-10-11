DEST?=/usr/local
SOEXT=so
FEATURES?=default
DEFAULT_FEATURES?=yes

UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
	SOEXT=dylib
endif

ifeq ($(DEFAULT_FEATURES),no)
ifeq ($(FEATURES),default)
	FEATURE_FLAGS=--no-default-features
else
	FEATURE_FLAGS=--features $(FEATURES) --no-default-features
endif
else
	FEATURE_FLAGS=--features $(FEATURES)
endif

build:
	cargo build --release $(FEATURE_FLAGS) --manifest-path libextism/Cargo.toml

bench:
	@(cargo criterion || echo 'For nicer output use cargo-criterion: `cargo install cargo-criterion` - using `cargo bench`') && cargo bench

.PHONY: kernel
kernel:
	cd kernel && bash build.sh

lint:
	cargo clippy --release --no-deps --manifest-path runtime/Cargo.toml

debug:
	RUSTFLAGS=-g $(MAKE) build

install:
	mkdir -p $(DEST)/lib $(DEST)/include
	install runtime/extism.h $(DEST)/include/extism.h
	install target/release/libextism.$(SOEXT) $(DEST)/lib/libextism.$(SOEXT)

uninstall:
	rm -f $(DEST)/include/extism.h $(DEST)/lib/libextism.$(SOEXT)


