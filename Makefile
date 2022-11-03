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


.PHONY: build

lint:
	cargo clippy --release --no-deps --manifest-path runtime/Cargo.toml

build:
	cargo build --release $(FEATURE_FLAGS) --manifest-path libextism/Cargo.toml

install:
	install runtime/extism.h $(DEST)/include
	install target/release/libextism.$(SOEXT) $(DEST)/lib

uninstall:
	rm -f $(DEST)/include/extism.h $(DEST)/lib/libextism.$(SOEXT)


