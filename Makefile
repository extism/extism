DEST?=/usr/local
SOEXT=so
AEXT=a
FEATURES?=default
DEFAULT_FEATURES?=yes
RUST_TARGET?=
EXTRA_LIBS=

UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
	SOEXT=dylib
	EXTRA_LIBS=-framework Security
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

ifeq ($(RUST_TARGET),)
	TARGET_FLAGS=
else
	TARGET_FLAGS=--target $(RUST_TARGET)
endif

build:
	cargo build --release $(FEATURE_FLAGS) --manifest-path libextism/Cargo.toml $(TARGET_FLAGS)
	sed -e "s%@CMAKE_INSTALL_PREFIX@%$(DEST)%" libextism/extism.pc.in > libextism/extism.pc
	sed -e "s%@CMAKE_INSTALL_PREFIX@%$(DEST)%" \
	    -e "s%Libs: %Libs: $(EXTRA_LIBS) %" libextism/extism-static.pc.in > libextism/extism-static.pc

bench:
	@(cargo criterion $(TARGET_FLAGS) || echo 'For nicer output use cargo-criterion: `cargo install cargo-criterion` - using `cargo bench`') && cargo bench $(TARGET_FLAGS)

.PHONY: kernel
kernel:
	cd kernel && bash build.sh

lint:
	cargo clippy --release --no-deps --manifest-path runtime/Cargo.toml $(TARGET_FLAGS)

debug:
	RUSTFLAGS=-g RUST_TARGET=$(RUST_TARGET) $(MAKE) build

install:
	echo $(RUST_TARGET)
	mkdir -p $(DEST)/lib $(DEST)/include $(DEST)/lib/pkgconfig
	install runtime/extism.h $(DEST)/include/extism.h
	if [ -f target/$(RUST_TARGET)/release/libextism.$(SOEXT) ]; then \
	    install target/$(RUST_TARGET)/release/libextism.$(SOEXT) $(DEST)/lib/libextism.$(SOEXT); \
	fi
	install target/$(RUST_TARGET)/release/libextism.$(AEXT) $(DEST)/lib/libextism.$(AEXT)
	install libextism/extism.pc $(DEST)/lib/pkgconfig/extism.pc
	install libextism/extism-static.pc $(DEST)/lib/pkgconfig/extism-static.pc

uninstall:
	rm -f $(DEST)/include/extism.h $(DEST)/lib/libextism.$(SOEXT) $(DEST)/lib/libextism.$(AEXT) \
	$(DEST)/lib/pkgconfig/extism*.pc


