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

build:
	cd core && cargo build --release $(FEATURE_FLAGS)

install:
	install core/extism.h $(DEST)/include
	install core/target/release/libextism.$(SOEXT) $(DEST)/lib
	
	ls $(DEST)/include | grep extism
	ls $(DEST)/lib | grep extism

uninstall:
	rm -f $(DEST)/include/extism.h $(DEST)/lib/libextism.$(SOEXT)


