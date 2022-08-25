SRC=count_vowels.c
PROJECT=code

rust:
	cd rust-pdk && $(MAKE)

c:
	emcc -o code.wasm count_vowels.c --no-entry -Wl,--export-all -sERROR_ON_UNDEFINED_SYMBOLS=0
