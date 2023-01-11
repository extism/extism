prepare: import_headers.c
	ifeq ($(OS),Windows_NT)
		gcc -c import_headers.c
	else
		cl -TC -c import_headers.c