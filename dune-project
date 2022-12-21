(lang dune 3.2)

(name extism)

(generate_opam_files true)

(source
 (github extism/extism))

(authors "Extism Authors <oss@extism.org>")

(maintainers "Extism Authors <oss@extism.org>")

(license BSD-3-Clause)

(documentation https://github.com/extism/extism)

(package
 (name extism)
 (synopsis "Extism bindings")
 (description "Bindings to Extism, the universal plugin system")
 (depends 
  (ocaml (>= 4.14.1)) 
  dune
  ctypes-foreign
  bigstringaf
  ppx_yojson_conv
  extism-manifest
  ppx_inline_test
  (cmdliner (>= 1.1.1))
 )
 (tags
  (topics wasm plugin)))

(package
 (name extism-manifest)
 (synopsis "Extism manifest bindings")
 (description "Bindings to Extism, the universal plugin system")
 (depends 
  (ocaml (>= 4.14.1)) 
  dune
  ppx_yojson_conv
  base64
 )
 (tags
  (topics wasm plugin)))
