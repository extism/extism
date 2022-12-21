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
  (dune (>= 3.2))
  (ctypes-foreign (>= 0.18.0))
  (bigstringaf (>= 0.9.0))
  (ppx_yojson_conv (>= 0.15.0))
  extism-manifest
  (ppx_inline_test (>= 0.15.0))
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
  (dune (>= 3.2))
  (ppx_yojson_conv (>= 0.15.0))
  (base64 (>= 3.5.0))
 )
 (tags
  (topics wasm plugin)))
