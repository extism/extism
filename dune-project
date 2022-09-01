(lang dune 3.2)

(name extism)

(generate_opam_files true)

(source
 (github extism/extism))

(authors "Extism Authors <oss@extism.org>")

(maintainers "Extism Authors <oss@extism.org>")

(license BSD-3)

(documentation https://github.com/extism/extism)

(package
 (name extism)
 (synopsis "Extism bindings")
 (description "Bindings to Extism, the universal plugin system")
 (depends ocaml dune ctypes-foreign bigstringaf ppx_yojson_conv base64)
 (tags
  (topics wasm plugin)))
