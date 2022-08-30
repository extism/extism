(lang dune 3.2)

(name extism)

(generate_opam_files true)

(source
 (github extism/extism))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name extism)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ctypes-foreign bigstringaf ppx_yojson_conv base64)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
