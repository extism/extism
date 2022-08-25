open Extism

let () =
  let input =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "this is a test"
  in
  let manifest = Manifest.v [ Manifest.file "../wasm/code.wasm" ] in
  let plugin = Extism.register_manifest manifest in
  let res = Extism.call plugin ~name:"count_vowels" input |> Result.get_ok in
  print_endline res
