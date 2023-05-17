open Extism
open Cmdliner

let read_stdin () = In_channel.input_all stdin

let main file func_name input =
  let input = if String.equal input "-" then read_stdin () else input in
  let file = In_channel.with_open_bin file In_channel.input_all in
  let plugin = Plugin.create file ~wasi:true |> Result.get_ok in
  let res = Plugin.call plugin ~name:func_name input |> Result.get_ok in
  print_endline res

let file =
  let doc = "The WASM module or Extism manifest path." in
  Arg.(value & pos 0 file "" & info [] ~docv:"FILE" ~doc)

let func_name =
  let doc = "The function to run." in
  Arg.(value & pos 1 string "_start" & info [] ~docv:"NAME" ~doc)

let input =
  let doc = "Input data." in
  Arg.(value & opt string "" & info [ "input"; "i" ] ~docv:"INPUT" ~doc)

let main_t = Term.(const main $ file $ func_name $ input)
let cmd = Cmd.v (Cmd.info "extism-run") main_t
let () = exit (Cmd.eval cmd)
