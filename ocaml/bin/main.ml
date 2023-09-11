open Extism
open Cmdliner

let read_stdin () = In_channel.input_all stdin

let split_allowed_paths =
  List.filter_map (fun path ->
      let s = String.split_on_char ':' path in
      match s with
      | [] -> None
      | [ p ] -> Some (p, p)
      | p :: tl -> Some (p, String.concat ":" tl))

let main file func_name input loop timeout_ms allowed_paths allowed_hosts
    manifest =
  let input = if String.equal input "-" then read_stdin () else input in
  let file = In_channel.with_open_bin file In_channel.input_all in
  let allowed_paths = split_allowed_paths allowed_paths in
  let manifest =
    if manifest then
      let m = Manifest.of_file file in
      {
        m with
        timeout_ms = Some timeout_ms;
        allowed_hosts = Some allowed_hosts;
        allowed_paths = Some allowed_paths;
      }
    else
      Manifest.create ~timeout_ms ~allowed_hosts ~allowed_paths
        [ Manifest.(Wasm.File { path = file; hash = None; name = None }) ]
  in
  let plugin = Plugin.of_manifest manifest ~wasi:true |> Result.get_ok in
  for _ = 0 to loop do
    let res = Plugin.call plugin ~name:func_name input |> Result.get_ok in
    print_endline res
  done

let file =
  let doc = "The WASM module or Extism manifest path." in
  Arg.(value & pos 0 file "" & info [] ~docv:"FILE" ~doc)

let func_name =
  let doc = "The function to run." in
  Arg.(value & pos 1 string "_start" & info [] ~docv:"NAME" ~doc)

let input =
  let doc = "Input data." in
  Arg.(value & opt string "" & info [ "input"; "i" ] ~docv:"INPUT" ~doc)

let loop =
  let doc = "Number of times to call the plugin." in
  Arg.(value & opt int 1 & info [ "loop" ] ~docv:"TIMES" ~doc)

let timeout =
  let doc = "Plugin timeout in milliseconds." in
  Arg.(value & opt int 30000 & info [ "timeout"; "t" ] ~docv:"MILLIS" ~doc)

let allowed_paths =
  let doc = "Allowed paths." in
  Arg.(value & opt_all string [] & info [ "allow-path" ] ~docv:"PATH" ~doc)

let allowed_hosts =
  let doc = "Allowed hosts for HTTP requests." in
  Arg.(value & opt_all string [] & info [ "allow-host" ] ~docv:"HOST" ~doc)

let manifest =
  let doc = "Use Manifest instead of raw Wasm on disk." in
  Arg.(value & flag & info [ "manifest" ] ~doc)

let main_t =
  Term.(
    const main $ file $ func_name $ input $ loop $ timeout $ allowed_paths
    $ allowed_hosts $ manifest)

let cmd = Cmd.v (Cmd.info "extism-run") main_t
let () = exit (Cmd.eval cmd)
