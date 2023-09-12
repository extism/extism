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

let split_config =
  List.filter_map (fun path ->
      let s = String.split_on_char '=' path in
      match s with
      | [] -> None
      | [ p ] -> Some (p, None)
      | p :: tl -> Some (p, Some (String.concat "=" tl)))

let main file func_name input loop timeout_ms allowed_paths allowed_hosts config
    memory_max log_level log_file wasi =
  let input = if String.equal input "-" then read_stdin () else input in
  let allowed_paths = split_allowed_paths allowed_paths in
  let config = split_config config in
  let memory = Manifest.{ max_pages = memory_max } in
  let manifest =
    try
      let m = Manifest.of_file file in
      {
        m with
        timeout_ms = Some timeout_ms;
        allowed_hosts = Some allowed_hosts;
        allowed_paths = Some allowed_paths;
        config = Some config;
        memory = Some memory;
      }
    with _ ->
      Manifest.create ~timeout_ms ~allowed_hosts ~allowed_paths ~config ~memory
        [ Manifest.(Wasm.File { path = file; hash = None; name = None }) ]
  in
  let () =
    match (log_level, log_file) with
    | None, _ -> ()
    | Some level, Some file -> assert (set_log_file ~level file)
    | Some level, None -> assert (set_log_file ~level "stderr")
  in
  let plugin =
    match Plugin.of_manifest manifest ~wasi with
    | Ok x -> x
    | Error (`Msg e) ->
        Printf.eprintf "ERROR Unable to load plugin: %s" e;
        exit 1
  in
  for _ = 0 to loop do
    match Plugin.call plugin ~name:func_name input with
    | Ok res -> print_endline res
    | Error (`Msg e) ->
        Printf.eprintf "ERROR Unable to call function: %s" e;
        exit 2
  done

let file =
  let doc = "The Wasm module or Extism manifest path." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let func_name =
  let doc = "The function to run." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"NAME" ~doc)

let input =
  let doc = "Input data." in
  Arg.(value & opt string "" & info [ "input"; "i" ] ~docv:"INPUT" ~doc)

let loop =
  let doc = "Number of times to call the plugin." in
  Arg.(value & opt int 0 & info [ "loop" ] ~docv:"TIMES" ~doc)

let memory_max =
  let doc = "Max number of memory pages." in
  Arg.(value & opt (some int) None & info [ "memory-max" ] ~docv:"PAGES" ~doc)

let timeout =
  let doc = "Plugin timeout in milliseconds." in
  Arg.(value & opt int 30000 & info [ "timeout"; "t" ] ~docv:"MILLIS" ~doc)

let allowed_paths =
  let doc = "Allowed paths." in
  Arg.(
    value & opt_all string []
    & info [ "allow-path" ] ~docv:"LOCAL_PATH[:PLUGIN_PATH]" ~doc)

let allowed_hosts =
  let doc = "Allowed hosts for HTTP requests." in
  Arg.(value & opt_all string [] & info [ "allow-host" ] ~docv:"HOST" ~doc)

let config =
  let doc = "Plugin config." in
  Arg.(value & opt_all string [] & info [ "config" ] ~docv:"KEY=VALUE" ~doc)

let log_file =
  let doc = "File to write logs to." in
  Arg.(
    value & opt (some string) None & info [ "log-file" ] ~docv:"FILENAME" ~doc)

let log_level_enum =
  Arg.enum
    [
      ("warn", `Warn);
      ("info", `Info);
      ("debug", `Debug);
      ("error", `Error);
      ("trace", `Trace);
    ]

let log_level =
  let doc = "Log level." in
  Arg.(
    value
    & opt (some log_level_enum) None
    & info [ "log-level" ] ~docv:"LEVEL" ~doc)

let wasi =
  let doc = "Enable WASI." in
  Arg.(value & flag & info [ "wasi" ] ~doc)

let main_t =
  Term.(
    const main $ file $ func_name $ input $ loop $ timeout $ allowed_paths
    $ allowed_hosts $ config $ memory_max $ log_level $ log_file $ wasi)

let cmd = Cmd.v (Cmd.info "extism-run") main_t
let () = exit (Cmd.eval cmd)
