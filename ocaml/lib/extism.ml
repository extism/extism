module Manifest = Extism_manifest
module Error = Error
module Plugin = Plugin
module Function = Function
module Current_plugin = Current_plugin
include Types

let extism_version = Bindings.extism_version

let with_plugin f p =
  Fun.protect ~finally:(fun () -> Plugin.free p) (fun () -> f p)

let%test _ = String.length (extism_version ()) > 0

let set_log_file ?level filename =
  let level =
    Option.map
      (function
        | `Error -> "error"
        | `Warn -> "warn"
        | `Info -> "info"
        | `Debug -> "debug"
        | `Trace -> "trace")
      level
  in
  Bindings.extism_log_file filename level

let%test _ = set_log_file ~level:`Trace "stderr"
