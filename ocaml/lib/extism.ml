module Manifest = Extism_manifest
module Error = Error
module Context = Context
module Plugin = Plugin
module Function = Function
module Current_plugin = Current_plugin
include Types

let with_context = Plugin.with_context
let extism_version = Bindings.extism_version

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
