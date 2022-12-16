let ( // ) = Filename.concat

let paths =
  [
    "/usr/lib";
    "/usr/local/lib";
    Sys.getenv "HOME" // ".local/lib";
    Sys.getcwd ();
  ]

let check x =
  let a = x // "libextism.so" in
  let b = x // "libextism.dylib" in
  if Sys.file_exists a then Some a
  else if Sys.file_exists b then Some b
  else None

let locate () =
  let init =
    match Sys.getenv_opt "EXTISM_PATH" with
    | Some path -> (
        match check path with None -> check (path // "lib") | Some _ as x -> x)
    | None -> None
  in
  List.fold_left
    (fun acc path -> match acc with Some _ -> acc | None -> check path)
    init paths
  |> function
  | Some x -> x
  | None -> raise Not_found

let from =
  let filename = locate () in
  Dl.dlopen ~filename ~flags:[ Dl.RTLD_GLOBAL; Dl.RTLD_NOW ]

open Ctypes

let fn = Foreign.foreign ~from ~release_runtime_lock:true
let context = ptr void
let extism_context_new = fn "extism_context_new" (void @-> returning context)
let extism_context_free = fn "extism_context_free" (context @-> returning void)

let extism_plugin_new =
  fn "extism_plugin_new"
    (context @-> string @-> uint64_t @-> bool @-> returning int32_t)

let extism_plugin_update =
  fn "extism_plugin_update"
    (context @-> int32_t @-> string @-> uint64_t @-> bool @-> returning bool)

let extism_plugin_config =
  fn "extism_plugin_config"
    (context @-> int32_t @-> string @-> uint64_t @-> returning bool)

let extism_plugin_call =
  fn "extism_plugin_call"
    (context @-> int32_t @-> string @-> ptr char @-> uint64_t
   @-> returning int32_t)

let extism_plugin_call_s =
  fn "extism_plugin_call"
    (context @-> int32_t @-> string @-> string @-> uint64_t
   @-> returning int32_t)

let extism_error =
  fn "extism_error" (context @-> int32_t @-> returning string_opt)

let extism_plugin_output_length =
  fn "extism_plugin_output_length" (context @-> int32_t @-> returning uint64_t)

let extism_plugin_output_data =
  fn "extism_plugin_output_data" (context @-> int32_t @-> returning (ptr char))

let extism_log_file =
  fn "extism_log_file" (string @-> string_opt @-> returning bool)

let extism_version = fn "extism_version" (void @-> returning string)

let extism_plugin_free =
  fn "extism_plugin_free" (context @-> int32_t @-> returning void)

let extism_context_reset = fn "extism_context_reset" (context @-> returning void)

let extism_plugin_function_exists =
  fn "extism_plugin_function_exists"
    (context @-> int32_t @-> string @-> returning bool)
