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

module Extism_val_type = struct
  type t = I32 | I64 | F32 | F64 | FuncRef | ExternRef

  let to_int = function
    | I32 -> 0
    | I64 -> 1
    | F32 -> 2
    | F64 -> 3
    | FuncRef -> 4
    | ExternRef -> 5

  let of_int = function
    | 0 -> I32
    | 1 -> I64
    | 2 -> F32
    | 3 -> F64
    | 4 -> FuncRef
    | 5 -> ExternRef
    | n -> invalid_arg ("Extism_val_type.of_int: " ^ string_of_int n)

  let t : t typ = view ~read:of_int ~write:to_int int
end

module Extism_val_union = struct
  type t

  let t : t union typ = union "ExtismValUnion"
  let i32 = field t "i32" int32_t
  let i64 = field t "i64" int64_t
  let f32 = field t "f32" float
  let f64 = field t "f64" double
  let () = seal t
end

module Extism_val = struct
  type t

  let t : t structure typ = structure "ExtismVal"
  let ty = field t "t" Extism_val_type.t
  let v = field t "v" Extism_val_union.t
  let () = seal t
end

let extism_plugin_new =
  fn "extism_plugin_new"
    (context @-> string @-> uint64_t @-> bool @-> returning int32_t)

let extism_plugin_new_with_functions =
  fn "extism_plugin_new_with_functions"
    (context @-> string @-> uint64_t
    @-> ptr (ptr void)
    @-> uint64_t @-> bool @-> returning int32_t)

let extism_plugin_update =
  fn "extism_plugin_update"
    (context @-> int32_t @-> string @-> uint64_t @-> bool @-> returning bool)

let extism_plugin_update_with_functions =
  fn "extism_plugin_update_with_functions"
    (context @-> int32_t @-> string @-> uint64_t
    @-> ptr (ptr void)
    @-> uint64_t @-> bool @-> returning bool)

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

let extism_function_type =
  Foreign.funptr ~runtime_lock:true
    (ptr void @-> ptr Extism_val.t @-> uint64_t @-> ptr Extism_val.t
   @-> uint64_t @-> ptr void @-> returning void)

let extism_free_user_data =
  Foreign.funptr_opt ~runtime_lock:true (ptr void @-> returning void)

let extism_function_new =
  fn "extism_function_new"
    (string @-> ptr Extism_val_type.t @-> uint64_t @-> ptr Extism_val_type.t
   @-> uint64_t @-> extism_function_type @-> ptr void @-> extism_free_user_data
    @-> returning (ptr void))

let extism_function_free =
  fn "extism_function_free" (ptr void @-> returning void)

let extism_current_plugin_memory =
  fn "extism_current_plugin_memory" (ptr void @-> returning (ptr uint8_t))

let extism_current_plugin_memory_length =
  fn "extism_current_plugin_memory_length"
    (ptr void @-> uint64_t @-> returning uint64_t)

let extism_current_plugin_memory_alloc =
  fn "extism_current_plugin_memory_alloc"
    (ptr void @-> uint64_t @-> returning uint64_t)

let extism_current_plugin_memory_free =
  fn "extism_current_plugin_memory_free"
    (ptr void @-> uint64_t @-> returning void)
