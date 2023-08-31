module Manifest = Extism_manifest

type t = {
  mutable pointer : unit Ctypes.ptr;
  mutable functions : Function.t list;
}

let set_config plugin = function
  | None -> true
  | Some config ->
      let config =
        Extism_manifest.yojson_of_config config |> Yojson.Safe.to_string
      in
      Bindings.extism_plugin_config plugin.pointer config
        (Unsigned.UInt64.of_int (String.length config))

let free t =
  if not (Ctypes.is_null t.pointer) then
    let () = Bindings.extism_plugin_free t.pointer in
    t.pointer <- Ctypes.null

let strlen ptr =
  let rec aux ptr len =
    let c = Ctypes.( !@ ) ptr in
    if c = char_of_int 0 then len else aux (Ctypes.( +@ ) ptr 1) (len + 1)
  in
  aux ptr 0

let get_errmsg ptr =
  if Ctypes.is_null ptr then "Call failed"
  else
    let length = strlen ptr in
    let s = Ctypes.string_from_ptr ~length ptr in
    let () = Bindings.extism_plugin_new_error_free ptr in
    s

let create ?config ?(wasi = false) ?(functions = []) wasm =
  let func_ptrs = List.map (fun x -> x.Function.pointer) functions in
  let arr = Ctypes.CArray.of_list Ctypes.(ptr void) func_ptrs in
  let n_funcs = Ctypes.CArray.length arr in
  let errmsg =
    Ctypes.(allocate (ptr char) (coerce (ptr void) (ptr char) null))
  in
  let pointer =
    Bindings.extism_plugin_new wasm
      (Unsigned.UInt64.of_int (String.length wasm))
      (Ctypes.CArray.start arr)
      (Unsigned.UInt64.of_int n_funcs)
      wasi errmsg
  in
  if Ctypes.is_null pointer then
    let s = get_errmsg (Ctypes.( !@ ) errmsg) in
    Error (`Msg s)
  else
    let t = { pointer; functions } in
    if not (set_config t config) then Error (`Msg "call to set_config failed")
    else
      let () = Gc.finalise free t in
      Ok t

let of_manifest ?wasi ?functions manifest =
  let data = Manifest.to_json manifest in
  create ?wasi ?functions data

let%test "free plugin" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = of_manifest manifest |> Error.unwrap in
  free plugin;
  true

let call' f { pointer; _ } ~name input len =
  if Ctypes.is_null pointer then Error.throw (`Msg "Plugin already freed")
  else
    let rc = f pointer name input len in
    if rc <> 0l then
      match Bindings.extism_error pointer with
      | None -> Error (`Msg "extism_plugin_call failed")
      | Some msg -> Error (`Msg msg)
    else
      let out_len = Bindings.extism_plugin_output_length pointer in
      let ptr = Bindings.extism_plugin_output_data pointer in
      let buf =
        Ctypes.bigarray_of_ptr Ctypes.array1
          (Unsigned.UInt64.to_int out_len)
          Char ptr
      in
      Ok buf

let call_bigstring (t : t) ~name input =
  let len = Unsigned.UInt64.of_int (Bigstringaf.length input) in
  let ptr = Ctypes.bigarray_start Ctypes.array1 input in
  call' Bindings.extism_plugin_call t ~name ptr len

let%test "call_bigstring" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = of_manifest manifest |> Error.unwrap in
  call_bigstring plugin ~name:"count_vowels"
    (Bigstringaf.of_string ~off:0 ~len:14 "this is a test")
  |> Error.unwrap |> Bigstringaf.to_string = "{\"count\": 4}"

let call (t : t) ~name input =
  let len = String.length input in
  call' Bindings.extism_plugin_call_s t ~name input (Unsigned.UInt64.of_int len)
  |> Result.map Bigstringaf.to_string

let%test "call" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = of_manifest manifest |> Error.unwrap in
  call plugin ~name:"count_vowels" "this is a test"
  |> Error.unwrap = "{\"count\": 4}"

let%test "call_functions" =
  let open Types.Val_type in
  let hello_world =
    Function.create "hello_world" ~params:[ I64 ] ~results:[ I64 ]
      ~user_data:"Hello again!"
    @@ fun plugin params results user_data ->
    let open Types.Val_array in
    let s = Current_plugin.input_string plugin params 0 in
    let () = print_endline "Hello from OCaml!" in
    let () = print_endline user_data in
    let () = print_endline s in
    results.$[0] <- params.$[0]
  in
  let functions = [ hello_world ] in
  let manifest = Manifest.(create [ Wasm.file "test/code-functions.wasm" ]) in
  let plugin = of_manifest manifest ~functions ~wasi:true |> Error.unwrap in
  call plugin ~name:"count_vowels" "this is a test"
  |> Error.unwrap = "{\"count\": 4}"

let function_exists { pointer; _ } name =
  if Ctypes.is_null pointer then Error.throw (`Msg "Plugin already freed")
  else Bindings.extism_plugin_function_exists pointer name

let%test "function exists" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = of_manifest manifest |> Error.unwrap in
  function_exists plugin "count_vowels"
  && not (function_exists plugin "function_does_not_exist")

module Cancel_handle = struct
  type t = { inner : unit Ctypes.ptr }

  let cancel { inner } = Bindings.extism_plugin_cancel inner
end

let cancel_handle { pointer; _ } =
  if Ctypes.is_null pointer then Error.throw (`Msg "Plugin already freed")
  else Cancel_handle.{ inner = Bindings.extism_plugin_cancel_handle pointer }

let id { pointer; _ } =
  if Ctypes.is_null pointer then Error.throw (`Msg "Plugin already freed")
  else
    let id = Bindings.extism_plugin_id pointer in
    let s = Ctypes.string_from_ptr id ~length:16 in
    Uuidm.unsafe_of_bytes s
