module Manifest = Extism_manifest

type t = { id : int32; ctx : Context.t }

let with_context f =
  let ctx = Context.create () in
  let x =
    try f ctx
    with exc ->
      Context.free ctx;
      raise exc
  in
  Context.free ctx;
  x

let set_config plugin = function
  | None -> true
  | Some config ->
      let config =
        Extism_manifest.yojson_of_config config |> Yojson.Safe.to_string
      in
      Bindings.extism_plugin_config plugin.ctx.pointer plugin.id config
        (Unsigned.UInt64.of_int (String.length config))

let free t =
  if not (Ctypes.is_null t.ctx.pointer) then
    Bindings.extism_plugin_free t.ctx.pointer t.id

let make ?config ?(wasi = false) ctx wasm =
  let id =
    Bindings.extism_plugin_new ctx.Context.pointer wasm
      (Unsigned.UInt64.of_int (String.length wasm))
      wasi
  in
  if id < 0l then
    match Bindings.extism_error ctx.pointer (-1l) with
    | None -> Error (`Msg "extism_plugin_call failed")
    | Some msg -> Error (`Msg msg)
  else
    let t = { id; ctx } in
    if not (set_config t config) then Error (`Msg "call to set_config failed")
    else
      let () = Gc.finalise free t in
      Ok t

let of_manifest ?wasi ctx manifest =
  let data = Manifest.json manifest in
  make ctx ?wasi data

let%test "free plugin" =
  let manifest = Manifest.v [ Manifest.file "test/code.wasm" ] in
  with_context (fun ctx ->
      let plugin = of_manifest ctx manifest |> Result.get_ok in
      free plugin;
      true)

let update plugin ?config ?(wasi = false) wasm =
  let { id; ctx } = plugin in
  let ok =
    Bindings.extism_plugin_update ctx.pointer id wasm
      (Unsigned.UInt64.of_int (String.length wasm))
      wasi
  in
  if not ok then
    match Bindings.extism_error ctx.pointer (-1l) with
    | None -> Error (`Msg "extism_plugin_update failed")
    | Some msg -> Error (`Msg msg)
  else if not (set_config plugin config) then
    Error (`Msg "call to set_config failed")
  else Ok ()

let update_manifest plugin ?wasi manifest =
  let data = Manifest.json manifest in
  update plugin ?wasi data

let%test "update plugin manifest and config" =
  let manifest = Manifest.v [ Manifest.file "test/code.wasm" ] in
  with_context (fun ctx ->
      let config = [ ("a", Some "1") ] in
      let plugin = of_manifest ctx manifest |> Result.get_ok in
      let manifest = Manifest.with_config manifest config in
      update_manifest plugin manifest |> Result.is_ok)

let call' f { id; ctx } ~name input len =
  let rc = f ctx.pointer id name input len in
  if rc <> 0l then
    match Bindings.extism_error ctx.pointer id with
    | None -> Error (`Msg "extism_plugin_call failed")
    | Some msg -> Error (`Msg msg)
  else
    let out_len = Bindings.extism_plugin_output_length ctx.pointer id in
    let ptr = Bindings.extism_plugin_output_data ctx.pointer id in
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
  let manifest = Manifest.v [ Manifest.file "test/code.wasm" ] in
  with_context (fun ctx ->
      let plugin = of_manifest ctx manifest |> Result.get_ok in
      call_bigstring plugin ~name:"count_vowels"
        (Bigstringaf.of_string ~off:0 ~len:14 "this is a test")
      |> Result.get_ok |> Bigstringaf.to_string = "{\"count\": 4}")

let call (t : t) ~name input =
  let len = String.length input in
  call' Bindings.extism_plugin_call_s t ~name input (Unsigned.UInt64.of_int len)
  |> Result.map Bigstringaf.to_string

let%test "call" =
  let manifest = Manifest.v [ Manifest.file "test/code.wasm" ] in
  with_context (fun ctx ->
      let plugin = of_manifest ctx manifest |> Result.get_ok in
      call plugin ~name:"count_vowels" "this is a test"
      |> Result.get_ok = "{\"count\": 4}")

let function_exists { id; ctx } name =
  Bindings.extism_plugin_function_exists ctx.pointer id name

let%test "function exists" =
  let manifest = Manifest.v [ Manifest.file "test/code.wasm" ] in
  with_context (fun ctx ->
      let plugin = of_manifest ctx manifest |> Result.get_ok in
      function_exists plugin "count_vowels"
      && not (function_exists plugin "function_does_not_exist"))
