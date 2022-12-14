let ( // ) = Filename.concat

module Bindings = struct
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
          match check path with
          | None -> check (path // "lib")
          | Some _ as x -> x)
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

  let extism_context_reset =
    fn "extism_context_reset" (context @-> returning void)

  let extism_plugin_function_exists =
    fn "extism_plugin_function_exists"
      (context @-> int32_t @-> string @-> returning bool)
end

type error = [ `Msg of string ]

module Manifest = struct
  type memory = { max_pages : int option [@yojson.option] } [@@deriving yojson]

  type wasm_file = {
    path : string;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type base64 = string

  let yojson_of_base64 x = `String (Base64.encode_exn x)
  let base64_of_yojson j = Yojson.Safe.Util.to_string j

  type wasm_data = {
    data : base64;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type dict = (string * string) list
  type config = (string * string option) list

  let is_null = function `Null -> true | _ -> false

  let dict_of_yojson j =
    let assoc = Yojson.Safe.Util.to_assoc j in
    List.map (fun (k, v) -> (k, Yojson.Safe.Util.to_string v)) assoc

  let yojson_of_dict c = `Assoc (List.map (fun (k, v) -> (k, `String v)) c)

  let config_of_yojson j =
    let assoc = Yojson.Safe.Util.to_assoc j in
    List.map
      (fun (k, v) ->
        (k, if is_null v then None else Some (Yojson.Safe.Util.to_string v)))
      assoc

  let yojson_of_config c =
    `Assoc
      (List.map
         (fun (k, v) -> (k, match v with None -> `Null | Some v -> `String v))
         c)

  type wasm_url = {
    url : string;
    headers : dict option; [@yojson.option]
    name : string option; [@yojson.option]
    meth : string option; [@yojson.option] [@key "method"]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type wasm = File of wasm_file | Data of wasm_data | Url of wasm_url

  let yojson_of_wasm = function
    | File f -> yojson_of_wasm_file f
    | Data d -> yojson_of_wasm_data d
    | Url u -> yojson_of_wasm_url u

  let wasm_of_yojson x =
    try File (wasm_file_of_yojson x)
    with _ -> (
      try Data (wasm_data_of_yojson x) with _ -> Url (wasm_url_of_yojson x))

  type t = {
    wasm : wasm list;
    memory : memory option; [@yojson.option]
    config : config option; [@yojson.option]
    allowed_hosts : string list option; [@yojson.option]
    allowed_paths : dict option; [@yojson.option]
    timeout_ms : int option; [@yojson.option]
  }
  [@@deriving yojson]

  let file ?name ?hash path = File { path; name; hash }
  let data ?name ?hash data = Data { data; name; hash }

  let url ?headers ?name ?meth ?hash url =
    Url { headers; name; meth; hash; url }

  let v ?config ?memory ?allowed_hosts ?allowed_paths ?timeout_ms wasm =
    { config; wasm; memory; allowed_hosts; allowed_paths; timeout_ms }

  let json t = yojson_of_t t |> Yojson.Safe.to_string
  let with_config t config = { t with config = Some config }
end

module Context = struct
  type t = { mutable pointer : unit Ctypes.ptr }

  let create () =
    let ptr = Bindings.extism_context_new () in
    let t = { pointer = ptr } in
    Gc.finalise (fun { pointer } -> Bindings.extism_context_free pointer) t;
    t

  let free ctx =
    let () = Bindings.extism_context_free ctx.pointer in
    ctx.pointer <- Ctypes.null

  let reset ctx = Bindings.extism_context_reset ctx.pointer

  let%test "test context" =
    let ctx = create () in
    reset ctx;
    free ctx;
    true
end

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
      let config = Manifest.yojson_of_config config |> Yojson.Safe.to_string in
      Bindings.extism_plugin_config plugin.ctx.pointer plugin.id config
        (Unsigned.UInt64.of_int (String.length config))

let free t =
  if not (Ctypes.is_null t.ctx.pointer) then
    Bindings.extism_plugin_free t.ctx.pointer t.id

let plugin ?config ?(wasi = false) ctx wasm =
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
  plugin ctx ?wasi data

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

let extism_version = Bindings.extism_version

let%test _ = String.length (extism_version ()) > 0
