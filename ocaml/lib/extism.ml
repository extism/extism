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

  let extism_plugin_register =
    fn "extism_plugin_register"
      (string @-> uint64_t @-> bool @-> returning int32_t)

  let extism_plugin_update =
    fn "extism_plugin_update"
      (int32_t @-> string @-> uint64_t @-> bool @-> returning bool)

  let extism_plugin_config =
    fn "extism_plugin_config"
      (int32_t @-> string @-> uint64_t @-> returning bool)

  let extism_call =
    fn "extism_call"
      (int32_t @-> string @-> ptr char @-> uint64_t @-> returning int32_t)

  let extism_call_s =
    fn "extism_call"
      (int32_t @-> string @-> string @-> uint64_t @-> returning int32_t)

  let extism_error = fn "extism_error" (int32_t @-> returning string_opt)

  let extism_output_length =
    fn "extism_output_length" (int32_t @-> returning uint64_t)

  let extism_output_get =
    fn "extism_output_get" (int32_t @-> returning (ptr char))

  let extism_log_file =
    fn "extism_log_file" (string @-> string_opt @-> returning bool)
end

type error = [ `Msg of string ]
type t = { id : int32 }

module Manifest = struct
  type memory = { max : int option [@yojson.option] } [@@deriving yojson]

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

  type wasm_url = {
    url : string;
    header : (string * string) list option; [@yojson.option]
    name : string option; [@yojson.option]
    meth : string option; [@yojson.option] [@key "method"]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type config = (string * string) list
  type wasm = File of wasm_file | Data of wasm_data | Url of wasm_url

  let yojson_of_wasm = function
    | File f -> yojson_of_wasm_file f
    | Data d -> yojson_of_wasm_data d
    | Url u -> yojson_of_wasm_url u

  let wasm_of_yojson x =
    try File (wasm_file_of_yojson x)
    with _ -> (
      try Data (wasm_data_of_yojson x) with _ -> Url (wasm_url_of_yojson x))

  let config_of_yojson j =
    let assoc = Yojson.Safe.Util.to_assoc j in
    List.map (fun (k, v) -> (k, Yojson.Safe.Util.to_string v)) assoc

  let yojson_of_config c = `Assoc (List.map (fun (k, v) -> (k, `String v)) c)

  type t = {
    wasm : wasm list;
    memory : memory option; [@yojson.option]
    config : config option; [@yojson.option]
  }
  [@@deriving yojson]

  let file ?name ?hash path = File { path; name; hash }
  let data ?name ?hash data = Data { data; name; hash }
  let url ?header ?name ?meth ?hash url = Url { header; name; meth; hash; url }
  let v ?config ?memory wasm = { config; wasm; memory }
  let json t = yojson_of_t t |> Yojson.Safe.to_string
end

exception Failed_to_load_plugin

let set_log_file ?level filename = Bindings.extism_log_file filename level

let set_config plugin config =
  match config with
  | Some config ->
      let config = Manifest.yojson_of_config config |> Yojson.Safe.to_string in
      let _ =
        Bindings.extism_plugin_config plugin config
          (Unsigned.UInt64.of_int (String.length config))
      in
      ()
  | None -> ()

let register ?config ?(wasi = false) wasm =
  let id =
    Bindings.extism_plugin_register wasm
      (Unsigned.UInt64.of_int (String.length wasm))
      wasi
  in
  if id < 0l then raise Failed_to_load_plugin;
  set_config id config;
  { id }

let register_manifest ?config ?wasi manifest =
  let data = Manifest.json manifest in
  register ?config ?wasi data

let update { id } ?config ?(wasi = false) wasm =
  let ok =
    Bindings.extism_plugin_update id wasm
      (Unsigned.UInt64.of_int (String.length wasm))
      wasi
  in
  if ok then
    let () = set_config id config in
    true
  else false

let update_manifest plugin ?config ?wasi manifest =
  let data = Manifest.json manifest in
  update plugin ?config ?wasi data

let call' f { id } ~name input len =
  let rc = f id name input len in
  if rc <> 0l then
    match Bindings.extism_error id with
    | None -> Error (`Msg "extism_call failed")
    | Some msg -> Error (`Msg msg)
  else
    let out_len = Bindings.extism_output_length id in
    let ptr = Bindings.extism_output_get id in
    let buf =
      Ctypes.bigarray_of_ptr Ctypes.array1
        (Unsigned.UInt64.to_int out_len)
        Char ptr
    in
    Ok buf

let call_bigstring t ~name input =
  let len = Unsigned.UInt64.of_int (Bigstringaf.length input) in
  let ptr = Ctypes.bigarray_start Ctypes.array1 input in
  call' Bindings.extism_call t ~name ptr len

let call t ~name input =
  let len = String.length input in
  call' Bindings.extism_call_s t ~name input (Unsigned.UInt64.of_int len)
  |> Result.map Bigstringaf.to_string
