type memory = { max_pages : int option } [@@deriving yojson]
(** Memory options *)

type dict = (string * string) list [@@deriving yojson]
(** Key/value dictionary *)

type config = (string * string option) list [@@deriving yojson]
(** Key/value dictionary with optional values *)

type wasm_file = {
  path : string;
  name : string option; [@yojson.option]
  hash : string option; [@yojson.option]
}
[@@deriving yojson]
(** WebAssembly file *)

type wasm_data = {
  data : string;
  name : string option; [@yojson.option]
  hash : string option; [@yojson.option]
}
[@@deriving yojson]
(** WebAssembly module data *)

type wasm_url = {
  url : string;
  headers : dict option; [@yojson.option]
  name : string option; [@yojson.option]
  meth : string option; [@yojson.option] [@key "method"]
  hash : string option; [@yojson.option]
}
[@@deriving yojson]
(** WebAssembly URL *)

(** WebAssembly from a file, module data or URL *)
type wasm = File of wasm_file | Data of wasm_data | Url of wasm_url
[@@deriving yojson]

type t = {
  wasm : wasm list;
  memory : memory option;
  config : config option;
  allowed_hosts : string list option;
  allowed_paths : dict option;
  timeout_ms : int option;
}
[@@deriving yojson]
(** Manifest type *)

val file : ?name:string -> ?hash:string -> string -> wasm
(** Create [wasm] from filename *)

val data : ?name:string -> ?hash:string -> string -> wasm
(** Create [wasm] from WebAssembly module data *)

val url :
  ?headers:(string * string) list ->
  ?name:string ->
  ?meth:string ->
  ?hash:string ->
  string ->
  wasm
(** Create [wasm] from URL *)

val v :
  ?config:config ->
  ?memory:memory ->
  ?allowed_hosts:string list ->
  ?allowed_paths:dict ->
  ?timeout_ms:int ->
  wasm list ->
  t
(** Create new manifest *)

val json : t -> string
(** Convert manifest to JSON *)

val with_config : t -> config -> t
(** Updates a manifest config *)
