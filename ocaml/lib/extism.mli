type t
type error = [`Msg of string]

exception Failed_to_load_plugin

module Manifest : sig
  type memory = { max : int option } [@@deriving yojson]
  type wasm_file = {
    path : string;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }

  type wasm_data = {
    data : string;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }

  type wasm_url = {
    url : string;
    header : (string * string) list option; [@yojson.option]
    name : string option; [@yojson.option]
    meth : string option; [@yojson.option] [@key "method"]
    hash : string option; [@yojson.option]
  }

  type wasm = File of wasm_file | Data of wasm_data | Url of wasm_url
  
  type config = (string * string) list

  type t = {
    wasm : wasm list;
    memory : memory option;
    config: config option;
  }

  val file: ?name:string -> ?hash:string -> string -> wasm
  val data: ?name:string -> ?hash:string -> string -> wasm
  val url: ?header:(string * string) list -> ?name:string -> ?meth:string -> ?hash:string -> string -> wasm
  val v: ?config:config -> ?memory:memory -> wasm list -> t
  val json: t -> string
end

val set_log_file: ?level:string -> string -> bool
val register: ?config:(string * string) list -> ?wasi:bool -> string -> t
val register_manifest: ?config:(string * string) list -> ?wasi:bool -> Manifest.t -> t
val update: t -> ?config:(string * string) list -> ?wasi:bool -> string -> bool
val update_manifest: t -> ?config:(string * string) list -> ?wasi:bool -> Manifest.t -> bool
val call_bigstring: t -> name:string -> Bigstringaf.t -> (Bigstringaf.t, error) result
val call: t -> name:string -> string -> (string, error) result
