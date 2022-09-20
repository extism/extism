type t
type error = [`Msg of string]

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
    allowed_hosts: string list option;
  }

  val file: ?name:string -> ?hash:string -> string -> wasm
  val data: ?name:string -> ?hash:string -> string -> wasm
  val url: ?header:(string * string) list -> ?name:string -> ?meth:string -> ?hash:string -> string -> wasm
  val v:
    ?config:config ->
    ?memory:memory ->
    ?allowed_hosts: string list ->
    wasm list -> t
  val json: t -> string
end

module Context : sig
  type t

  val create: unit -> t
  val free: t -> unit
  val reset: t -> unit
end

val with_context : (Context.t -> 'a) -> 'a
val set_log_file: ?level:string -> string -> bool
val plugin: ?config:(string * string) list -> ?wasi:bool -> Context.t -> string -> (t, [`Msg of string]) result
val of_manifest: ?config:(string * string) list -> ?wasi:bool -> Context.t -> Manifest.t -> (t, [`Msg of string]) result
val update: t -> ?config:(string * string) list -> ?wasi:bool -> string -> (unit, [`Msg of string]) result
val update_manifest: t -> ?config:(string * string) list -> ?wasi:bool -> Manifest.t -> (unit, [`Msg of string]) result
val call_bigstring: t -> name:string -> Bigstringaf.t -> (Bigstringaf.t, error) result
val call: t -> name:string -> string -> (string, error) result
val free: t -> unit
val function_exists: t -> string -> bool
