type t
type error = [ `Msg of string ]

val extism_version : unit -> string

module Manifest : sig
  type memory = { max_pages : int option } [@@deriving yojson]

  type dict = (string * string) list
  type config = (string * (string option)) list

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
    headers : dict option; [@yojson.option]
    name : string option; [@yojson.option]
    meth : string option; [@yojson.option] [@key "method"]
    hash : string option; [@yojson.option]
  }

  type wasm = File of wasm_file | Data of wasm_data | Url of wasm_url

  type t = {
    wasm : wasm list;
    memory : memory option;
    config : config option;
    allowed_hosts : string list option;
    allowed_paths : dict option;
    timeout_ms: int option;
  }

  val file : ?name:string -> ?hash:string -> string -> wasm
  val data : ?name:string -> ?hash:string -> string -> wasm

  val url :
    ?headers:(string * string) list ->
    ?name:string ->
    ?meth:string ->
    ?hash:string ->
    string ->
    wasm

  val v :
    ?config:config ->
    ?memory:memory ->
    ?allowed_hosts:string list ->
    ?allowed_paths:dict ->
    ?timeout_ms:int ->
    wasm list ->
    t

  val json : t -> string
  val with_config : t -> config -> t
end

module Context : sig
  type t

  val create : unit -> t
  val free : t -> unit
  val reset : t -> unit
end

val with_context : (Context.t -> 'a) -> 'a

val set_log_file :
  ?level:[ `Error | `Warn | `Info | `Debug | `Trace ] -> string -> bool

val plugin :
  ?config:Manifest.config ->
  ?wasi:bool ->
  Context.t ->
  string ->
  (t, [ `Msg of string ]) result

val of_manifest :
  ?wasi:bool ->
  Context.t ->
  Manifest.t ->
  (t, [ `Msg of string ]) result

val update :
  t ->
  ?config:(string * string option) list ->
  ?wasi:bool ->
  string ->
  (unit, [ `Msg of string ]) result

val update_manifest :
  t ->
  ?wasi:bool ->
  Manifest.t ->
  (unit, [ `Msg of string ]) result

val call_bigstring :
  t -> name:string -> Bigstringaf.t -> (Bigstringaf.t, error) result

val call : t -> name:string -> string -> (string, error) result
val free : t -> unit
val function_exists : t -> string -> bool
