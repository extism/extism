(** Extism bindings for OCaml *)

(** The error type used throughout this library *)
type error = [ `Msg of string ]

(** Returns the libextism version, not the version of the OCaml library *)
val extism_version : unit -> string

(** [Manifest] is used to configure, fetch and link WebAssembly modules *)
module Manifest : sig

  (** Memory options *)
  type memory = { max_pages : int option } [@@deriving yojson]

  (** Key/value dictionary *)
  type dict = (string * string) list

  (** Key/value dictionary with optional values *)
  type config = (string * string option) list

  (** WebAssembly file *)
  type wasm_file = {
    path : string;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }

  (** WebAssembly module data *)
  type wasm_data = {
    data : string;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }

  (** WebAssembly URL *)
  type wasm_url = {
    url : string;
    headers : dict option; [@yojson.option]
    name : string option; [@yojson.option]
    meth : string option; [@yojson.option] [@key "method"]
    hash : string option; [@yojson.option]
  }

  (** WebAssembly from a file, module data or URL *)
  type wasm = File of wasm_file | Data of wasm_data | Url of wasm_url

  (** Manifest type *)
  type t = {
    wasm : wasm list;
    memory : memory option;
    config : config option;
    allowed_hosts : string list option;
    allowed_paths : dict option;
    timeout_ms : int option;
  }

  (** Create [wasm] from filename *)
  val file : ?name:string -> ?hash:string -> string -> wasm

  (** Create [wasm] from WebAssembly module data *)
  val data : ?name:string -> ?hash:string -> string -> wasm

  (** Create [wasm] from URL *)
  val url :
    ?headers:(string * string) list ->
    ?name:string ->
    ?meth:string ->
    ?hash:string ->
    string ->
    wasm

  (** Create new manifest *)
  val v :
    ?config:config ->
    ?memory:memory ->
    ?allowed_hosts:string list ->
    ?allowed_paths:dict ->
    ?timeout_ms:int ->
    wasm list ->
    t

  (** Convert manifest to JSON *)
  val json : t -> string

  (** Updates a manifest config *)
  val with_config : t -> config -> t
end

(** [Context] is used to group plugins *)
module Context : sig
  (** Context type *)
  type t

  (** Create a new context *)
  val create : unit -> t

  (** Free a context. All plugins will be removed and the value should not be 
      accessed after this call *)
  val free : t -> unit

  (** Reset a context. All plugins will be removed *)
  val reset : t -> unit
end

(** Execute a function with a fresh context and free it after *)
val with_context : (Context.t -> 'a) -> 'a

val set_log_file :
  ?level:[ `Error | `Warn | `Info | `Debug | `Trace ] -> string -> bool

(** [Plugins] contain functions that can be called *)
module Plugin : sig
  type t

  (** Make a new plugin from raw WebAssembly or JSON encoded manifest *)
  val make :
    ?config:Manifest.config ->
    ?wasi:bool ->
    Context.t ->
    string ->
    (t, [ `Msg of string ]) result

  (** Make a new plugin from a [Manifest] *)
  val of_manifest :
    ?wasi:bool -> Context.t -> Manifest.t -> (t, [ `Msg of string ]) result

  (** Update a plugin from raw WebAssembly or JSON encoded manifest *)
  val update :
    t ->
    ?config:(string * string option) list ->
    ?wasi:bool ->
    string ->
    (unit, [ `Msg of string ]) result

  (** Update a plugin from a [Manifest] *)
  val update_manifest :
    t -> ?wasi:bool -> Manifest.t -> (unit, [ `Msg of string ]) result

  (** Call a function, uses [Bigstringaf.t] for input/output *)
  val call_bigstring :
    t -> name:string -> Bigstringaf.t -> (Bigstringaf.t, error) result

  (** Call a function, uses [string] for input/output *)
  val call : t -> name:string -> string -> (string, error) result

  (** Drop a plugin *)
  val free : t -> unit

  (** Check if a function is exported by a plugin *)
  val function_exists : t -> string -> bool
end
