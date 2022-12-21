(** Extism bindings for OCaml *)


(** Returns the libextism version, not the version of the OCaml library *)
val extism_version : unit -> string

module Manifest = Extism_manifest

module Error : sig
  type t = [`Msg of string]

  exception Error of t
  val unwrap: ('a, t) result -> 'a
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
    (t, Error.t) result

  (** Make a new plugin from a [Manifest] *)
  val of_manifest :
    ?wasi:bool -> Context.t -> Manifest.t -> (t, Error.t) result

  (** Update a plugin from raw WebAssembly or JSON encoded manifest *)
  val update :
    t ->
    ?config:(string * string option) list ->
    ?wasi:bool ->
    string ->
    (unit, [ `Msg of string ]) result

  (** Update a plugin from a [Manifest] *)
  val update_manifest :
    t -> ?wasi:bool -> Manifest.t -> (unit, Error.t) result

  (** Call a function, uses [Bigstringaf.t] for input/output *)
  val call_bigstring :
    t -> name:string -> Bigstringaf.t -> (Bigstringaf.t, Error.t) result

  (** Call a function, uses [string] for input/output *)
  val call : t -> name:string -> string -> (string, Error.t) result

  (** Drop a plugin *)
  val free : t -> unit

  (** Check if a function is exported by a plugin *)
  val function_exists : t -> string -> bool
end
