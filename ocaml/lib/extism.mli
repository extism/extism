(** Extism bindings for OCaml *)

val extism_version : unit -> string
(** Returns the libextism version, not the version of the OCaml library *)

module Manifest = Extism_manifest

module Error : sig
  type t = [ `Msg of string ]

  exception Error of t

  val unwrap : ('a, t) result -> 'a
end

module Val_type : sig
  type t = I32 | I64 | F32 | F64 | FuncRef | ExternRef

  val t : t Ctypes.typ
  val of_int : int -> t
  val to_int : t -> int
end

module Val : sig
  type t

  val t : t Ctypes.typ
  val of_i32 : int32 -> t
  val of_i64 : int64 -> t
  val of_f32 : float -> t
  val of_f64 : float -> t
  val to_i32 : t -> int32 option
  val to_i64 : t -> int64 option
  val to_f32 : t -> float option
  val to_f64 : t -> float option
  val to_i32_exn : t -> int32
  val to_i64_exn : t -> int64
  val to_f32_exn : t -> float
  val to_f64_exn : t -> float
end

module Val_array : sig
  type t = Val.t Ctypes.CArray.t

  val get : t -> int -> Val.t
  val set : t -> int -> Val.t -> unit
  val length : t -> int
  val ( .$[] ) : t -> int -> Val.t
  val ( .$[]<- ) : t -> int -> Val.t -> unit
end

module Current_plugin : sig
  type t
  type offs = Unsigned.uint64
  type len = Unsigned.uint64

  val memory : t -> Unsigned.uint8 Ctypes.ptr
  val length : t -> offs -> len
  val alloc : t -> len -> offs
  val free : t -> offs -> unit
end

module Function : sig
  type t

  val v :
    string ->
    Val_type.t list ->
    Val_type.t list ->
    user_data:'a ->
    (Current_plugin.t -> Val_array.t -> Val_array.t -> 'a option -> unit) ->
    t
end

(** [Context] is used to group plugins *)
module Context : sig
  type t
  (** Context type *)

  val create : unit -> t
  (** Create a new context *)

  val free : t -> unit
  (** Free a context. All plugins will be removed and the value should not be 
      accessed after this call *)

  val reset : t -> unit
  (** Reset a context. All plugins will be removed *)
end

val with_context : (Context.t -> 'a) -> 'a
(** Execute a function with a fresh context and free it after *)

val set_log_file :
  ?level:[ `Error | `Warn | `Info | `Debug | `Trace ] -> string -> bool

(** [Plugins] contain functions that can be called *)
module Plugin : sig
  type t

  val make :
    ?config:Manifest.config ->
    ?wasi:bool ->
    ?functions:Function.t list ->
    Context.t ->
    string ->
    (t, Error.t) result
  (** Make a new plugin from raw WebAssembly or JSON encoded manifest *)

  val of_manifest :
    ?wasi:bool ->
    ?functions:Function.t list ->
    Context.t ->
    Manifest.t ->
    (t, Error.t) result
  (** Make a new plugin from a [Manifest] *)

  val update :
    t ->
    ?config:(string * string option) list ->
    ?wasi:bool ->
    ?functions:Function.t list ->
    string ->
    (unit, [ `Msg of string ]) result
  (** Update a plugin from raw WebAssembly or JSON encoded manifest *)

  val update_manifest : t -> ?wasi:bool -> Manifest.t -> (unit, Error.t) result
  (** Update a plugin from a [Manifest] *)

  val call_bigstring :
    t -> name:string -> Bigstringaf.t -> (Bigstringaf.t, Error.t) result
  (** Call a function, uses [Bigstringaf.t] for input/output *)

  val call : t -> name:string -> string -> (string, Error.t) result
  (** Call a function, uses [string] for input/output *)

  val free : t -> unit
  (** Drop a plugin *)

  val function_exists : t -> string -> bool
  (** Check if a function is exported by a plugin *)
end
