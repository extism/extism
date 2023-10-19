(** Extism bindings for OCaml *)

val extism_version : unit -> string
(** Returns the libextism version, not the version of the OCaml library *)

module Manifest = Extism_manifest

module Error : sig
  type t = [ `Msg of string ]

  exception Error of t

  val unwrap : ('a, t) result -> 'a
  val throw : t -> 'a
end

(** [Val_type] enumerates every possible argument/result type *)
module Val_type : sig
  type t =
    | I32
    | I64
    | F32
    | F64
    | V128
    | FuncRef
    | ExternRef  (** Value type *)

  val of_int : int -> t
  val to_int : t -> int
end

(** [Val] represents low-level WebAssembly values *)
module Val : sig
  type t
  (** Val *)

  val ty : t -> Val_type.t
  (** [ty v] returns the [Val_type.t] for the value [v] *)

  val of_i32 : int32 -> t
  (** Create an i32 [Val] *)

  val of_i64 : int64 -> t
  (** Create an i64 [Val] *)

  val of_f32 : float -> t
  (** Create an f32 [Val] *)

  val of_f64 : float -> t
  (** Create an f64 [Val] *)

  val to_i32 : t -> int32 option
  (** Get an int32 from [Val] if the type matches *)

  val to_i64 : t -> int64 option
  (** Get an int64 from [Val] if the type matches *)

  val to_f32 : t -> float option
  (** Get a f32 from [Val] if the type matches *)

  val to_f64 : t -> float option
  (** Get an f64 from [Val] if the type matches *)

  val to_i32_exn : t -> int32
  (** Same as [to_i32] but raises an exception if the types don't match*)

  val to_i64_exn : t -> int64
  (** Same as [to_i64] but raises an exception if the types don't match*)

  val to_f32_exn : t -> float
  (** Same as [to_f32] but raises an exception if the types don't match*)

  val to_f64_exn : t -> float
  (** Same as [to_f64] but raises an exception if the types don't match*)
end

(** [Val_array] is used for input/output parameters for host functions *)
module Val_array : sig
  type t = Val.t Ctypes.CArray.t
  (** [Val_array] type *)

  val get : t -> int -> Val.t
  (** Get an index *)

  val set : t -> int -> Val.t -> unit
  (** Set an index *)

  val length : t -> int
  (** Get the number of items in a [Val_array]*)

  val ( .$[] ) : t -> int -> Val.t
  (** Syntax for [get] *)

  val ( .$[]<- ) : t -> int -> Val.t -> unit
  (** Syntax for [set] *)
end

(** [Current_plugin] represents the plugin that is currently running, it should
    it should only be used from a host function *)
module Current_plugin : sig
  type t
  (** Opaque type, wraps [ExtismCurrentPlugin] *)

  type memory_handle = { offs : Unsigned.UInt64.t; len : Unsigned.UInt64.t }
  (** Represents a block of guest memory *)

  val memory : ?offs:Unsigned.UInt64.t -> t -> Unsigned.uint8 Ctypes.ptr
  (** Get pointer to entire plugin memory *)

  val find : t -> Unsigned.UInt64.t -> memory_handle option
  (** Convert an offset into a {memory_handle} *)

  val alloc : t -> int -> memory_handle
  (** Allocate a new block of memory *)

  val free : t -> memory_handle -> unit
  (** Free allocated memory *)

  val return_string : t -> Val_array.t -> int -> string -> unit
  val return_bigstring : t -> Val_array.t -> int -> Bigstringaf.t -> unit
  val input_string : t -> Val_array.t -> int -> string
  val input_bigstring : t -> Val_array.t -> int -> Bigstringaf.t

  (** Some helpter functions for reading/writing memory *)
  module Memory_handle : sig
    val to_val : memory_handle -> Val.t
    (** Convert memory block to [Val] *)

    val of_val : t -> Val.t -> memory_handle option
    (** Convert [Val] to memory block *)

    val of_val_exn : t -> Val.t -> memory_handle
    (** Convert [Val] to memory block, raises [Invalid_argument] if the value is not a pointer
        to a valid memory block *)

    val get_string : t -> memory_handle -> string
    (** Get a string from memory stored at the provided offset *)

    val get_bigstring : t -> memory_handle -> Bigstringaf.t
    (** Get a bigstring from memory stored at the provided offset *)

    val set_string : t -> memory_handle -> string -> unit
    (** Store a string into memory at the provided offset *)

    val set_bigstring : t -> memory_handle -> Bigstringaf.t -> unit
    (** Store a bigstring into memory at the provided offset *)
  end
end

(** [Function] is used to create new a new function, which can be called 
    from a WebAssembly plugin *)
module Function : sig
  type t
  (** Function type *)

  val create :
    string ->
    ?namespace:string ->
    params:Val_type.t list ->
    results:Val_type.t list ->
    user_data:'a ->
    (Current_plugin.t -> Val_array.t -> Val_array.t -> 'a -> unit) ->
    t
  (** Create a new function, [Function.v name ~params ~results ~user_data f] creates
      a new [Function] with the given [name], [params] specifies the argument types,
      [results] specifies the return types, [user_data] is used to pass arbitrary
      OCaml values into the function and [f] is the OCaml function that will be
      called.
  *)

  val with_namespace : t -> string -> t
  (** Update a function's namespace *)

  val free : t -> unit
  (** Free a function *)

  val free_all : t list -> unit
  (** Free a list of functions *)
end

val set_log_file :
  ?level:[ `Error | `Warn | `Info | `Debug | `Trace ] -> string -> bool

(** [Plugins] contain functions that can be called *)
module Plugin : sig
  type t

  val create :
    ?config:Manifest.config ->
    ?wasi:bool ->
    ?functions:Function.t list ->
    string ->
    (t, Error.t) result
  (** Make a new plugin from raw WebAssembly or JSON encoded manifest *)

  val of_manifest :
    ?wasi:bool ->
    ?functions:Function.t list ->
    Manifest.t ->
    (t, Error.t) result
  (** Make a new plugin from a [Manifest] *)

  val call_bigstring :
    t -> name:string -> Bigstringaf.t -> (Bigstringaf.t, Error.t) result
  (** Call a function, uses [Bigstringaf.t] for input/output *)

  val call : t -> name:string -> string -> (string, Error.t) result
  (** Call a function, uses [string] for input/output *)

  val free : t -> unit
  (** Drop a plugin *)

  val function_exists : t -> string -> bool
  (** Check if a function is exported by a plugin *)

  module Cancel_handle : sig
    type t

    val cancel : t -> bool
  end

  val cancel_handle : t -> Cancel_handle.t

  val id : t -> Uuidm.t
end

val with_plugin : (Plugin.t -> 'a) -> Plugin.t -> 'a
