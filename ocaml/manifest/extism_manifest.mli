type memory_options = { max_pages : int option } [@@deriving yojson]
(** Memory options *)

type dict = (string * string) list [@@deriving yojson]
(** Key/value dictionary *)

type config = (string * string option) list [@@deriving yojson]
(** Key/value dictionary with optional values *)

module Wasm : sig
  type file = {
    path : string;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]
  (** WebAssembly file *)

  type data = {
    data : string;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]
  (** WebAssembly module data *)

  type url = {
    url : string;
    headers : dict option; [@yojson.option]
    name : string option; [@yojson.option]
    meth : string option; [@yojson.option] [@key "method"]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]
  (** WebAssembly URL *)

  (** WebAssembly from a file, module data or URL *)
  type t = File of file | Data of data | Url of url [@@deriving yojson]

  val file : ?name:string -> ?hash:string -> string -> t
  (** Create [wasm] from filename *)

  val data : ?name:string -> ?hash:string -> string -> t
  (** Create [wasm] from WebAssembly module data *)

  val url :
    ?headers:(string * string) list ->
    ?name:string ->
    ?meth:string ->
    ?hash:string ->
    string ->
    t
  (** Create [wasm] from URL *)
end

type t = {
  wasm : Wasm.t list;
  memory : memory_options option;
  config : config option;
  allowed_hosts : string list option;
  allowed_paths : dict option;
  timeout_ms : int option;
}
[@@deriving yojson]
(** Manifest type *)

val create :
  ?config:config ->
  ?memory:memory_options ->
  ?allowed_hosts:string list ->
  ?allowed_paths:dict ->
  ?timeout_ms:int ->
  Wasm.t list ->
  t
(** Create new manifest *)

val to_json : t -> string
(** Convert manifest to JSON *)

val of_json : string -> t
(** Read manifest from JSON string *)

val of_file : string -> t
(** Read manifest from JSON file *)

val with_config : t -> config -> t
(** Updates a manifest config *)
