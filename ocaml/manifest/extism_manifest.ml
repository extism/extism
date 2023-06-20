open Ppx_yojson_conv_lib.Yojson_conv

type base64 = string

let yojson_of_base64 x = `String (Base64.encode_exn x)
let base64_of_yojson j = Yojson.Safe.Util.to_string j

type memory_options = { max_pages : int option [@yojson.option] }
[@@deriving yojson]

type dict = (string * string) list
type config = (string * string option) list

let is_null = function `Null -> true | _ -> false

let dict_of_yojson j =
  let assoc = Yojson.Safe.Util.to_assoc j in
  List.map (fun (k, v) -> (k, Yojson.Safe.Util.to_string v)) assoc

let yojson_of_dict c = `Assoc (List.map (fun (k, v) -> (k, `String v)) c)

let config_of_yojson j =
  let assoc = Yojson.Safe.Util.to_assoc j in
  List.map
    (fun (k, v) ->
      (k, if is_null v then None else Some (Yojson.Safe.Util.to_string v)))
    assoc

let yojson_of_config c =
  `Assoc
    (List.map
       (fun (k, v) -> (k, match v with None -> `Null | Some v -> `String v))
       c)

module Wasm = struct
  type file = {
    path : string;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type data = {
    data : base64;
    name : string option; [@yojson.option]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type url = {
    url : string;
    headers : dict option; [@yojson.option]
    name : string option; [@yojson.option]
    meth : string option; [@yojson.option] [@key "method"]
    hash : string option; [@yojson.option]
  }
  [@@deriving yojson]

  type t = File of file | Data of data | Url of url

  let yojson_of_t = function
    | File f -> yojson_of_file f
    | Data d -> yojson_of_data d
    | Url u -> yojson_of_url u

  let t_of_yojson x =
    try File (file_of_yojson x)
    with _ -> ( try Data (data_of_yojson x) with _ -> Url (url_of_yojson x))

  let file ?name ?hash path = File { path; name; hash }
  let data ?name ?hash data = Data { data; name; hash }

  let url ?headers ?name ?meth ?hash url =
    Url { headers; name; meth; hash; url }
end

type t = {
  wasm : Wasm.t list;
  memory : memory_options option; [@yojson.option]
  config : config option; [@yojson.option]
  allowed_hosts : string list option; [@yojson.option]
  allowed_paths : dict option; [@yojson.option]
  timeout_ms : int option; [@yojson.option]
}
[@@deriving yojson]

let create ?config ?memory ?allowed_hosts ?allowed_paths ?timeout_ms wasm =
  { config; wasm; memory; allowed_hosts; allowed_paths; timeout_ms }

let to_json t = yojson_of_t t |> Yojson.Safe.to_string

let of_json s =
  let j = Yojson.Safe.from_string s in
  t_of_yojson j

let of_file filename =
  let j = Yojson.Safe.from_file filename in
  t_of_yojson j

let with_config t config = { t with config = Some config }

let%test "rountrip" =
  let config = [ ("a", Some "b"); ("b", Some "c") ] in
  let memory = { max_pages = Some 5 } in
  let t =
    create ~config ~memory ~allowed_hosts:[ "example.com" ]
      ~allowed_paths:[ ("a", "b") ]
      ~timeout_ms:1000 []
  in
  let a = to_json t in
  let b = of_json a in
  let c = to_json b in
  String.equal a c
