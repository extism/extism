type memory = { max_pages : int option [@yojson.option] } [@@deriving yojson]

type wasm_file = {
  path : string;
  name : string option; [@yojson.option]
  hash : string option; [@yojson.option]
}
[@@deriving yojson]

type base64 = string

let yojson_of_base64 x = `String (Base64.encode_exn x)
let base64_of_yojson j = Yojson.Safe.Util.to_string j

type wasm_data = {
  data : base64;
  name : string option; [@yojson.option]
  hash : string option; [@yojson.option]
}
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

type wasm_url = {
  url : string;
  headers : dict option; [@yojson.option]
  name : string option; [@yojson.option]
  meth : string option; [@yojson.option] [@key "method"]
  hash : string option; [@yojson.option]
}
[@@deriving yojson]

type wasm = File of wasm_file | Data of wasm_data | Url of wasm_url

let yojson_of_wasm = function
  | File f -> yojson_of_wasm_file f
  | Data d -> yojson_of_wasm_data d
  | Url u -> yojson_of_wasm_url u

let wasm_of_yojson x =
  try File (wasm_file_of_yojson x)
  with _ -> (
    try Data (wasm_data_of_yojson x) with _ -> Url (wasm_url_of_yojson x))

type t = {
  wasm : wasm list;
  memory : memory option; [@yojson.option]
  config : config option; [@yojson.option]
  allowed_hosts : string list option; [@yojson.option]
  allowed_paths : dict option; [@yojson.option]
  timeout_ms : int option; [@yojson.option]
}
[@@deriving yojson]

let file ?name ?hash path = File { path; name; hash }
let data ?name ?hash data = Data { data; name; hash }
let url ?headers ?name ?meth ?hash url = Url { headers; name; meth; hash; url }

let v ?config ?memory ?allowed_hosts ?allowed_paths ?timeout_ms wasm =
  { config; wasm; memory; allowed_hosts; allowed_paths; timeout_ms }

let json t = yojson_of_t t |> Yojson.Safe.to_string
let with_config t config = { t with config = Some config }
