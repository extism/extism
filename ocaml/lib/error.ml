type t = [ `Msg of string ]

exception Error of t

let () =
  Printexc.register_printer (function
    | Error (`Msg msg) -> Some (Printf.sprintf "extism error: %s" msg)
    | _ -> None)

let unwrap = function Ok x -> x | Error t -> raise (Error t)
let throw e = raise (Error e)
