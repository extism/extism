open Ctypes

type t = unit ptr
type memory_handle = { offs : Unsigned.UInt64.t; len : Unsigned.UInt64.t }

let memory ?(offs = Unsigned.UInt64.zero) t =
  Bindings.extism_current_plugin_memory t +@ Unsigned.UInt64.to_int offs

let find t offs =
  let len = Bindings.extism_current_plugin_memory_length t offs in
  if Unsigned.UInt64.(equal zero len) then None else Some { offs; len }

let alloc t len =
  let len = Unsigned.UInt64.of_int len in
  let offs = Bindings.extism_current_plugin_memory_alloc t len in
  { offs; len }

let free t { offs; _ } = Bindings.extism_current_plugin_memory_free t offs

module Memory_handle = struct
  let of_val t v =
    match Types.Val.to_i64 v with
    | None -> None
    | Some v ->
        let offs = Unsigned.UInt64.of_int64 v in
        find t offs

  let of_val_exn t v =
    match of_val t v with
    | None -> invalid_arg "Memory_block.of_val_exn"
    | Some v -> v

  let to_val { offs; len = _ } =
    Types.Val.of_i64 (Unsigned.UInt64.to_int64 offs)

  let get_bigstring t { offs; len } : Bigstringaf.t =
    let p = memory t ~offs in
    bigarray_of_ptr array1
      (Unsigned.UInt64.to_int len)
      Bigarray.Char
      (coerce (ptr uint8_t) (ptr char) p)

  let get_string t { offs; len } =
    let p = memory t ~offs in
    Ctypes.string_from_ptr
      (coerce (ptr uint8_t) (ptr char) p)
      ~length:(Unsigned.UInt64.to_int len)

  let set_bigstring t { offs; len } bs =
    let length = min (Unsigned.UInt64.to_int @@ len) (Bigstringaf.length bs) in
    let p = coerce (ptr uint8_t) (ptr char) @@ memory t ~offs in
    for i = 0 to length - 1 do
      p +@ i <-@ Bigstringaf.unsafe_get bs i
    done

  let set_string t { offs; len } s =
    let length = min (Unsigned.UInt64.to_int @@ len) (String.length s) in
    let p = coerce (ptr uint8_t) (ptr char) @@ memory t ~offs in
    for i = 0 to length - 1 do
      p +@ i <-@ String.unsafe_get s i
    done
end

let return_string t (outputs : Types.Val_array.t) index s =
  let mem = alloc t (String.length s) in
  Memory_handle.set_string t mem s;
  Types.Val_array.(
    outputs.$[index] <- Types.Val.of_i64 (Unsigned.UInt64.to_int64 mem.offs))

let return_bigstring t (outputs : Types.Val_array.t) index s =
  let mem = alloc t (Bigstringaf.length s) in
  Memory_handle.set_bigstring t mem s;
  Types.Val_array.(
    outputs.$[index] <- Types.Val.of_i64 (Unsigned.UInt64.to_int64 mem.offs))

let input_string t inputs index =
  let inp = Types.Val_array.(inputs.$[index]) in
  let mem = Memory_handle.of_val_exn t inp in
  Memory_handle.get_string t mem

let input_bigstring t inputs index =
  let inp = Types.Val_array.(inputs.$[index]) in
  let mem = Memory_handle.of_val_exn t inp in
  Memory_handle.get_bigstring t mem
