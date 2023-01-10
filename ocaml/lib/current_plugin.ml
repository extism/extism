open Ctypes

type t = unit ptr
type offs = Unsigned.uint64
type len = Unsigned.uint64

let memory t = Bindings.extism_current_plugin_memory t
let length t offs = Bindings.extism_current_plugin_memory_length t offs
let alloc t len = Bindings.extism_current_plugin_memory_alloc t len
let free t offs = Bindings.extism_current_plugin_memory_free t offs

module Memory = struct
  let get_bigstring t offs : Bigstringaf.t =
    let length = length t offs in
    let p = memory t +@ Unsigned.UInt64.to_int offs in
    bigarray_of_ptr array1
      (Unsigned.UInt64.to_int length)
      Bigarray.Char
      (coerce (ptr uint8_t) (ptr char) p)

  let get_string t offs =
    let length = length t offs in
    let p = memory t +@ Unsigned.UInt64.to_int offs in
    Ctypes.string_from_ptr
      (coerce (ptr uint8_t) (ptr char) p)
      ~length:(Unsigned.UInt64.to_int length)

  let set_bigstring t offs bs =
    let length =
      min (Unsigned.UInt64.to_int @@ length t offs) (Bigstringaf.length bs)
    in
    let p =
      coerce (ptr uint8_t) (ptr char)
      @@ (memory t +@ Unsigned.UInt64.to_int offs)
    in
    for i = 0 to length - 1 do
      p +@ i <-@ Bigstringaf.unsafe_get bs i
    done

  let set_string t offs s =
    let length =
      min (Unsigned.UInt64.to_int @@ length t offs) (String.length s)
    in
    let p =
      coerce (ptr uint8_t) (ptr char)
      @@ (memory t +@ Unsigned.UInt64.to_int offs)
    in
    for i = 0 to length - 1 do
      p +@ i <-@ String.unsafe_get s i
    done
end
