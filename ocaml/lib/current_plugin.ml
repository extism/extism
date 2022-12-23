type t = unit Ctypes.ptr
type offs = Unsigned.uint64
type len = Unsigned.uint64

let memory t = Bindings.extism_current_plugin_memory t
let length t offs = Bindings.extism_current_plugin_memory_length t offs
let alloc t len = Bindings.extism_current_plugin_memory_alloc t len
let free t offs = Bindings.extism_current_plugin_memory_free t offs
