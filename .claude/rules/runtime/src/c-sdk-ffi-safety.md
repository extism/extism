---
paths:
  - 'runtime/src/sdk.rs'
  - 'runtime/extism.h'
---

# C SDK FFI Safety Principles

These patterns have all been learned from real bugs across the FFI boundary:

### 1. Never Panic Across FFI (commit 4db57de)
All fallible operations must use `Result` handling and return null/error codes. Panics crossing into C are UB. Use `.unwrap_or_else()` with error output params, not `.unwrap()`.

### 2. Null-Terminate All Error Strings (commit 6d2735c)
C consumers have no way to get error message length. The `make_error_msg()` helper appends `\0`. Always use it when storing error messages.

### 3. Empty Vec = Null Pointer (commit 34096bd)
`Vec::as_ptr()` returns a dangling pointer when the vec is empty. FFI consumers (especially JNA) will dereference it and SIGSEGV. Always check `is_empty()` and return `null` for empty vecs.

### 4. Float Bit Conversion (commit 7775c57)
When converting `ExtismVal` floats to wasmtime `Val`, use `f32::to_bits()` / `f64::to_bits()`, not `as` casts. Wasmtime `Val::F32`/`Val::F64` expect bit representations.

### 5. ExtismFunction is Single-Use
`ExtismFunction` wraps `Cell<Option<Function>>`. Calling `.take()` consumes it. A function pointer can only be registered with one plugin. Passing a consumed pointer to a second plugin returns an error.

### 6. CString Ownership for Error Output Params
Error output params (`char **errmsg`) use `CString::into_raw()` to transfer ownership to C. The caller must free with `extism_plugin_new_error_free()`. Always check `!errmsg.is_null()` before writing.
