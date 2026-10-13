---
paths:
  - 'runtime/src/pdk.rs'
  - 'runtime/src/current_plugin.rs'
---

# PDK Host Function Memory Ownership

### Rule: Host functions own all input memory handles

Every PDK function that receives a memory handle offset **must call `memory_free()` on it**. The host takes ownership; the plugin (caller) must not free it.

This was formalized in commit 3ac3d9a. All current PDK functions follow this pattern consistently:
- `config_get`, `var_get`: free key handle, allocate and return new handle for value
- `var_set`: frees both key and value handles
- `http_request`: frees request handle; frees body handle only if offset > 0
- `log_*` functions: all delegate to `log()` which frees the message handle

### Rule: Return values are new allocations

Functions that return data allocate new memory with `memory_new()` and pass ownership to the caller (plugin). The plugin is then responsible for freeing returned handles.

### ExternRef Single-Allocation Pattern

`Plugin` pre-allocates one `ExternRef` at creation (in `relink()`) and reuses it across calls by swapping the inner `Box<dyn Any>` value in `raw_call`. This avoids wasmtime's "failed to allocate externref" error (commit d2a3699). The ExternRef is reset to `Box::new(())` after each call.
