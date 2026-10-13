---
paths:
  - 'runtime/src/plugin.rs'
  - 'runtime/src/internal.rs'
  - 'runtime/Cargo.toml'
---

# WASI: Command vs Reactor Modules and wasi-common

### Command Modules (`_start`) Require Full Store Reset

Per the WASI spec, `_start` should be called "at most once." When a plugin calls `_start`, `store_needs_reset` is set to `true` (line ~890). On the next call, `reset_store()` recreates the entire `Store`, `Linker`, WASI context, and re-links all host functions. This is expensive but necessary because command modules may corrupt global state.

- **Reactor modules** (`_initialize` / `__wasm_call_ctors`): Initialize once, stay alive across multiple calls. No store reset needed.
- **Haskell modules**: Detected by `hs_init` export; may also have `_initialize` as reactor.

### Why `wasi-common` Instead of `wasmtime-wasi`

The project uses `wasi-common` (not `wasmtime-wasi`) to avoid tokio async runtime conflicts (see wasmtime issue #8799, commit 9dbc228). The runtime is fully synchronous. Both `wasi-common` and `wasmtime` version ranges must stay synchronized (currently 26.x).

### Error Handling for WASI Exits

WASI command modules exit via `wasi_common::I32Exit`. The `raw_call` function handles this specially: exit code 0 without an Extism error is treated as success; non-zero exit codes and exit code 0 with Extism errors are returned as errors.
