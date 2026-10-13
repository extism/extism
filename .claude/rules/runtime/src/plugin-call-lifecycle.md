---
paths:
  - 'runtime/src/plugin.rs'
  - 'runtime/src/lib.rs'
---

# Plugin Call Lifecycle and Error Handling

### Call Flow in `raw_call`

1. Reset fuel if configured
2. `reset_store` if flagged (WASI command module ran `_start` last time)
3. `instantiate` lazily (only if instance is None, detects guest runtime)
4. Swap host context into pre-allocated ExternRef
5. `set_input` (re-syncs raw pointers, resets kernel memory, writes input)
6. Start timer with epoch interruption
7. Call the function
8. Reset host context to `()`
9. Stop timer, set `store_needs_reset` if function was `_start`
10. Handle errors (fuel, extism error, wasmtime trap, WASI exit, OOM, timeout)

### Fuel Detection Gotcha (commit c2866a7)

The `catch_out_of_fuel!` macro checks `store.get_fuel().is_ok_and(|x| x == 0)` after an error. You can't rely on the error type alone to detect out-of-fuel; you must check the remaining fuel value. This macro is used throughout `plugin.rs` and `lib.rs`.

### Instantiation Tracking

The `instantiations` counter tracks how many times the module has been instantiated. Wasmtime `Instance`s are only cleaned up when their `Store` is dropped, so repeated instantiations without store resets can cause memory buildup. The counter is reset to 0 when `reset_store()` runs.
