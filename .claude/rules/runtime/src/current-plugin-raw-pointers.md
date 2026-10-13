---
paths:
  - 'runtime/src/current_plugin.rs'
  - 'runtime/src/plugin.rs'
---

# CurrentPlugin Raw Pointer Invariants

`CurrentPlugin` holds raw `*mut` pointers to `Store` and `Linker` because wasmtime's `Store<CurrentPlugin>` owns `CurrentPlugin` as its data, creating a circular dependency. Host functions receive `Caller<CurrentPlugin>` (giving `&mut CurrentPlugin`) but need access to the `Store`/`Linker` for memory operations. Raw pointers are the only way to provide this back-reference.

### Critical Invariant: Pointer Re-synchronization

These raw pointers **must be re-set** whenever `Store` or `Linker` are recreated:

- After `Plugin::new_from_compiled` (lines ~431-432)
- After `reset_store` (lines ~477-481)
- Before `set_input` (lines ~567-571, defensive re-sync)

Forgetting to re-sync after store recreation produces dangling pointer bugs. Commit ecf18a2 fixed exactly this in `reset_store`.

### Safety Guarantees

Pointers are safe because they're only dereferenced during synchronous host function calls where the `Plugin` struct is on the stack and cannot move. The `Store`/`Linker` fields don't relocate within a `Plugin`'s lifetime.
