# extism-store (Extism 2 extension)

Host-owned SQLite for Extism plugins. This is **not** the kernel and **not**
`extism:host/env`. Plugins import `extism:host/store`; the host seeds and reads
guest paths with `DurableStore::fs_put` / `DurableStore::fs_get` instead of
`allowed_paths`.

```rust
let store = DurableStore::memory("user-123")?;
store.fs_put("/hello", b"from-host")?;
let mut plugin = PluginBuilder::new(wasm)
    .with_store(store.clone())
    .build()?;
plugin.call("run", "/hello")?;
assert_eq!(store.fs_get("/out")?, Some(b"from-plugin".to_vec()));
```

## Guest ABI (`extism:host/store`)

Handles are kernel offsets, same as the PDK. Callers pass ownership of input
handles; `0` means missing / empty.

| Function | Params | Result | Notes |
| --- | --- | --- | --- |
| `get` | `i64` path | `i64` value | `0` if the path is missing |
| `put` | `i64` path, `i64` data | — | fails if the host seeded the path read-only |
| `delete` | `i64` path | — | fails on read-only paths |
| `list` | `i64` prefix | `i64` JSON array | prefix `0` lists `/` |
| `exec` | `i64` sql, `i64` params | `i64` JSON | params `0` is `[]`; `ATTACH` is denied |
| `id` | — | `i64` | the object id |

## Backends

| Backend | How | Survives process exit |
| --- | --- | --- |
| **In-memory** (default) | `DurableStore::memory(id)` | No |
| Directory | `DurableStore::open_dir(path, id)` — operator opt-in | On that machine, one writer per id |
| Snapshot bytes | `snapshot` / `from_snapshot` | If the host persists the blob |

Guest paths are virtual (`/data/x`), never host filesystem paths. WASI FS, when
it exists, should be a POSIX skin over the same object.
