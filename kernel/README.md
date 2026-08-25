# Extism kernel

The Extism kernel implements core parts of the Extism runtime in Rust:

- Isolated memory from both host and plugin
- An allocator for managing that memory
- Input/output and error-message handling

It is a conceptual re-write of [memory.rs][] so those pieces can move between
WebAssembly runtimes.

There are two builds of the same protocol:

| Build | How | Who uses it |
| --- | --- | --- |
| `wasm32-unknown-unknown` | `sh build.sh` → `runtime/src/extism-runtime.wasm` | Wasmtime instantiates this as `extism:host/env` |
| native | `cargo test` / `cargo build` in this crate | Host-side [`Kernel`](src/host.rs). Engines that cannot instantiate the kernel as a sibling Wasm module with its own memory (notably [Wasm3](https://github.com/wasm3/wasm3)) should call this from host functions. |

See [lib.rs][] for the allocator, and [docs/wasm3-port.md](../docs/wasm3-port.md)
for why Wasm3 needs the native path.

## Building the Wasm kernel

Because `extism-runtime.wasm` uses the `wasm32-unknown-unknown` target, it is a
separate build from the `extism` crate.

```shell
$ sh build.sh
```

That compiles the kernel, merges `extism-context.wat` (the `extism_context`
externref global), strips the result, and copies it to
`runtime/src/extism-runtime.wasm`.

## Native kernel

```shell
$ cargo test
```

Runs the host `Kernel` tests (no Wasm engine required). This crate no longer
forces `--target wasm32-unknown-unknown` via `.cargo/config.toml`; `build.sh`
still passes that target explicitly.

[memory.rs]: https://github.com/extism/extism/blob/f4aa139eced4a74eb4a103f78222ba503e146109/runtime/src/memory.rs
[lib.rs]: ./src/lib.rs
