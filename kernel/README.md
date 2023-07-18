# Extism kernel

The Extism kernel implements core parts of the Extism runtime in Rust compiled to WebAssembly. This code is a conceptual
re-write of [memory.rs][] with the goal of making core parts of the Extism implementation more portable across WebAssembly
runtimes.

See [lib.rs][] for more details about the implementation itself.

## Building

Because this crate is built using the `wasm32-unknown-unknown` target, it is a separate build process from the `extism-runtime` crate. 

To build `extism-runtime.wasm`, strip it and copy it to the proper location in the `extism-runtime` tree you can run:

```shell
$ sh build.sh
```

[memory.rs]: https://github.com/extism/extism/blob/f4aa139eced4a74eb4a103f78222ba503e146109/runtime/src/memory.rs
[lib.rs]: ./src/lib.rs
