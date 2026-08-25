# Porting Extism from Wasmtime to Wasm3

This is an exploration of what it would take to replace Wasmtime with
[Wasm3](https://github.com/wasm3/wasm3) as Extism's embedded Wasm engine.

**Recommendation:** do not try to load `extism-runtime.wasm` inside Wasm3.
Implement the kernel as native host code (this crate's [`Kernel`](../kernel/src/host.rs))
and use Wasm3 only to run plugin modules. Cut or reimplement several runtime
features that exist only because Wasmtime provides them.

A native kernel is already in this branch; it is the first piece a Wasm3
runtime would call from `m3_LinkRawFunction` host stubs.

## How Extism uses Wasmtime today

The runtime is not a thin wrapper. `runtime/` is built around Wasmtime types:

| Wasmtime type | Where it leaks |
| --- | --- |
| `Engine` / `Config` / `Cache` | `CompiledPlugin`, `PluginBuilder::with_wasmtime_config`, compilation cache |
| `Store<CurrentPlugin>` / `Linker` / `Instance` / `InstancePre` | `Plugin` internals, store reset, lazy instantiate |
| `Module` | Manifest loading, WAT + binary, multi-module maps |
| `Val` | **Public** `extism::Val` is `wasmtime::Val` |
| `Caller<CurrentPlugin>` | Host functions and every PDK builtin |
| `ExternRef` / GC | `extism_context` host context |
| `epoch_interruption` | Timeouts and `CancelHandle` (cross-thread) |
| `fuel` | `with_fuel_limit` / `fuel_consumed` |
| `wasi-common` | WASI preview 1, `allowed_paths`, readonly dirs |
| `ResourceLimiter` | `memory.max_pages` |
| Coredump / profiler / debug info | `DebugOptions` |

The call path is: compile modules → `Linker` wires kernel + PDK + user hosts +
WASI → `instantiate_pre` → on `call`, instantiate, write input into **kernel**
memory, invoke the export, read output from **kernel** memory.

## Start with the kernel (this is the important part)

The kernel was written specifically so the allocator / I/O protocol would be
portable across engines. That portability assumed an engine that can:

1. Instantiate a **separate** Wasm module with **its own** linear memory.
2. Re-export that module's functions as imports for the plugin (`Linker::module`).
3. Let the host map that memory (`Memory::data_ptr`).

Plugins never share linear memory with the kernel. A typical plugin (e.g.
`wasm/echo.wasm`) imports `extism:host/env::{alloc, store_u8, output_set, …}`
and keeps its own `(memory …)`. I/O is copied through those functions, byte or
`u64` at a time. The host reads kernel memory to return results.

`extism-runtime.wasm` itself is tiny (~3.5KiB), has **no imports**, exports
`memory` plus the alloc/I/O functions, and a mutable `externref` global
(`extism_context`) merged in from `kernel/extism-context.wat`. It does **not**
use Wasm atomics, SIMD, bulk-memory, or GC in the compiled image — Rust
`AtomicU*` on `wasm32-unknown-unknown` without `+atomics` lowers to ordinary
loads/stores. Reference-types (`ref.null extern`) is the only extra proposal.

### Why Wasm3 cannot host the kernel as Wasm

Wasm3's linking model is **host functions only** (`m3_LinkRawFunction`). There
is no `Linker::module`. Two further engine constraints:

- **One linear memory per `M3Runtime`.** Every module loaded into that runtime
  shares it ([wasm3#546](https://github.com/wasm3/wasm3/issues/546)). Loading
  kernel (16-page memory, allocator root at offset 1) next to a plugin
  (its own stack/heap at offset 0) would collide.
- **No Wasm-to-Wasm import wiring.** Plugin imports of `extism:host/env::alloc`
  cannot be bound to the kernel module's `alloc` export. They can only be bound
  to a C/Rust host function.

So the kernel's original "compile me to Wasm and instantiate me beside the
plugin" trick, which is elegant on Wasmtime, is a dead end on Wasm3.

### What to do instead

Run the same allocator **natively**:

```
plugin Wasm  --imports-->  Wasm3 host functions  --call-->  kernel::Kernel (Vec<u8>)
                                |                              |
                                +-- PDK (config, vars, http) --+
                                +-- user host functions
                                +-- optional WASI
```

Handles remain offsets into kernel memory (never native pointers). The plugin's
Wasm linear memory stays Wasm3's `m3_GetMemory`. That matches today's isolation
model.

`kernel::Kernel` in this branch is that native allocator. It is the piece to
link from Wasm3 raw-call stubs with signatures like `I(I)` for `alloc`.

Native vs wasm32 layout: the wasm kernel places `MemoryRoot` at offset 1,
which is not `AtomicU64`-aligned. That is fine inside Wasm (the compiled
kernel does not emit atomic opcodes). The host kernel places the root at
offset 8 and rounds block sizes up to `align_of::<MemoryBlock>()` so native
debug builds do not abort on misaligned dereferences. Handles are not a
stable cross-engine ABI; only the function protocol is.

## Feature cuts and hard problems

Legend: **keep** / **cut** / **replace** / **problem**.

### Can keep (with work)

| Feature | Notes |
| --- | --- |
| PDK builtins (`config_get`, `var_get`/`var_set`, `http_*`, logs) | Already host functions. Rewrite `Caller<CurrentPlugin>` → `&mut CurrentPlugin` + `Kernel`. |
| User host functions | Same, but signatures are runtime-dynamic. Wasm3 wants a static C signature string (`"I(I)"`). Need a small dispatcher, not `link_closure` per arity only. |
| Manifest load from bytes / file / HTTP | Independent of the engine. Drop WAT or convert with the `wat` crate before `m3_ParseModule`. |
| Plugin vars, HTTP allowlists, `max_http_response_bytes`, `max_var_bytes` | Host-side, unchanged. |
| `Pool` | Thread-safe plugin reuse. Wasm3 runtimes are not re-entrant; keep one runtime per `Plugin`. |
| C SDK `ExtismVal` (i32/i64/f32/f64) | Already engine-agnostic. |
| Guest `_initialize` / `__wasm_call_ctors` / `hs_init` | Just `m3_FindFunction` + `m3_Call`. |

### Cut (Wasmtime-only, acceptable losses)

| Feature | Why |
| --- | --- |
| `PluginBuilder::with_wasmtime_config` | No equivalent. |
| Compilation cache (`EXTISM_CACHE_CONFIG`, `with_cache_config`) | Interpreter; nothing to cache except maybe parsed modules. |
| JIT profiling (`EXTISM_PROFILE=perf\|jitdump\|vtune`) | No JIT. |
| Wasmtime coredumps | Wasm3 has optional backtraces (`m3_GetBacktrace` / `d_m3RecordBacktraces`). |
| Wasm GC, typed function references, exceptions as configured today | Wasm3: GC is N/A; typed refs incomplete; EH exists but imported tags do not alias across modules. Extism does not need these for the PDK. |
| SIMD (`v128` opcodes) | Wasm3 parses `v128` types so LLVM leftover locals don't reject a module, then **traps on SIMD opcodes**. Plugins built with `+simd128` will not run. |
| Memory64 | WIP in Wasm3; Extism uses `i64` pointers into a 32-bit memory. |
| `wasmtime-exceptions` feature | Cut. |
| Precompiled `CompiledPlugin` as AOT machine code | Wasm3 parses to metacode at load. `CompiledPlugin` can still mean "parsed bytes + manifest" but not Cranelift artifacts. |

### Replace (doable, different shape)

| Feature | Wasm3 reality |
| --- | --- |
| WAT input | Use `wat::parse_bytes` (or require binary). Easy. |
| `memory.max_pages` | No `ResourceLimiter`. Rewrite the plugin's memory section max before parse, or cap `Kernel` via `Kernel::with_max_pages` (plugin memory still needs an engine-side cap — Wasm3's `d_m3MaxLinearMemoryPages` is compile-time). |
| `Val` public type | Must become Extism's own enum (`I32/I64/F32/F64`, maybe drop `V128`/`FuncRef`/`ExternRef`). **Rust SDK breaking change.** |
| Host context (`call_with_host_context`, `extism_context` externref) | Wasm3 has `externref` and `m3_GetUserData`. Put a `*mut CurrentPlugin` (or a slot in userdata) on the runtime instead of a GC `ExternRef`. |
| Fuel | Wasm3 does not count instructions. Official approach is **binary instrumentation** (`wasm-metering` / `wasm-instrument`) plus a `metering.usegas` import. Different numbers than Wasmtime fuel; same idea. ~2× slower on top of the interpreter. |
| WASI | Wasm3's `m3_LinkWASI` is a process-global snapshot of `wasi_unstable` / `wasi_snapshot_preview1`. It does **not** implement Extism's per-plugin `allowed_paths`, readonly prefixes, or `EXTISM_ENABLE_WASI_OUTPUT`. Either cut WASI, or re-bind the subset plugins actually import (`fd_write`, `fd_seek`, `fd_close`, `random_get`, …) ourselves. |

### Problems (the ones that can sink the port)

#### 1. Timeouts and `CancelHandle` — no epoch interrupt

Wasmtime: a timer thread calls `engine.increment_epoch()`; the running store
traps with `Trap::Interrupt`. This is how `timeout_ms` and
`Plugin::cancel_handle()` work, including infinite loops with no host calls.

Wasm3: `m3_Call` blocks the thread. `m3_Yield` runs on **function entry**, not
inside loops ([wasm3#138](https://github.com/wasm3/wasm3/issues/138)). There is
no safe cross-thread cancel.

Options, none of them good:

1. **Instrument loops** (same as fuel) and check a cancel flag in `usegas`.
   Works for timeout + fuel together. Requires rewriting every plugin binary.
2. **Kill the thread** (`pthread_cancel` / stop a dedicated executor thread).
   Unsafe if a host function holds locks; Extism host functions do.
3. **Patch Wasm3** to test an atomic flag in the interpreter loop. Best
   long-term, but you are now maintaining a Wasm3 fork. The upstream project is
   in [minimal maintenance](https://github.com/wasm3/wasm3).

Without one of these, Extism cannot offer the current "untrusted plugin cannot
wedge the host" story. That is a core product feature, not a nicety.

#### 2. Multi-module manifests (`runtime/examples/linking.rs`)

Extism can load several Wasm modules and let `main` import another's exports
(e.g. `upper.wasm` providing `extism:host/user::host_reflect` for
`reflect.wasm`). Wasmtime's linker does this natively.

Wasm3 cannot. You would have to:

- Load each module (memory-sharing problem if more than one defines memory).
- For every export used as an import, install a host trampoline that
  `m3_Call`s the other module.

`upper.wasm` happens to define **no memory** and only talks to the kernel, so
a trampoline might work for that demo. General plugin-to-plugin linking with
two memories will not. **Call this a cut** unless someone builds the trampoline
layer and restricts linked modules to "no memory / kernel-only."

#### 3. Bindings story

`wasm3` on crates.io is **0.3.1 from 2021**, marked looking-for-maintainer.
Do not use it. This branch vendors **Wasm3 v0.9.0** (2026-08-24) under
`wasm3-sys/wasm3/` and compiles it with `cc` (`wasm3-sys`). The C API is
bound by hand so we do not need libclang at build time.

#### 4. SIMD / modern toolchains

Default `rustc` + LTO can emit SIMD for `wasm32-unknown-unknown` / WASI.
Those plugins load on Wasmtime (SIMD enabled) and **fail at compile-time** on
Wasm3. Document `RUSTFLAGS=-C target-feature=-simd128` for PDK builds, or
reject modules that contain `0xFD` opcodes with a clear error.

Large in-tree fixtures (`code.wasm`, `http.wasm`, `kitchensink.wasm`) are
debug-heavy Rust WASI builds; opcode-byte counts are dominated by DWARF, not
necessarily SIMD. Real compatibility needs loading them in Wasm3, not grepping
bytes.

#### 5. Performance and size (the actual reason people pick Wasm3)

Wasm3 is a fast *interpreter*: better startup, much smaller native binary, no
Cranelift. Plugin throughput will drop versus Wasmtime, often a lot. Fine for
embedded / iOS-style "cannot JIT" hosts; a regression for server plugin
platforms. Be explicit about the audience.

## Suggested implementation sequence

1. **Native kernel** (done in this branch). Keep the wasm32 kernel for
   Wasmtime until the runtime actually switches.
2. **Engine-agnostic `Val` / host-function callback** (`&mut CurrentPlugin`,
   not `Caller<_>`). Required anyway; unblocks both a Wasm3 backend and tests.
3. **Thin Wasm3 crate** in-tree (done): `wasm3-sys` vendors v0.9.0, compiles
   with `cc`, and exposes Environment / Runtime / parse / link raw / call /
   memory. `tests/echo.rs` runs `echo.wasm` through native `Kernel` host stubs.
4. **Minimal `Plugin`**: load one plugin module, link kernel host functions +
   PDK, `call` `echo` / `count_vowels` without WASI. The echo test is a sketch
   of this; `runtime/` is still Wasmtime.
5. **Host functions** with a signature encoder (`ValType` → `"iI fF"`).
6. Decide WASI: cut, or a small custom bind of the imported snapshot.
7. **Timeout/cancel**: pick instrumentation vs Wasm3 patch *before* claiming
   feature parity. Do not ship without one if untrusted plugins matter.
8. WAT via `wat` crate; drop cache/profiler/coredump/wasmtime config; document
   SIMD and multi-module cuts.
9. Only then consider replacing Wasmtime in `runtime/` instead of
   `cfg`-switching engines.

## Public API breakage (Rust SDK)

These cannot stay if Wasmtime is removed:

- `pub type Val = wasmtime::Val`
- `PluginBuilder::with_wasmtime_config`
- `PluginBuilder::with_profiling_strategy(wasmtime::ProfilingStrategy)`
- `PluginBuilder::with_cache_config` / `with_cache_disabled` (or keep as no-ops)
- Re-exporting `wasmtime::Config` in docs/examples

C / language SDKs that go through `libextism` see `ExtismVal` and
`with_wasi` / fuel C APIs. Those can stay; fuel and WASI become
best-effort or return "unsupported."

## What we are not doing in this branch

This is not a working Wasm3 `Plugin`. Wiring `runtime/src/plugin.rs` to Wasm3
is the bulk of the port (linker, WASI, timer, SDK, tests). The native kernel
and `wasm3-sys` exist so that work has an I/O layer and an engine to call.

[lib.rs]: ./src/lib.rs
[Wasm3]: https://github.com/wasm3/wasm3
