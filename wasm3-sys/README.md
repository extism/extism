# wasm3-sys

Extism's in-tree Wasm3 engine. Compiles [Wasm3 v0.9.0](https://github.com/wasm3/wasm3/releases/tag/v0.9.0)
from vendored C sources with `cc`. This is **not** the crates.io `wasm3` crate
(0.3.1, last published 2021).

```rust
use wasm3_sys::{Environment, Runtime};

let env = Environment::new()?;
let mut rt = Runtime::new(&env, Runtime::DEFAULT_STACK)?;
let wasm = wat::parse_str(r#"(module (func (export "add") (param i32 i32) (result i32)
    local.get 0 local.get 1 i32.add))"#)?;
rt.parse_and_load(&wasm)?;
let add = rt.find_function("add")?;
assert_eq!(add.call_i32(&[3, 6])?, 9);
```

See [docs/wasm3-port.md](../docs/wasm3-port.md) for how this fits the Extism kernel.
