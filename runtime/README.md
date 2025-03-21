# Extism runtime and rust-sdk

This repo contains the code for the [Extism](https://extism.org/) runtime and rust-sdk. It can be embedded in any Rust application to call Extism plug-ins.

> **Note**: If you're unsure what Extism is or what an SDK is see our homepage: [https://extism.org](https://extism.org).

## Installation

### Cargo

To use the `extism` crate, you can add it to your Cargo file:

```toml
[dependencies]
extism = "1.4.1"
```

## Environment variables

There are a few environment variables that can be used for debugging purposes:

- `EXTISM_ENABLE_WASI_OUTPUT=1`: show WASI stdout/stderr
- `EXTISM_MEMDUMP=extism.mem`: dump Extism linear memory to a file
- `EXTISM_COREDUMP=extism.core`: write [coredump](https://github.com/WebAssembly/tool-conventions/blob/main/Coredump.md) to a file when a WebAssembly function traps
- `EXTISM_DEBUG=1`: generate debug information
- `EXTISM_PROFILE=perf|jitdump|vtune`: enable Wasmtime profiling
- `EXTISM_CACHE_CONFIG=path/to/config.toml`: enable Wasmtime cache, see [the docs](https://docs.wasmtime.dev/cli-cache.html) for details about configuration. Setting this to an empty string will disable caching.

> *Note*: The debug and coredump info will only be written if the plug-in has an error.

## Getting Started

This guide should walk you through some of the concepts in Extism and the `extism` crate.

### Creating A Plug-in

The primary concept in Extism is the [plug-in](https://extism.org/docs/concepts/plug-in). You can think of a plug-in as a code module stored in a `.wasm` file.

Since you may not have an Extism plug-in on hand to test, let's load a demo plug-in from the web:

```rust
use extism::*;

fn main() {
  let url = Wasm::url(
    "https://github.com/extism/plugins/releases/latest/download/count_vowels.wasm"
  );
  let manifest = Manifest::new([url]);
  let mut plugin = Plugin::new(&manifest, [], true).unwrap();
  let res = plugin.call::<&str, &str>("count_vowels", "Hello, world!").unwrap();
  println!("{}", res);
}
```

> **Note**: See [the Manifest docs](https://docs.rs/extism-manifest/latest/extism_manifest/) as it has a rich schema and a lot of options.

### Calling A Plug-in's Exports

This plug-in was written in Rust and it does one thing, it counts vowels in a string. As such, it exposes one "export" function: `count_vowels`. We can call exports using [Extism::Plugin::call](https://docs.rs/extism/latest/extism/struct.Plugin.html#method.call):

```rust
let res = plugin.call::<&str, &str>("count_vowels", "Hello, world!").unwrap();
println!("{}", res);
# => {"count": 3, "total": 3, "vowels": "aeiouAEIOU"}
```

All exports have a simple interface of bytes-in and bytes-out. This plug-in happens to take a string and return a JSON encoded string with a report of results.

The `call` function uses [extism-convert](https://docs.rs/extism-convert) to determine which input/output types can be used. If we wanted to use a concrete type for
the `count_vowels` result, we could defined a struct:

```rust
#[derive(Debug, serde::Deserialize)]
struct VowelCount {
  count: usize,
  total: usize,
  vowels: String,
}
```

Then we can use [Json](https://docs.rs/extism-convert/latest/extism_convert/struct.Json.html) to get the JSON results decoded into `VowelCount`:

```rust
let Json(res) = plugin.call::<&str, Json<VowelCount>>("count_vowels", "Hello, world!").unwrap();
println!("{:?}", res);
# => VowelCount {count: 3, total: 3, vowels: "aeiouAEIOU"}
```

### Plug-in State

Plug-ins may be stateful or stateless. Plug-ins can maintain state between calls by the use of variables. Our count vowels plug-in remembers the total number of vowels it's ever counted in the "total" key in the result. You can see this by making subsequent calls to the export:

```rust
let res = plugin.call::<&str, &str>("count_vowels", "Hello, world!").unwrap();
println!("{}", res);
# => {"count": 3, "total": 6, "vowels": "aeiouAEIOU"}

let res = plugin.call::<&str, &str>("count_vowels", "Hello, world!").unwrap();
println!("{}", res);
# => {"count": 3, "total": 9, "vowels": "aeiouAEIOU"}
```

These variables will persist until this plug-in is freed or you initialize a new one.

### Configuration

Plug-ins may optionally take a configuration object. This is a static way to configure the plug-in. Our count-vowels plugin takes an optional configuration to change out which characters are considered vowels. Example:

```rust
let manifest = Manifest::new([url]);
let mut plugin = Plugin::new(&manifest, [], true);
let res = plugin.call::<&str, &str>("count_vowels", "Yellow, world!").unwrap();
println!("{}", res);
# => {"count": 3, "total": 3, "vowels": "aeiouAEIOU"}
let mut plugin = Plugin::new(&manifest, [], true).with_config_key("vowels", "aeiouyAEIOUY");
let res = plugin.call::<&str, &str>("count_vowels", "Yellow, world!").unwrap();
println!("{}", res);
# => {"count": 4, "total": 4, "vowels": "aeiouyAEIOUY"}
```

### Host Functions

Let's extend our count-vowels example a little bit: Instead of storing the `total` in an ephemeral plug-in var, let's store it in a persistent key-value store!

Wasm can't use our KV store on it's own. This is where [Host Functions](https://extism.org/docs/concepts/host-functions) come in.

[Host functions](https://extism.org/docs/concepts/host-functions) allow us to grant new capabilities to our plug-ins from our application. They are simply some Rust functions you write which can be passed down and invoked from any language inside the plug-in.

Let's load the manifest like usual but load up this `count_vowels_kvstore` plug-in:

```rust
let url = Wasm::url(
  "https://github.com/extism/plugins/releases/latest/download/count_vowels_kvstore.wasm"
);
let manifest = Manifest::new([url]);
```

> *Note*: The source code for this is [here](https://github.com/extism/plugins/blob/main/count_vowels_kvstore/src/lib.rs) and is written in rust, but it could be written in any of our PDK languages.

Unlike our previous plug-in, this plug-in expects you to provide host functions that satisfy our its import interface for a KV store.

We want to expose two functions to our plugin, `kv_write(key: String, value: Bytes)` which writes a bytes value to a key and `kv_read(key: String) -> Bytes` which reads the bytes at the given `key`.

```rust
use extism::*;

// pretend this is redis or something :)
type KVStore = std::collections::BTreeMap<String, Vec<u8>>;

// When a first argument separated with a semicolon is provided to `host_fn` it is used as the
// variable name and type for the `UserData` parameter
host_fn!(kv_read(user_data: KVStore; key: String) -> u32 {
    let kv = user_data.get()?;
    let kv = kv.lock().unwrap();
    let value = kv
        .get(&key)
        .map(|x| u32::from_le_bytes(x.clone().try_into().unwrap()))
        .unwrap_or_else(|| 0u32);
    Ok(value)
});

host_fn!(kv_write(user_data: KVStore; key: String, value: u32) {
    let kv = user_data.get()?;
    let mut kv = kv.lock().unwrap();
    kv.insert(key, value.to_le_bytes().to_vec());
    Ok(())
});

fn main() {
    let kv_store = UserData::new(KVStore::default());

    let url = Wasm::url(
        "https://github.com/extism/plugins/releases/latest/download/count_vowels_kvstore.wasm",
    );
    let manifest = Manifest::new([url]);
    let mut plugin = PluginBuilder::new(manifest)
        .with_wasi(true)
        .with_function(
            "kv_read",
            [PTR],
            [PTR],
            kv_store.clone(),
            kv_read,
        )
        .with_function(
            "kv_write",
            [PTR, PTR],
            [],
            kv_store.clone(),
            kv_write,
        )
        .build()
        .unwrap();

    for _ in 0..5 {
        let res = plugin
            .call::<&str, &str>("count_vowels", "Hello, world!")
            .unwrap();
        println!("{}", res);
    }
}
```

> *Note*: In order to write host functions you should get familiar with the methods on the [CurrentPlugin](https://docs.rs/extism/latest/extism/struct.CurrentPlugin.html) and [UserData](https://docs.rs/extism/latest/extism/enum.UserData.html) types.

Now we can invoke the event:

```rust
let res = plugin.call::<&str, &str>("count_vowels", "Hello, world!").unwrap();
println!("{}", res);
# => Read from key=count-vowels"
# => Writing value=3 from key=count-vowels"
# => {"count": 3, "total": 3, "vowels": "aeiouAEIOU"}

let res = plugin.call::<&str, &str>("count_vowels", "Hello, world!").unwrap();
println!("{}", res);
# => Read from key=count-vowels"
# => Writing value=6 from key=count-vowels"
# => {"count": 3, "total": 6, "vowels": "aeiouAEIOU"}
```

### Logging

Plug-ins can't directly print anything to the console. They can however use Extism's built-in logging functionality, for example the [`log!` macro in the rust-pdk](https://docs.rs/extism-pdk/1.4.0/extism_pdk/macro.log.html) or the [`logInfo` function in the haskell-pdk](https://hackage.haskell.org/package/extism-pdk-1.2.0.0/docs/Extism-PDK.html#v:logInfo).

Inside your host application, the rust-sdk emits these as [tracing](https://github.com/tokio-rs/tracing) events. The simplest way to make the logged messages visible is by adding the `tracing_subscriber` dependency to your crate and then initializing a tracing subscriber at the top of your main function:

```rust
tracing_subscriber::fmt::init();
```

