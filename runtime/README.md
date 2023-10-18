# Extism runtime and rust-sdk

This repo contains the code for the [Extism](https://extism.org/) runtime and rust-sdk. It can be embedded in any Rust application to call Extism plug-ins.

> **Note**: If you're unsure what Extism is or what an SDK is see our homepage: [https://extism.org](https://extism.org).

## Installation

### Cargo

To use the `extism` crate, you can add it to your Cargo file:

```toml
[depdendencies]
extism = "*"
```

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
  let plugin = Plugin::new_with_manifest(&manifest);
}
```

> **Note**: See [the Manifest docs](https://docs.rs/extism-manifest/0.5.0/extism_manifest/) as it has a rich schema and a lot of options.

### Calling A Plug-in's Exports

This plug-in was written in Rust and it does one thing, it counts vowels in a string. As such, it exposes one "export" function: `count_vowels`. We can call exports using [Extism::Plugin#call](https://extism.github.io/ruby-sdk/Extism/Plugin.html#call-instance_method):

```rust
let res = plugin.call::<String, String>::("count_vowels", "Hello, world!").unwrap();
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
let Json(res) = plugin.call::<String, Json<VowelCount>>::("count_vowels", "Hello, world!").unwrap();
println!("{:?}", res);
# => VowelCount {count: 3, total: 3, vowels: "aeiouAEIOU"}
```

### Plug-in State

Plug-ins may be stateful or stateless. Plug-ins can maintain state b/w calls by the use of variables. Our count vowels plug-in remembers the total number of vowels it's ever counted in the "total" key in the result. You can see this by making subsequent calls to the export:

```rust
let res = plugin.call::<String, String>::("count_vowels", "Hello, world!").unwrap();
println!("{}", res);
# => {"count": 3, "total": 6, "vowels": "aeiouAEIOU"}

let res = plugin.call::<String, String>::("count_vowels", "Hello, world!").unwrap();
println!("{}", res);
# => {"count": 3, "total": 9, "vowels": "aeiouAEIOU"}
```

These variables will persist until this plug-in is freed or you initialize a new one.

### Configuration

Plug-ins may optionally take a configuration object. This is a static way to configure the plug-in. Our count-vowels plugin takes an optional configuration to change out which characters are considered vowels. Example:

```ruby
plugin = Extism::Plugin.new(manifest)
plugin.call("count_vowels", "Yellow, World!")
# => {"count": 3, "total": 3, "vowels": "aeiouAEIOU"}

plugin = Extism::Plugin.new(manifest, config: { vowels: "aeiouyAEIOUY" })
plugin.call("count_vowels", "Yellow, World!")
# => {"count": 4, "total": 4, "vowels": "aeiouAEIOUY"}
```

### Host Functions

Let's extend our count-vowels example a little bit: Instead of storing the `total` in an ephemeral plug-in var, let's store it in a persistent key-value store!

Wasm can't use our KV store on it's own. This is where [Host Functions](https://extism.org/docs/concepts/host-functions) come in.

[Host functions](https://extism.org/docs/concepts/host-functions) allow us to grant new capabilities to our plug-ins from our application. They are simply some ruby methods you write which can be passed down and invoked from any language inside the plug-in.

Let's load the manifest like usual but load up this `count_vowels_kvstore` plug-in:

```ruby
url = "https://github.com/extism/plugins/releases/latest/download/count_vowels_kvstore.wasm"
manifest = Extism::Manifest.from_url(url)
```

> *Note*: The source code for this is [here](https://github.com/extism/plugins/blob/main/count_vowels_kvstore/src/lib.rs) and is written in rust, but it could be written in any of our PDK languages.

Unlike our previous plug-in, this plug-in expects you to provide host functions that satisfy our its import interface for a KV store.

In the ruby sdk, we have a concept for this called a [Host Environment](https://extism.github.io/ruby-sdk/Extism/HostEnvironment.html). An environment is an instance of a class that implements any host functions your plug-in needs.

We want to expose two functions to our plugin, `kv_write(key: String, value: Bytes)` which writes a bytes value to a key and `kv_read(key: String) -> Bytes` which reads the bytes at the given `key`.

```ruby
# pretend this is Redis or something :)
KV_STORE = {}

class KvEnvironment
  include Extism::HostEnvironment

  # We need to describe the wasm function signature of each host function
  # to register them to this environment
  register_import :kv_read, [Extism::ValType::I64], [Extism::ValType::I64]
  register_import :kv_write, [Extism::ValType::I64, Extism::ValType::I64], []

  def kv_read(plugin, inputs, outputs, _user_data)
    key = plugin.input_as_string(inputs.first)
    val = KV_STORE[key] || [0].pack('V') # get 4 LE bytes for 0 default
    puts "Read from key=#{key}"
    plugin.output_string(outputs.first, val)
  end

  def kv_write(plugin, inputs, _outputs, _user_data)
    key = plugin.input_as_string(inputs.first)
    val = plugin.input_as_string(inputs[1])
    puts "Writing value=#{val.unpack1('V')} from key=#{key}"
    KV_STORE[key] = val
  end
end
```

> *Note*: In order to write host functions you should get familiar with the methods on the [Extism::CurrentPlugin](https://extism.github.io/ruby-sdk/Extism/CurrentPlugin.html) class. The `plugin` parameter is an instance of this class.

Now we just need to create a new host environment and pass it in when loading the plug-in. Here our environment initializer takes no arguments, but you could imagine putting some customer specific instance variables in there:

```ruby
env = KvEnvironment.new
plugin = Extism::Plugin.new(manifest, environment: env)
```

Now we can invoke the event:

```ruby
plugin.call("count_vowels", "Hello, World!")
# => Read from key=count-vowels"
# => Writing value=3 from key=count-vowels"
# => {"count": 3, "total": 3, "vowels": "aeiouAEIOU"}
plugin.call("count_vowels", "Hello, World!")
# => Read from key=count-vowels"
# => Writing value=6 from key=count-vowels"
# => {"count": 3, "total": 6, "vowels": "aeiouAEIOU"}
```



