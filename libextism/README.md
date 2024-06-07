# Extism C SDK

This crate contains no actual code, but is used to generated `libextism` from the [extism](../runtime) crate.

The C SDK is a little different from the other languages because it is generated from the Rust source using cbindgen. It operates at a lower level than the other SDKs because they build higher level abstractions on top of it.

## Building from source

`libextism` can be built using the `Makefile` in the root of the repository:

```shell
make
```

`libextism` will be built in `target/release/libextism.*` and the header file can be found in `runtime/extism.h`

## Installation 

The [Extism CLI](https://github.com/extism/cli) can be used to install releases from Github:

```shell
sudo PATH="$PATH" env extism lib install
```

Or from source:

```shell
sudo make install DEST=/usr/local
```

This will install the shared object into `/usr/local/lib` and `extism.h` into `/usr/local/include`.


## Getting Started

To use libextism you should include the header file:

```c
#include <extism.h>
```

and link the library:

```
-lextism
```

### Creating A Plug-in

The primary concept in Extism is the [plug-in](https://extism.org/docs/concepts/plug-in). You can think of a plug-in as a code module stored in a `.wasm` file.

Since you may not have an Extism plug-in on hand to test, let's load a demo plug-in from the web:

```c
#include <extism.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_plugin_output(ExtismPlugin *plugin, int32_t rc){
  if (rc != EXTISM_SUCCESS) {
    fprintf(stderr, "ERROR: %s\n", extism_plugin_error(plugin));
    return;
  }

  ExtismSize outlen = extism_plugin_output_length(plugin);
  const uint8_t *out = extism_plugin_output_data(plugin);
  fwrite(out, 1, outlen, stdout);
}

int main(void) {
  const char *manifest = "{\"wasm\": [{\"url\": "
                         "\"https://github.com/extism/plugins/releases/latest/"
                         "download/count_vowels.wasm\"}]}";

  char *errmsg = NULL;
  ExtismPlugin *plugin = extism_plugin_new(
      (const uint8_t *)manifest, strlen(manifest), NULL, 0, true, &errmsg);
  if (plugin == NULL) {
    fprintf(stderr, "ERROR: %s\n", errmsg);
    extism_plugin_new_error_free(errmsg);
    exit(1);
  }

  const char *input = "Hello, world!";
  print_plugin_output(plugin, extism_plugin_call(plugin, "count_vowels",
                                  (const uint8_t *)input, strlen(input)));
  extism_plugin_free(plugin);
  return 0;
}
```

> **Note**: In this case the manifest is a string constant, however it has a rich schema and a lot of options, see the [extism-manifest docs](https://docs.rs/extism-manifest/latest/extism_manifest/) for more details.

### Calling A Plug-in's Exports

This plug-in was written in Rust and it does one thing, it counts vowels in a string. As such, it exposes one "export" function: `count_vowels`. We can call exports using `extism_plugin_call`, then will use `extism_plugin_output_length`
and `extism_plugin_output_data` to get the result:

```c
int32_t rc = extism_plugin_call(plugin, "count_vowels",
                                (const uint8_t *)input, strlen(input));
if (rc != EXTISM_SUCCESS) {
  fprintf(stderr, "ERROR: %s\n", extism_plugin_error(plugin));
  exit(2);
}

ExtismSize outlen = extism_plugin_output_length(plugin);
const uint8_t *out = extism_plugin_output_data(plugin);
fwrite(out, 1, outlen, stdout);
```

Will print

```
{"count": 3, "total": 3, "vowels": "aeiouAEIOU"}
```

All exports have a simple interface of bytes-in and bytes-out. This plug-in happens to take a string and return a JSON encoded string with a report of results.

### Plug-in State

Plug-ins may be stateful or stateless. Plug-ins can maintain state b/w calls by the use of variables. Our count vowels plug-in remembers the total number of vowels it's ever counted in the "total" key in the result. You can see this by making subsequent calls to the export:

```c
print_plugin_output(plugin, extism_plugin_call(plugin, "count_vowels",
                                (const uint8_t *)input, strlen(input)));
# => {"count": 3, "total": 6, "vowels": "aeiouAEIOU"}
print_plugin_output(plugin, extism_plugin_call(plugin, "count_vowels",
                                (const uint8_t *)input, strlen(input)));
# => {"count": 3, "total": 9, "vowels": "aeiouAEIOU"}
```

These variables will persist until this plug-in is freed or you initialize a new one.

### Configuration

Plug-ins may optionally take a configuration object. This is a static way to configure the plug-in. Our count-vowels plugin takes an optional configuration to change out which characters are considered vowels. Example:

```c
const char *input = "Yellow, world!";
print_plugin_output(plugin, extism_plugin_call(plugin, "count_vowels",
                                (const uint8_t *)input, strlen(input)));
# => {"count": 3, "total": 3, "vowels": "aeiouAEIOU"}
const char * config = "{\"vowels\": \"aeiouyAEIOUY\"}";
extism_plugin_config(plugin, config, strlen(config));
print_plugin_output(plugin, extism_plugin_call(plugin, "count_vowels",
                                (const uint8_t *)input, strlen(input)));
# => {"count": 4, "total": 4, "vowels": "aeiouyAEIOUY"}
```

### Host Functions

Let's extend our count-vowels example a little bit: Instead of storing the `total` in an ephemeral plug-in var, let's store it in a persistent key-value store!

Wasm can't use our KV store on it's own. This is where [Host Functions](https://extism.org/docs/concepts/host-functions) come in.

[Host functions](https://extism.org/docs/concepts/host-functions) allow us to grant new capabilities to our plug-ins from our application. They are simply some C functions you write which can be passed down and invoked from any language inside the plug-in.

Let's load the manifest like usual but load up this `count_vowels_kvstore` plug-in from `https://github.com/extism/plugins/releases/latest/download/count_vowels.wasm`

> *Note*: The source code for this is [here](https://github.com/extism/plugins/blob/main/count_vowels_kvstore/src/lib.rs) and is written in rust, but it could be written in any of our PDK languages.

Unlike our previous plug-in, this plug-in expects you to provide host functions that satisfy our its import interface for a KV store.

We want to expose two functions to our plugin, `kv_write(key: String, value: Bytes)` which writes a bytes value to a key and `kv_read(key: String) -> Bytes` which reads the bytes at the given `key`.

```c
#include <extism.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// A stubbed out KV store
typedef struct KVStore KVStore;
extern KVStore *fake_kv_store_new();
extern void fake_kv_store_free(KVStore *kv);
extern void fake_kv_store_set(KVStore *kv, const char *key, size_t keylen,
                              uint32_t);
extern const uint32_t fake_kv_store_get(KVStore *kv, const char *key,
                                        size_t keylen);

// Our host functions to access the fake KV store
void kv_get(ExtismCurrentPlugin *plugin, const ExtismVal *inputs,
            ExtismSize ninputs, ExtismVal *outputs, ExtismSize noutputs,
            void *userdata) {
  // Cast the userdata pointer
  KVStore *kv = (KVStore *)userdata;

  // Get the offset to the key in the plugin memory
  uint64_t offs = inputs[0].v.i64;
  ExtismSize keylen = extism_current_plugin_memory_length(plugin, offs);

  // Allocate a new block to return
  uint64_t outoffs =
      extism_current_plugin_memory_alloc(plugin, sizeof(uint32_t));

  // Load the value from our k/v store
  uint64_t value = fake_kv_store_get(
      kv, (const char *)extism_current_plugin_memory(plugin) + offs, keylen);

  // Update the plugin memory
  *(uint64_t *)(extism_current_plugin_memory(plugin) + outoffs) = value;

  // Return the offset to our allocated block
  outputs[0].t = EXTISM_PTR;
  outputs[0].v.i64 = outoffs;
}

void kv_set(ExtismCurrentPlugin *plugin, const ExtismVal *inputs,
            ExtismSize ninputs, ExtismVal *outputs, ExtismSize noutputs,
            void *userdata) {
  // Cast the userdata pointer
  KVStore *kv = (KVStore *)userdata;

  // Get the offset to the key in the plugin memory
  uint64_t keyoffs = inputs[0].v.i64;
  ExtismSize keylen = extism_current_plugin_memory_length(plugin, keyoffs);

  // Get the offset to the value in the plugin memory
  uint64_t valueoffs = inputs[1].v.i64;
  ExtismSize valuelen = extism_current_plugin_memory_length(plugin, valueoffs);

  // Set key => value
  fake_kv_store_set(
      kv, (const char *)extism_current_plugin_memory(plugin) + keyoffs, keylen,
      *(uint32_t *)(extism_current_plugin_memory(plugin) + keyoffs));
}

int main(void) {
  KVStore *kv = fake_kv_store_new();
  const char *manifest = "{\"wasm\": [{\"url\": "
                         "\"https://github.com/extism/plugins/releases/latest/"
                         "download/count_vowels_kvstore.wasm\"}]}";
  const ExtismValType kv_get_inputs[] = {EXTISM_PTR};
  const ExtismValType kv_get_outputs[] = {EXTISM_PTR};
  ExtismFunction *kv_get_fn = extism_function_new(
      "kv_get", kv_get_inputs, 1, kv_get_outputs, 1, kv_get, kv, NULL);

  const ExtismValType kv_set_inputs[] = {EXTISM_PTR};
  const ExtismValType kv_set_outputs[] = {EXTISM_PTR};
  ExtismFunction *kv_set_fn = extism_function_new(
      "kv_set", kv_set_inputs, 1, kv_set_outputs, 1, kv_set, kv, NULL);
  const ExtismFunction *functions[] = {kv_get_fn};
  char *errmsg = NULL;
  ExtismPlugin *plugin = extism_plugin_new(
      (const uint8_t *)manifest, strlen(manifest), functions, 1, true, &errmsg);
  if (plugin == NULL) {
    fprintf(stderr, "ERROR: %s\n", errmsg);
    extism_plugin_new_error_free(errmsg);
    exit(1);
  }

  const char *input = "Hello, world!";
  print_plugin_output(plugin, extism_plugin_call(plugin, "count_vowels",
                                                 (const uint8_t *)input,
                                                 strlen(input)));
  print_plugin_output(plugin, extism_plugin_call(plugin, "count_vowels",
                                                 (const uint8_t *)input,
                                                 strlen(input)));

  extism_plugin_free(plugin);
  extism_function_free(kv_get_fn);
  extism_function_free(kv_set_fn);
  fake_kv_store_free(kv);
  return 0;
}
```

> *Note*: In order to write host functions you should get familiar with the `extism_current_plugin_*` functions.

Now when we invoke the event:

```c
print_plugin_output(plugin, extism_plugin_call(plugin, "count_vowels",
                                               (const uint8_t *)input,
                                               strlen(input)));
# => Read from key=count-vowels"
# => Writing value=3 from key=count-vowels"
# => {"count": 3, "total": 3, "vowels": "aeiouAEIOU"}

print_plugin_output(plugin, extism_plugin_call(plugin, "count_vowels",
                                               (const uint8_t *)input,
                                               strlen(input)));
# => Read from key=count-vowels"
# => Writing value=6 from key=count-vowels"
# => {"count": 3, "total": 6, "vowels": "aeiouAEIOU"}
```
