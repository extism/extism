# Extism Runtime

This crate is the core of the Extism Runtime. The runtime is distributed as a shared object.
Languages and environments that do not have access to a Wasm runtime, can dynamically
load and call this object to run Extism Plug-ins. Where a native Wasm runtime already exists,
we just use the [Extism Kernel](../kernel) to bring the Extism capabilities to that Wasm runtime.

## Runtime API

We [generate C headers](https://github.com/extism/extism/blob/main/runtime/extism.h) so that any language with a C-compatible FFI can bind functions to the runtime itself and embed Extism. This is how most of the [official SDKs](/docs/concepts/host-sdk) are created.

If you would like to embed Extism into a language that we currently do not support, you should take a look at the header file linked above. 

The general set of functions that is necessary to satisfy the runtime requirements is:

### `extism_context_new`

Create a new context.

```c
struct ExtismContext *extism_context_new(void);
```

### `extism_context_free`

Free a context.

```c
void extism_context_free(struct ExtismContext *ctx);
```

### `extism_plugin_new`

Create a new plugin.
- `wasm`: is a WASM module (wat or wasm) or a JSON encoded manifest
- `wasm_size`: the length of the `wasm` parameter
- `functions`: is an array of `ExtismFunction*`
- `n_functions`: is the number of functions
- `with_wasi`: enables/disables WASI

```c
ExtismPlugin extism_plugin_new(
                               const uint8_t *wasm,
                               ExtismSize wasm_size,
                               const ExtismFunction **functions,
                               ExtismSize n_functions,
                               bool with_wasi,
                               char **errmsg);
```

### `extism_plugin_update`

Update a plugin, keeping the existing ID.

Similar to `extism_plugin_new` but takes an `index` argument to specify which plugin to update.

Memory for this plugin will be reset upon update.

```c
bool extism_plugin_update(struct ExtismContext *ctx,
                          ExtismPlugin index,
                          const uint8_t *wasm,
                          ExtismSize wasm_size,
                          const ExtismFunction **functions,
                          ExtismSize n_functions,
                          bool with_wasi);
```

### `extism_plugin_free`

Remove a plugin from the registry and free associated memory.

```c
void extism_plugin_free(struct ExtismContext *ctx, ExtismPlugin plugin);
```

### `extism_context_reset`

Remove all plugins from the registry.

```c
void extism_context_reset(struct ExtismContext *ctx);
```

### `extism_plugin_config`

Update plugin config values, this will merge with the existing values.

```c
bool extism_plugin_config(struct ExtismContext *ctx,
                          ExtismPlugin plugin,
                          const uint8_t *json,
                          ExtismSize json_size);
```

### `extism_plugin_function_exists`

Returns true if `func_name` exists.

```c
bool extism_plugin_function_exists(struct ExtismContext *ctx,
                                   ExtismPlugin plugin,
                                   const char *func_name);
```

### `extism_plugin_call`

Call a function.
- `func_name`: is the function to call
- `data`: is the input data
- `data_len`: is the length of `data`

```c
int32_t extism_plugin_call(struct ExtismContext *ctx,
                           ExtismPlugin plugin_id,
                           const char *func_name,
                           const uint8_t *data,
                           ExtismSize data_len);
```

### `extism_error`

Get the error associated with a `Context` or `Plugin`, if `plugin` is `-1` then the context error will be returned.

```c
const char *extism_error(struct ExtismContext *ctx, ExtismPlugin plugin);
```

### `extism_plugin_output_length`

Get the length of a plugin's output data.

```c
ExtismSize extism_plugin_output_length(struct ExtismContext *ctx, ExtismPlugin plugin);
```

### `extism_plugin_output_data`

Get the plugin's output data.

```c
const uint8_t *extism_plugin_output_data(struct ExtismContext *ctx, ExtismPlugin plugin);
```

### `extism_log_file`

Set log file and level.

```c
bool extism_log_file(const char *filename, const char *log_level);
```

### `extism_version`

Get the Extism version string.

```c
const char *extism_version(void);
```

### `extism_current_plugin_memory`

Returns a pointer to the memory of the currently running plugin

```c
uint8_t *extism_current_plugin_memory(ExtismCurrentPlugin *plugin);
```

### `extism_current_plugin_memory_alloc`

Allocate a memory block in the currently running plugin

```c
uint64_t extism_current_plugin_memory_alloc(ExtismCurrentPlugin *plugin, ExtismSize n);
```

### `extism_current_plugin_memory_length`

Get the length of an allocated block

```c
ExtismSize extism_current_plugin_memory_length(ExtismCurrentPlugin *plugin, ExtismSize n);
```

### `extism_current_plugin_memory_free`

Free an allocated memory block

```c
void extism_current_plugin_memory_free(ExtismCurrentPlugin *plugin, uint64_t ptr);
```

### `extism_function_new`
Create a new host function
- `name`: function name, this should be valid UTF-8
- `inputs`: argument types
- `n_inputs`: number of argument types
- `outputs`: return types
- `n_outputs`: number of return types
- `func`: the function to call
- `user_data`: a pointer that will be passed to the function when it's called
   this value should live as long as the function exists
- `free_user_data`: a callback to release the `user_data` value when the resulting
  `ExtismFunction` is freed.

Returns a new `ExtismFunction` or `null` if the `name` argument is invalid.

```c
ExtismFunction *extism_function_new(const char *name,
                                    const ExtismValType *inputs,
                                    ExtismSize n_inputs,
                                    const ExtismValType *outputs,
                                    ExtismSize n_outputs,
                                    ExtismFunctionType func,
                                    void *user_data,
                                    void (*free_user_data)(void *_));
```

### `extism_function_set_namespace`

Set the namespace of an `ExtismFunction`

```c
void extism_function_set_namespace(ExtismFunction *ptr, const char *namespace_);
```

### `extism_function_free`

Free an `ExtismFunction`

```c
void extism_function_free(ExtismFunction *ptr);
```

## Type definitions: 

### `ExtismContext`

A `Context` is used to store and manage plugins

```c
typedef struct ExtismContext ExtismContext;
```

### `ExtismPlugin`

```c
typedef int32_t ExtismPlugin;
```

### `ExtismSize`

```c
typedef uint64_t ExtismSize;
```

### `ExtismFunction`

`ExtismFunction` is used to register host functions with plugins

```c
typedef struct ExtismFunction ExtismFunction;
```

### `ExtismCurrentPlugin`

`ExtismCurrentPlugin` provides access to the currently executing plugin from within a host function

```c
typedef struct ExtismCurrentPlugin ExtismCurrentPlugin;
```
