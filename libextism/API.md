# libextism API

We [generate C headers](https://github.com/extism/extism/blob/main/runtime/extism.h) so that any language with a C-compatible FFI can bind functions to the runtime itself and embed Extism. This is how most of the [official SDKs](/docs/concepts/host-sdk) are created.

If you would like to embed Extism into a language that we currently do not support, you should take a look at the header file linked above.

The general set of functions that is necessary to satisfy the runtime requirements is:

### `extism_plugin_new`

Create a new plugin.
- `wasm`: is a WASM module (wat or wasm) or a JSON encoded manifest
- `wasm_size`: the length of the `wasm` parameter
- `functions`: is an array of `ExtismFunction*`
- `n_functions`: is the number of functions
- `with_wasi`: enables/disables WASI
- `errmsg`: error message during plugin creation, this should be freed with
  `extism_plugin_new_error_free`


```c
ExtismPlugin extism_plugin_new(const uint8_t *wasm,
                               ExtismSize wasm_size,
                               const ExtismFunction **functions,
                               ExtismSize n_functions,
                               bool with_wasi,
                               char **errmsg);
```
---

### `extism_plugin_new_error_free`

Frees the error message returned when creating a plugin

```c
void extism_plugin_new_error_free(char *err);
```

---

### `extism_plugin_free`

Remove a plugin from the registry and free associated memory.

```c
void extism_plugin_free(ExtismPlugin *plugin);
```

---

### `extism_plugin_config`

Update plugin config values, this will merge with the existing values.

```c
bool extism_plugin_config(ExtismPlugin *plugin,
                          const uint8_t *json,
                          ExtismSize json_size);
```

---

### `extism_plugin_function_exists`

Returns true if `func_name` exists.

```c
bool extism_plugin_function_exists(ExtismPlugin *plugin,
                                   const char *func_name);
```

---

### `extism_plugin_call`

Call a function.
- `func_name`: is the function to call
- `data`: is the input data
- `data_len`: is the length of `data`

Returns `0` when the call is successful.

```c
int32_t extism_plugin_call(ExtismPlugin *plugin,
                           const char *func_name,
                           const uint8_t *data,
                           ExtismSize data_len);
```

---

### `extism_plugin_call_with_host_context`

Call a function with additional host context that can be accessed from inside host functions.
- `func_name`: is the function to call
- `data`: is the input data
- `data_len`: is the length of `data`
- `host_ctx`: an opaque pointer that can be accessed in host functions

Returns `0` when the call is successful.

```c
int32_t extism_plugin_call_with_host_context(ExtismPlugin *plugin,
                           const char *func_name,
                           const uint8_t *data,
                           ExtismSize data_len,
                           void *host_ctx);
```

---

### `extism_plugin_error`

Get the error associated with a `Plugin`

```c
const char *extism_plugin_error(ExtismPlugin *plugin);
```

---

### `extism_plugin_output_length`

Get the length of a plugin's output data.

```c
ExtismSize extism_plugin_output_length(ExtismPlugin *plugin);
```

---

### `extism_plugin_output_data`

Get the plugin's output data.

```c
const uint8_t *extism_plugin_output_data(ExtismPlugin *plugin);
```

---

### `extism_plugin_reset`

Reset the Extism runtime, this will invalidate all allocated memory.

```c
bool extism_plugin_reset(ExtismPlugin *plugin);
```

---

### `extism_log_file`

Set log file and level.

```c
bool extism_log_file(const char *filename, const char *log_level);
```

---

### `extism_log_custom`

Enable a custom log handler, this will buffer logs until `extism_log_drain`
is called Log level should be one of: info, error, trace, debug, warn

```c
bool extism_log_custom(const char *log_level);
```

---

### `extism_log_drain`

Calls the provided callback function for each buffered log line.
This is only needed when `extism_log_custom` is used.

```c
void extism_log_drain(void (*handler)(const char *, uintptr_t));
```

---

### `extism_version`

Get the Extism version string.

```c
const char *extism_version(void);
```

---

### `extism_current_plugin_memory`

Returns a pointer to the memory of the currently running plugin

```c
uint8_t *extism_current_plugin_memory(ExtismCurrentPlugin *plugin);
```

---

### `extism_current_plugin_host_context`

Get access to the host context, passed in using `extism_plugin_call_with_host_context`

```c
void *extism_current_plugin_host_context(ExtismCurrentPlugin *plugin);
```

---


### `extism_current_plugin_memory_alloc`

Allocate a memory block in the currently running plugin

```c
uint64_t extism_current_plugin_memory_alloc(ExtismCurrentPlugin *plugin, ExtismSize n);
```

---

### `extism_current_plugin_memory_length`

Get the length of an allocated block

```c
ExtismSize extism_current_plugin_memory_length(ExtismCurrentPlugin *plugin, ExtismSize n);
```

---

### `extism_current_plugin_memory_free`

Free an allocated memory block

```c
void extism_current_plugin_memory_free(ExtismCurrentPlugin *plugin, uint64_t ptr);
```

---

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

---

### `extism_function_set_namespace`

Set the namespace of an `ExtismFunction`

```c
void extism_function_set_namespace(ExtismFunction *ptr, const char *namespace_);
```

---

### `extism_function_free`

Free an `ExtismFunction`

```c
void extism_function_free(ExtismFunction *ptr);
```

---

### `extism_plugin_cancel_handle`

Get handle for plugin cancellation

```c
const ExtismCancelHandle *extism_plugin_cancel_handle(const ExtismPlugin *plugin);
```

---

### `extism_plugin_cancel`

Cancel a running plugin from another thread

```c
bool extism_plugin_cancel(const ExtismCancelHandle *handle);
```

---

## Type definitions:

### `ExtismPlugin`

```c
typedef struct ExtismPlugin ExtismPlugin;
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

### `ExtismCancelHandle`

`ExtismCancelHandle` can be used to cancel a running plugin from another thread

```c
typedef struct ExtismCancelHandle ExtismCancelHandle;
```


