#pragma once

#include <stdint.h>
#include <stdbool.h>

#define EXTISM_FUNCTION(N) extern void N(ExtismCurrentPlugin*, const ExtismVal*, ExtismSize, ExtismVal*, ExtismSize, void*)
#define EXTISM_GO_FUNCTION(N) extern void N(void*, ExtismVal*, ExtismSize, ExtismVal*, ExtismSize, uintptr_t)


/**
 * A list of all possible value types in WebAssembly.
 */
typedef enum {
  /**
   * Signed 32 bit integer.
   */
  I32,
  /**
   * Signed 64 bit integer.
   */
  I64,
  /**
   * Floating point 32 bit integer.
   */
  F32,
  /**
   * Floating point 64 bit integer.
   */
  F64,
  /**
   * A 128 bit number.
   */
  V128,
  /**
   * A reference to a Wasm function.
   */
  FuncRef,
  /**
   * A reference to opaque data in the Wasm instance.
   */
  ExternRef,
} ExtismValType;

/**
 * A `Context` is used to store and manage plugins
 */
typedef struct ExtismContext ExtismContext;

/**
 * Wraps host functions
 */
typedef struct ExtismFunction ExtismFunction;

/**
 * Plugin contains everything needed to execute a WASM function
 */
typedef struct ExtismCurrentPlugin ExtismCurrentPlugin;

typedef uint64_t ExtismSize;

/**
 * A union type for host function argument/return values
 */
typedef union {
  int32_t i32;
  int64_t i64;
  float f32;
  double f64;
} ExtismValUnion;

/**
 * `ExtismVal` holds the type and value of a function argument/return
 */
typedef struct {
  ExtismValType t;
  ExtismValUnion v;
} ExtismVal;

/**
 * Host function signature
 */
typedef void (*ExtismFunctionType)(ExtismCurrentPlugin *plugin, const ExtismVal *inputs, ExtismSize n_inputs, ExtismVal *outputs, ExtismSize n_outputs, void *data);

typedef int32_t ExtismPlugin;

/**
 * Create a new context
 */
ExtismContext *extism_context_new(void);

/**
 * Free a context
 */
void extism_context_free(ExtismContext *ctx);

/**
 * Returns a pointer to the memory of the currently running plugin
 * NOTE: this should only be called from host functions.
 */
uint8_t *extism_current_plugin_memory(ExtismCurrentPlugin *plugin);

/**
 * Allocate a memory block in the currently running plugin
 * NOTE: this should only be called from host functions.
 */
uint64_t extism_current_plugin_memory_alloc(ExtismCurrentPlugin *plugin, ExtismSize n);

/**
 * Get the length of an allocated block
 * NOTE: this should only be called from host functions.
 */
ExtismSize extism_current_plugin_memory_length(ExtismCurrentPlugin *plugin, ExtismSize n);

/**
 * Free an allocated memory block
 * NOTE: this should only be called from host functions.
 */
void extism_current_plugin_memory_free(ExtismCurrentPlugin *plugin, uint64_t ptr);

/**
 * Create a new host function
 *
 * Arguments
 * - `name`: function name, this should be valid UTF-8
 * - `inputs`: argument types
 * - `n_inputs`: number of argument types
 * - `outputs`: return types
 * - `n_outputs`: number of return types
 * - `func`: the function to call
 * - `user_data`: a pointer that will be passed to the function when it's called
 *    this value should live as long as the function exists
 * - `free_user_data`: a callback to release the `user_data` value when the resulting
 *   `ExtismFunction` is freed.
 *
 * Returns a new `ExtismFunction` or `null` if the `name` argument is invalid.
 */
ExtismFunction *extism_function_new(const char *name,
                                    const ExtismValType *inputs,
                                    ExtismSize n_inputs,
                                    const ExtismValType *outputs,
                                    ExtismSize n_outputs,
                                    ExtismFunctionType func,
                                    void *user_data,
                                    void (*free_user_data)(void *_));

/**
 * Free an `ExtismFunction`
 */
void extism_function_free(ExtismFunction *ptr);

/**
 * Create a new plugin with additional host functions
 *
 * `wasm`: is a WASM module (wat or wasm) or a JSON encoded manifest
 * `wasm_size`: the length of the `wasm` parameter
 * `functions`: an array of `ExtismFunction*`
 * `n_functions`: the number of functions provided
 * `with_wasi`: enables/disables WASI
 */
ExtismPlugin extism_plugin_new(ExtismContext *ctx,
                               const uint8_t *wasm,
                               ExtismSize wasm_size,
                               const ExtismFunction **functions,
                               ExtismSize n_functions,
                               bool with_wasi);

/**
 * Update a plugin, keeping the existing ID
 *
 * Similar to `extism_plugin_new` but takes an `index` argument to specify
 * which plugin to update
 *
 * Memory for this plugin will be reset upon update
 */
bool extism_plugin_update(ExtismContext *ctx,
                          ExtismPlugin index,
                          const uint8_t *wasm,
                          ExtismSize wasm_size,
                          const ExtismFunction **functions,
                          ExtismSize nfunctions,
                          bool with_wasi);

/**
 * Remove a plugin from the registry and free associated memory
 */
void extism_plugin_free(ExtismContext *ctx, ExtismPlugin plugin);

/**
 * Remove all plugins from the registry
 */
void extism_context_reset(ExtismContext *ctx);

/**
 * Update plugin config values, this will merge with the existing values
 */
bool extism_plugin_config(ExtismContext *ctx,
                          ExtismPlugin plugin,
                          const uint8_t *json,
                          ExtismSize json_size);

/**
 * Returns true if `func_name` exists
 */
bool extism_plugin_function_exists(ExtismContext *ctx, ExtismPlugin plugin, const char *func_name);

/**
 * Call a function
 *
 * `func_name`: is the function to call
 * `data`: is the input data
 * `data_len`: is the length of `data`
 */
int32_t extism_plugin_call(ExtismContext *ctx,
                           ExtismPlugin plugin_id,
                           const char *func_name,
                           const uint8_t *data,
                           ExtismSize data_len);

/**
 * Get the error associated with a `Context` or `Plugin`, if `plugin` is `-1` then the context
 * error will be returned
 */
const char *extism_error(ExtismContext *ctx, ExtismPlugin plugin);

/**
 * Get the length of a plugin's output data
 */
ExtismSize extism_plugin_output_length(ExtismContext *ctx, ExtismPlugin plugin);

/**
 * Get the length of a plugin's output data
 */
const uint8_t *extism_plugin_output_data(ExtismContext *ctx, ExtismPlugin plugin);

/**
 * Set log file and level
 */
bool extism_log_file(const char *filename, const char *log_level);

/**
 * Get the Extism version string
 */
const char *extism_version(void);
