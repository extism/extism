#pragma once

#include <stdint.h>
#include <stdbool.h>

/**
 * A list of all possible value types in WebAssembly.
 */
typedef enum ExtismValType {
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

typedef struct ExtismFunction ExtismFunction;

typedef int32_t ExtismPlugin;

typedef uint64_t ExtismSize;

typedef union ExtismValUnion {
  int32_t i32;
  int64_t i64;
  float f32;
  double f64;
} ExtismValUnion;

typedef struct ExtismVal {
  enum ExtismValType t;
  union ExtismValUnion v;
} ExtismVal;

/**
 * Create a new context
 */
struct ExtismContext *extism_context_new(void);

/**
 * Free a context
 */
void extism_context_free(struct ExtismContext *ctx);

/**
 * Create a new plugin
 *
 * `wasm`: is a WASM module (wat or wasm) or a JSON encoded manifest
 * `wasm_size`: the length of the `wasm` parameter
 * `with_wasi`: enables/disables WASI
 */
ExtismPlugin extism_plugin_new(struct ExtismContext *ctx,
                               const uint8_t *wasm,
                               ExtismSize wasm_size,
                               bool with_wasi);

uint8_t *extism_current_plugin_memory(struct ExtismContext *ctx);

uint64_t extism_current_plugin_alloc(struct ExtismContext *ctx, ExtismSize n);

ExtismSize extism_current_plugin_length(struct ExtismContext *ctx, uint64_t n);

void extism_current_plugin_free(struct ExtismContext *ctx, uint64_t ptr);

struct ExtismFunction *extism_function_new(const char *name,
                                           const enum ExtismValType *inputs,
                                           ExtismSize n_inputs,
                                           const enum ExtismValType *outputs,
                                           ExtismSize n_outputs,
                                           void (*func)(const struct ExtismVal *inputs, ExtismSize n_inputs, struct ExtismVal *outputs, ExtismSize n_outputs, void *data),
                                           void *user_data,
                                           void (*free_user_data)(void *_));

void extism_function_free(struct ExtismFunction *ptr);

/**
 * Create a new plugin with additional host functions
 *
 * `wasm`: is a WASM module (wat or wasm) or a JSON encoded manifest
 * `wasm_size`: the length of the `wasm` parameter
 * `with_wasi`: enables/disables WASI
 */
ExtismPlugin extism_plugin_new_with_functions(struct ExtismContext *ctx,
                                              const uint8_t *wasm,
                                              ExtismSize wasm_size,
                                              const struct ExtismFunction *const *functions,
                                              uint32_t nfunctions,
                                              bool with_wasi);

/**
 * Update a plugin, keeping the existing ID
 *
 * Similar to `extism_plugin_new` but takes an `index` argument to specify
 * which plugin to update
 *
 * Memory for this plugin will be reset upon update
 */
bool extism_plugin_update(struct ExtismContext *ctx,
                          ExtismPlugin index,
                          const uint8_t *wasm,
                          ExtismSize wasm_size,
                          bool with_wasi);

/**
 * Update a plugin including host functions, keeping the existing ID
 *
 * Similar to `extism_plugin_new` but takes an `index` argument to specify
 * which plugin to update
 *
 * Memory for this plugin will be reset upon update
 */
bool extism_plugin_update_with_functions(struct ExtismContext *ctx,
                                         ExtismPlugin index,
                                         const uint8_t *wasm,
                                         ExtismSize wasm_size,
                                         const struct ExtismFunction *const *functions,
                                         uint32_t nfunctions,
                                         bool with_wasi);

/**
 * Remove a plugin from the registry and free associated memory
 */
void extism_plugin_free(struct ExtismContext *ctx, ExtismPlugin plugin);

/**
 * Remove all plugins from the registry
 */
void extism_context_reset(struct ExtismContext *ctx);

/**
 * Update plugin config values, this will merge with the existing values
 */
bool extism_plugin_config(struct ExtismContext *ctx,
                          ExtismPlugin plugin,
                          const uint8_t *json,
                          ExtismSize json_size);

/**
 * Returns true if `func_name` exists
 */
bool extism_plugin_function_exists(struct ExtismContext *ctx,
                                   ExtismPlugin plugin,
                                   const char *func_name);

/**
 * Call a function
 *
 * `func_name`: is the function to call
 * `data`: is the input data
 * `data_len`: is the length of `data`
 */
int32_t extism_plugin_call(struct ExtismContext *ctx,
                           ExtismPlugin plugin_id,
                           const char *func_name,
                           const uint8_t *data,
                           ExtismSize data_len);

/**
 * Get the error associated with a `Context` or `Plugin`, if `plugin` is `-1` then the context
 * error will be returned
 */
const char *extism_error(struct ExtismContext *ctx, ExtismPlugin plugin);

/**
 * Get the length of a plugin's output data
 */
ExtismSize extism_plugin_output_length(struct ExtismContext *ctx, ExtismPlugin plugin);

/**
 * Get the length of a plugin's output data
 */
const uint8_t *extism_plugin_output_data(struct ExtismContext *ctx, ExtismPlugin plugin);

/**
 * Set log file and level
 */
bool extism_log_file(const char *filename, const char *log_level);

/**
 * Get the Extism version string
 */
const char *extism_version(void);
