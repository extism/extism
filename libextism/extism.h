#pragma once

#include <stdint.h>
#include <stdbool.h>

/**
 * A `Context` is used to store and manage plugins
 */
typedef struct ExtismContext ExtismContext;

typedef int32_t ExtismPlugin;

typedef uint64_t ExtismSize;

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
