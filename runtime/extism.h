#pragma once

#include <stdint.h>
#include <stdbool.h>

typedef int32_t ExtismPlugin;

typedef uint64_t ExtismSize;

ExtismPlugin extism_plugin_register(const uint8_t *wasm, ExtismSize wasm_size, bool with_wasi);

bool extism_plugin_update(ExtismPlugin index,
                          const uint8_t *wasm,
                          ExtismSize wasm_size,
                          bool with_wasi);

bool extism_plugin_config(ExtismPlugin plugin, const uint8_t *json, ExtismSize json_size);

bool extism_function_exists(ExtismPlugin plugin, const char *func_name);

int32_t extism_call(ExtismPlugin plugin_id,
                    const char *func_name,
                    const uint8_t *data,
                    ExtismSize data_len);

const char *extism_error(ExtismPlugin plugin);

ExtismSize extism_output_length(ExtismPlugin plugin);

const uint8_t *extism_output_get(ExtismPlugin plugin);

bool extism_log_file(const char *filename, const char *log_level);
