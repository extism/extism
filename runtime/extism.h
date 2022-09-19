#pragma once

#include <stdint.h>
#include <stdbool.h>

typedef struct ExtismContext ExtismContext;

typedef int32_t ExtismPlugin;

typedef uint64_t ExtismSize;

struct ExtismContext *extism_context_new(void);

void extism_context_free(struct ExtismContext *ctx);

ExtismPlugin extism_plugin_new(struct ExtismContext *ctx,
                               const uint8_t *wasm,
                               ExtismSize wasm_size,
                               bool with_wasi);

bool extism_plugin_update(struct ExtismContext *ctx,
                          ExtismPlugin index,
                          const uint8_t *wasm,
                          ExtismSize wasm_size,
                          bool with_wasi);

void extism_plugin_free(struct ExtismContext *ctx, ExtismPlugin plugin);

void extism_context_reset(struct ExtismContext *ctx);

bool extism_plugin_config(struct ExtismContext *ctx,
                          ExtismPlugin plugin,
                          const uint8_t *json,
                          ExtismSize json_size);

bool extism_plugin_function_exists(struct ExtismContext *ctx,
                                   ExtismPlugin plugin,
                                   const char *func_name);

int32_t extism_plugin_call(struct ExtismContext *ctx,
                           ExtismPlugin plugin_id,
                           const char *func_name,
                           const uint8_t *data,
                           ExtismSize data_len);

const char *extism_error(struct ExtismContext *ctx, ExtismPlugin plugin);

ExtismSize extism_plugin_output_length(struct ExtismContext *ctx, ExtismPlugin plugin);

const uint8_t *extism_plugin_output_data(struct ExtismContext *ctx, ExtismPlugin plugin);

bool extism_log_file(const char *filename, const char *log_level);
