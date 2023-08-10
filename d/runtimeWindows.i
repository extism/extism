// Hand-crafted from runtime.i on Windows to compile with ldc2-1.28.1-windows-x64
// TODO: Report issues with ImportC upstream to D maintainers
#line 1 "runtime/extism.h"
#pragma once

    typedef unsigned int     size_t;
    typedef int              ptrdiff_t;
    typedef int              intptr_t;
    typedef _Bool __vcrt_bool;
    typedef unsigned short wchar_t;

typedef signed char        int8_t;
typedef short              int16_t;
typedef int                int32_t;
typedef long long          int64_t;
typedef unsigned char      uint8_t;
typedef unsigned short     uint16_t;
typedef unsigned int       uint32_t;
typedef unsigned long long uint64_t;

typedef signed char        int_least8_t;
typedef short              int_least16_t;
typedef int                int_least32_t;
typedef long long          int_least64_t;
typedef unsigned char      uint_least8_t;
typedef unsigned short     uint_least16_t;
typedef unsigned int       uint_least32_t;
typedef unsigned long long uint_least64_t;

typedef signed char        int_fast8_t;
typedef int                int_fast16_t;
typedef int                int_fast32_t;
typedef long long          int_fast64_t;
typedef unsigned char      uint_fast8_t;
typedef unsigned int       uint_fast16_t;
typedef unsigned int       uint_fast32_t;
typedef unsigned long long uint_fast64_t;

typedef long long          intmax_t;
typedef unsigned long long uintmax_t;


typedef enum {
  I32,
  I64,
  F32,
  F64,
  V128,
  FuncRef,
  ExternRef,
} ExtismValType;

typedef struct ExtismContext ExtismContext;
typedef struct ExtismCancelHandle ExtismCancelHandle;
typedef struct ExtismFunction ExtismFunction;
typedef struct ExtismCurrentPlugin ExtismCurrentPlugin;

typedef uint64_t ExtismSize;

typedef union {
  int32_t i32;
  int64_t i64;
  float f32;
  double f64;
} ExtismValUnion;

typedef struct {
  ExtismValType t;
  ExtismValUnion v;
} ExtismVal;

typedef void (*ExtismFunctionType)(ExtismCurrentPlugin *plugin,
                                   const ExtismVal *inputs,
                                   ExtismSize n_inputs,
                                   ExtismVal *outputs,
                                   ExtismSize n_outputs,
                                   void *data);

typedef int32_t ExtismPlugin;

ExtismContext *extism_context_new(void);
void extism_context_free(ExtismContext *ctx);

uint8_t *extism_current_plugin_memory(ExtismCurrentPlugin *plugin);
uint64_t extism_current_plugin_memory_alloc(ExtismCurrentPlugin *plugin, ExtismSize n);
ExtismSize extism_current_plugin_memory_length(ExtismCurrentPlugin *plugin, ExtismSize n);
void extism_current_plugin_memory_free(ExtismCurrentPlugin *plugin, uint64_t ptr);

ExtismFunction *extism_function_new(const char *name,
                                    const ExtismValType *inputs,
                                    ExtismSize n_inputs,
                                    const ExtismValType *outputs,
                                    ExtismSize n_outputs,
                                    ExtismFunctionType func,
                                    void *user_data,
                                    void (*free_user_data)(void *_));
void extism_function_set_namespace(ExtismFunction *ptr, const char *namespace_);
void extism_function_free(ExtismFunction *ptr);

ExtismPlugin extism_plugin_new(ExtismContext *ctx,
                               const uint8_t *wasm,
                               ExtismSize wasm_size,
                               const ExtismFunction **functions,
                               ExtismSize n_functions,
                               _Bool with_wasi);
_Bool extism_plugin_update(ExtismContext *ctx,
                          ExtismPlugin index,
                          const uint8_t *wasm,
                          ExtismSize wasm_size,
                          const ExtismFunction **functions,
                          ExtismSize nfunctions,
                          _Bool with_wasi);
void extism_plugin_free(ExtismContext *ctx, ExtismPlugin plugin);
const ExtismCancelHandle *extism_plugin_cancel_handle(ExtismContext *ctx, ExtismPlugin plugin);
_Bool extism_plugin_cancel(const ExtismCancelHandle *handle);
void extism_context_reset(ExtismContext *ctx);
_Bool extism_plugin_config(ExtismContext *ctx,
                          ExtismPlugin plugin,
                          const uint8_t *json,
                          ExtismSize json_size);
_Bool extism_plugin_function_exists(ExtismContext *ctx, ExtismPlugin plugin, const char *func_name);
int32_t extism_plugin_call(ExtismContext *ctx,
                           ExtismPlugin plugin_id,
                           const char *func_name,
                           const uint8_t *data,
                           ExtismSize data_len);

ExtismSize extism_plugin_output_length(ExtismContext *ctx, ExtismPlugin plugin);
const uint8_t *extism_plugin_output_data(ExtismContext *ctx, ExtismPlugin plugin);

const char *extism_error(ExtismContext *ctx, ExtismPlugin plugin);

_Bool extism_log_file(const char *filename, const char *log_level);

const char *extism_version(void);
