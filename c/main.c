#include "../runtime/extism.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

void testing_123(const struct ExtismVal *inputs, uint64_t n_inputs,
                 struct ExtismVal *outputs, uint64_t n_outputs, void *data) {
  puts("Hello from C!");
  puts((char *)data);
  outputs[0].v.i64 = inputs[0].v.i64;
}

uint8_t *read_file(const char *filename, size_t *len) {

  FILE *fp = fopen(filename, "rb");
  if (fp == NULL) {
    return NULL;
  }
  fseek(fp, 0, SEEK_END);
  size_t length = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  uint8_t *data = malloc(length);
  if (data == NULL) {
    fclose(fp);
    return NULL;
  }

  assert(fread(data, 1, length, fp) == length);
  fclose(fp);

  *len = length;
  return data;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fputs("Not enough arguments\n", stderr);
    exit(1);
  }

  ExtismContext *ctx = extism_context_new();

  size_t len = 0;
  uint8_t *data = read_file("../wasm/code.wasm", &len);
  ExtismValType inputs[] = {I64};
  ExtismValType outputs[] = {I64};
  ExtismFunction *f =
      extism_function_new("testing_123", inputs, 1, outputs, 1, testing_123,
                          (void *)"Hello, again!", NULL);
  const ExtismFunction *functions[] = {f};
  ExtismPlugin plugin =
      extism_plugin_new_with_functions(ctx, data, len, functions, 1, true);
  free(data);
  if (plugin < 0) {
    exit(1);
  }

  assert(extism_plugin_call(ctx, plugin, "count_vowels", (uint8_t *)argv[1],
                            strlen(argv[1])) == 0);
  ExtismSize out_len = extism_plugin_output_length(ctx, plugin);
  const uint8_t *output = extism_plugin_output_data(ctx, plugin);
  write(STDOUT_FILENO, output, out_len);
  write(STDOUT_FILENO, "\n", 1);

  extism_plugin_free(ctx, plugin);
  extism_function_free(f);
  extism_context_free(ctx);
  return 0;
}
