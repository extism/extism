#include "../runtime/extism.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

void hello_world(ExtismCurrentPlugin *plugin, const ExtismVal *inputs,
                 uint64_t n_inputs, ExtismVal *outputs, uint64_t n_outputs,
                 void *data) {
  puts("Hello from C!");
  puts(data);

  ExtismSize ptr_offs = inputs[0].v.i64;

  uint8_t *buf = extism_current_plugin_memory(plugin) + ptr_offs;
  uint64_t length = extism_current_plugin_memory_length(plugin, ptr_offs);
  fwrite(buf, length, 1, stdout);
  fputc('\n', stdout);
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
  uint8_t *data = read_file("../wasm/code-functions.wasm", &len);
  ExtismValType inputs[] = {I64};
  ExtismValType outputs[] = {I64};
  ExtismFunction *f = extism_function_new("hello_world", inputs, 1, outputs, 1,
                                          hello_world, "Hello, again!", NULL);
  ExtismPlugin plugin =
      extism_plugin_new(ctx, data, len, (const ExtismFunction **)&f, 1, true);
  free(data);
  if (plugin < 0) {
    puts(extism_error(ctx, -1));
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
