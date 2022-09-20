#include "../runtime/extism.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

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
  ExtismPlugin plugin = extism_plugin_new(ctx, data, len, false);
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
  extism_context_free(ctx);
  return 0;
}
