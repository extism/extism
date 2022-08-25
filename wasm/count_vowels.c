#include "c-pdk/extism-pdk.h"

#include "printf.h"

int32_t count_vowels() {
  uint64_t offs = extism_input_offset();
  uint64_t length = extism_length(offs);

  if (offs == 0) {
    return 0;
  }

  char input[length];
  extism_load(offs, (uint8_t *)input, length);

  int64_t count = 0;
  for (int64_t i = 0; i < length; i++) {
    if (input[i] == 'a' || input[i] == 'e' || input[i] == 'i' ||
        input[i] == 'o' || input[i] == 'u' || input[i] == 'A' ||
        input[i] == 'E' || input[i] == 'I' || input[i] == 'O' ||
        input[i] == 'U') {
      count += 1;
    }
  }

  char out[128];
  int n = snprintf(out, 128, "{\"count\": %d}", count);

  uint64_t offs_ = extism_alloc(n);
  extism_store(offs_, (const uint8_t *)out, n);
  extism_output_set(offs_, n);

  return 0;
}
