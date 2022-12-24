#define EXTISM_NO_JSON
#include "extism.hpp"

#include <cstring>
#include <fstream>
#include <iostream>

using namespace extism;

void testing_123(ExtismCurrentPlugin *plugin, const Val *inputs,
                 ExtismSize nInputs, Val *outputs, ExtismSize nOutputs,
                 void *user_data) {
  std::cout << "Hello from C++" << std::endl;
  std::cout << (const char *)user_data << std::endl;
  outputs[0].v = inputs[0].v;
}

std::vector<uint8_t> read(const char *filename) {
  std::ifstream file(filename, std::ios::binary);
  return std::vector<uint8_t>((std::istreambuf_iterator<char>(file)),
                              std::istreambuf_iterator<char>());
}

int main(int argc, char *argv[]) {
  auto wasm = read("../wasm/code-functions.wasm");
  Context context = Context();

  std::vector<Function> functions = {
      Function("testing_123", {ValType::I64}, {ValType::I64}, testing_123,
               (void *)"Hello again!"),
  };

  Plugin plugin = context.plugin(wasm, true, functions);

  const char *input = argc > 1 ? argv[1] : "this is a test";
  ExtismSize length = strlen(input);

  extism::Buffer output = plugin.call("count_vowels", (uint8_t *)input, length);
  std::cout << (char *)output.data << std::endl;
  return 0;
}
