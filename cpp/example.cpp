#include "extism.hpp"

#include <cstring>
#include <fstream>
#include <iostream>

using namespace extism;

std::vector<uint8_t> read(const char *filename) {
  std::ifstream file(filename, std::ios::binary);
  return std::vector<uint8_t>((std::istreambuf_iterator<char>(file)),
                              std::istreambuf_iterator<char>());
}

int main(int argc, char *argv[]) {
  auto wasm = read("../wasm/code.wasm");
  Context context = Context();

  Plugin plugin = context.plugin(wasm);

  const char *input = argc > 1 ? argv[1] : "this is a test";
  ExtismSize length = strlen(input);

  extism::Buffer output = plugin.call("count_vowels", (uint8_t *)input, length);
  std::cout << (char *)output.data << std::endl;
  return 0;
}
