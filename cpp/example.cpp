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
  Plugin plugin(wasm);

  if (argc < 2) {
    std::cout << "Not enough arguments" << std::endl;
    return 1;
  }

  auto input = std::vector<uint8_t>((uint8_t *)argv[1],
                                    (uint8_t *)argv[1] + strlen(argv[1]));
  auto output = plugin.call("count_vowels", input);
  std::string str(output.begin(), output.end());
  std::cout << str << std::endl;
  return 0;
}
