#pragma once

#include <string>
#include <vector>

extern "C" {
#include "extism.h"
}

namespace extism {
class Error : public std::exception {
private:
  std::string message;

public:
  Error(std::string msg) : message(msg) {}
  const char *what() { return message.c_str(); }
};

class Plugin {
  ExtismPlugin plugin;

public:
  Plugin(const uint8_t *wasm, size_t length, bool with_wasi = false) {
    this->plugin = extism_plugin_register(wasm, length, with_wasi);
    if (this->plugin < 0) {
      throw Error("Unable to load plugin");
    }
  }

  Plugin(const std::string &s, bool with_wasi = false)
      : Plugin((const uint8_t *)s.c_str(), s.size(), with_wasi) {}
  Plugin(const std::vector<uint8_t> &s, bool with_wasi = false)
      : Plugin(s.data(), s.size(), with_wasi) {}

  std::vector<uint8_t> call(const std::string &func,
                            std::vector<uint8_t> input) {

    int32_t rc =
        extism_call(this->plugin, func.c_str(), input.data(), input.size());
    if (rc != 0) {
      const char *error = extism_error(this->plugin);
      if (error == nullptr) {
        throw Error("extism_call failed");
      }

      throw Error(error);
    }

    ExtismSize length = extism_output_length(this->plugin);
    const uint8_t *ptr = extism_output_get(this->plugin);
    std::vector<uint8_t> out = std::vector<uint8_t>(ptr, ptr + length);
    return out;
  }
};
} // namespace extism
