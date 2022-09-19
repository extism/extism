#pragma once

#include <memory>
#include <string>
#include <vector>

extern "C" {
#include <extism.h>
}

namespace extism {
class Error : public std::exception {
private:
  std::string message;

public:
  Error(std::string msg) : message(msg) {}
  const char *what() { return message.c_str(); }
};

class Buffer {
public:
  Buffer(const uint8_t *ptr, ExtismSize len) : data(ptr), length(len) {}
  const uint8_t *data;
  ExtismSize length;

  operator std::string() { return std::string((const char *)data, length); }
  operator std::vector<uint8_t>() {
    return std::vector<uint8_t>(data, data + length);
  }
};

class Plugin {
  std::shared_ptr<ExtismContext> context;
  ExtismPlugin plugin;

public:
  Plugin(std::shared_ptr<ExtismContext> ctx, const uint8_t *wasm,
         ExtismSize length, bool with_wasi = false) {
    this->plugin = extism_plugin_new(ctx.get(), wasm, length, with_wasi);
    if (this->plugin < 0) {
      const char *err = extism_error(ctx.get(), -1);
      throw Error(err == nullptr ? "Unable to load plugin" : err);
    }
    this->context = ctx;
  }

  ~Plugin() {
    extism_plugin_free(this->context.get(), this->plugin);
    this->plugin = -1;
  }

  void update(const uint8_t *wasm, size_t length, bool with_wasi = false) {
    bool b = extism_plugin_update(this->context.get(), this->plugin, wasm,
                                  length, with_wasi);
    if (!b) {
      const char *err = extism_error(this->context.get(), -1);
      throw Error(err == nullptr ? "Unable to update plugin" : err);
    }
  }

  Buffer call(const std::string &func, const uint8_t *input,
              ExtismSize input_length) {

    int32_t rc = extism_plugin_call(this->context.get(), this->plugin,
                                    func.c_str(), input, input_length);
    if (rc != 0) {
      const char *error = extism_error(this->context.get(), this->plugin);
      if (error == nullptr) {
        throw Error("extism_call failed");
      }

      throw Error(error);
    }

    ExtismSize length =
        extism_plugin_output_length(this->context.get(), this->plugin);
    const uint8_t *ptr =
        extism_plugin_output_data(this->context.get(), this->plugin);
    return Buffer(ptr, length);
  }

  Buffer call(const std::string &func, const std::vector<uint8_t> &input) {
    return this->call(func, input.data(), input.size());
  }

  Buffer call(const std::string &func, const std::string &input) {
    return this->call(func, (const uint8_t *)input.c_str(), input.size());
  }
};

class Context {
public:
  std::shared_ptr<ExtismContext> pointer;
  Context() {
    this->pointer = std::shared_ptr<ExtismContext>(extism_context_new(),
                                                   extism_context_free);
  }

  Plugin plugin(const uint8_t *wasm, size_t length, bool with_wasi = false) {
    return Plugin(this->pointer, wasm, length, with_wasi);
  }

  Plugin plugin(const std::string &str, bool with_wasi = false) {
    return Plugin(this->pointer, (const uint8_t *)str.c_str(), str.size(),
                  with_wasi);
  }

  Plugin plugin(const std::vector<uint8_t> &data, bool with_wasi = false) {
    return Plugin(this->pointer, data.data(), data.size(), with_wasi);
  }
};

} // namespace extism
