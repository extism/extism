#pragma once

#include <map>
#include <memory>
#include <string>
#include <vector>

#ifndef EXTISM_NO_JSON
#if __has_include(<jsoncpp/json/json.h>)
#include <jsoncpp/json/json.h>
#else
#include <json/json.h>
#endif
#endif // EXTISM_NO_JSON

extern "C" {
#include <extism.h>
}

namespace extism {

typedef std::map<std::string, std::string> Config;
class Wasm {
public:
  std::string path;
  std::string url;
  // TODO: add base64 encoded raw data
  std::string hash;

#ifndef EXTISM_NO_JSON
  Json::Value json() const {
    Json::Value doc;

    if (!this->path.empty()) {
      doc["path"] = this->path;
    }

    if (!this->url.empty()) {
      doc["url"] = this->url;
    }

    if (!this->hash.empty()) {
      doc["hash"] = this->hash;
    }

    return doc;
  }
#endif
};

class Manifest {
public:
  Config config;
  std::vector<Wasm> wasm;
  std::vector<std::string> allowed_hosts;
  std::map<std::string, std::string> allowed_paths;
  uint64_t timeout_ms;

  Manifest() : timeout_ms(30000) {}

  static Manifest path(std::string s, std::string hash = std::string()) {
    Manifest m;
    m.add_wasm_path(s, hash);
    return m;
  }

  static Manifest url(std::string s, std::string hash = std::string()) {
    Manifest m;
    m.add_wasm_url(s, hash);
    return m;
  }

#ifndef EXTISM_NO_JSON
  std::string json() const {
    Json::Value doc;
    Json::Value wasm;
    for (auto w : this->wasm) {
      wasm.append(w.json());
    }

    doc["wasm"] = wasm;

    if (!this->config.empty()) {
      Json::Value conf;

      for (auto k : this->config) {
        conf[k.first] = k.second;
      }
      doc["config"] = conf;
    }

    if (!this->allowed_hosts.empty()) {
      Json::Value h;

      for (auto s : this->allowed_hosts) {
        h.append(s);
      }
      doc["allowed_hosts"] = h;
    }

    if (!this->allowed_paths.empty()) {
      Json::Value h;
      for (auto k : this->allowed_paths) {
        h[k.first] = k.second;
      }
      doc["allowed_paths"] = h;
    }

    doc["timeout_ms"] = Json::Value(this->timeout_ms);

    Json::FastWriter writer;
    return writer.write(doc);
  }
#endif

  void add_wasm_path(std::string s, std::string hash = std::string()) {
    Wasm w;
    w.path = s;
    w.hash = hash;
    this->wasm.push_back(w);
  }

  void add_wasm_url(std::string u, std::string hash = std::string()) {
    Wasm w;
    w.url = u;
    w.hash = hash;
    this->wasm.push_back(w);
  }

  void allow_host(std::string host) { this->allowed_hosts.push_back(host); }

  void allow_path(std::string src, std::string dest = std::string()) {
    if (dest.empty()) {
      dest = src;
    }
    this->allowed_paths[src] = dest;
  }

  void set_timeout_ms(uint64_t ms) { this->timeout_ms = ms; }

  void set_config(std::string k, std::string v) { this->config[k] = v; }
};

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

  std::string string() { return (std::string)(*this); }

  std::vector<uint8_t> vector() { return (std::vector<uint8_t>)(*this); }

  operator std::string() { return std::string((const char *)data, length); }
  operator std::vector<uint8_t>() {
    return std::vector<uint8_t>(data, data + length);
  }
};

typedef ExtismValType ValType;
typedef ExtismValUnion ValUnion;
typedef ExtismVal Val;

class CurrentPlugin {
  ExtismCurrentPlugin *pointer;

public:
  CurrentPlugin(ExtismCurrentPlugin *p) : pointer(p) {}

  uint8_t *memory() { return extism_current_plugin_memory(this->pointer); }
  ExtismSize memory_length(uint64_t offs) {
    return extism_current_plugin_memory_length(this->pointer, offs);
  }

  uint64_t alloc(ExtismSize size) {
    return extism_current_plugin_memory_alloc(this->pointer, size);
  }

  void free(uint64_t offs) {
    extism_current_plugin_memory_free(this->pointer, offs);
  }
};

class Function {
  std::shared_ptr<ExtismFunction> func;
  std::string name;

public:
  Function(std::string name, std::vector<ValType> inputs,
           std::vector<ValType> outputs, ExtismFunctionType f,
           void *userData = NULL, void (*freeUserData)(void *) = NULL)
      : name(name) {
    auto ptr = extism_function_new(this->name.c_str(), inputs.data(),
                                   inputs.size(), outputs.data(),
                                   outputs.size(), f, userData, freeUserData);
    this->func = std::shared_ptr<ExtismFunction>(ptr, extism_function_free);
  }

  ExtismFunction *get() { return this->func.get(); }
};

class Plugin {
  std::shared_ptr<ExtismContext> context;
  ExtismPlugin plugin;
  std::vector<Function> functions;

public:
  Plugin(std::shared_ptr<ExtismContext> ctx, const uint8_t *wasm,
         ExtismSize length, bool with_wasi = false,
         std::vector<Function> functions = std::vector<Function>())
      : functions(functions) {
    std::vector<ExtismFunction *> ptrs;
    for (auto i : this->functions) {
      ptrs.push_back(i.get());
    }
    this->plugin = extism_plugin_new_with_functions(
        ctx.get(), wasm, length, ptrs.data(), ptrs.size(), with_wasi);
    if (this->plugin < 0) {
      const char *err = extism_error(ctx.get(), -1);
      throw Error(err == nullptr ? "Unable to load plugin" : err);
    }
    this->context = ctx;
  }

#ifndef EXTISM_NO_JSON
  Plugin(std::shared_ptr<ExtismContext> ctx, const Manifest &manifest,
         bool with_wasi = false, std::vector<Function> functions = {}) {
    std::vector<ExtismFunction *> ptrs;
    for (auto i : this->functions) {
      ptrs.push_back(i.get());
    }

    auto buffer = manifest.json();
    this->plugin = extism_plugin_new_with_functions(
        ctx.get(), (const uint8_t *)buffer.c_str(), buffer.size(), ptrs.data(),
        ptrs.size(), with_wasi);
    if (this->plugin < 0) {
      const char *err = extism_error(ctx.get(), -1);
      throw Error(err == nullptr ? "Unable to load plugin from manifest" : err);
    }
    this->context = ctx;
  }
#endif

  ~Plugin() {
    extism_plugin_free(this->context.get(), this->plugin);
    this->plugin = -1;
  }

  ExtismPlugin id() const { return this->plugin; }

  ExtismContext *get_context() const { return this->context.get(); }

  void update(const uint8_t *wasm, size_t length, bool with_wasi = false,
              std::vector<Function> functions = {}) {
    this->functions = functions;
    std::vector<ExtismFunction *> ptrs;
    for (auto i : this->functions) {
      ptrs.push_back(i.get());
    }
    bool b = extism_plugin_update_with_functions(
        this->context.get(), this->plugin, wasm, length, ptrs.data(),
        ptrs.size(), with_wasi);
    if (!b) {
      const char *err = extism_error(this->context.get(), -1);
      throw Error(err == nullptr ? "Unable to update plugin" : err);
    }
  }

#ifndef EXTISM_NO_JSON
  void update(const Manifest &manifest, bool with_wasi = false,
              std::vector<Function> functions = {}) {
    this->functions = functions;
    std::vector<ExtismFunction *> ptrs;
    for (auto i : this->functions) {
      ptrs.push_back(i.get());
    }
    auto buffer = manifest.json();
    bool b = extism_plugin_update_with_functions(
        this->context.get(), this->plugin, (const uint8_t *)buffer.c_str(),
        buffer.size(), ptrs.data(), ptrs.size(), with_wasi);
    if (!b) {
      const char *err = extism_error(this->context.get(), -1);
      throw Error(err == nullptr ? "Unable to update plugin" : err);
    }
  }

  void config(const Config &data) {
    Json::Value conf;

    for (auto k : data) {
      conf[k.first] = k.second;
    }

    Json::FastWriter writer;
    auto s = writer.write(conf);
    this->config(s);
  }
#endif

  void config(const char *json, size_t length) {
    bool b = extism_plugin_config(this->context.get(), this->plugin,
                                  (const uint8_t *)json, length);
    if (!b) {
      const char *err = extism_error(this->context.get(), this->plugin);
      throw Error(err == nullptr ? "Unable to update plugin config" : err);
    }
  }

  void config(const std::string &json) {
    this->config(json.c_str(), json.size());
  }

  Buffer call(const std::string &func, const uint8_t *input,
              ExtismSize input_length) const {
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

  Buffer call(const std::string &func,
              const std::vector<uint8_t> &input) const {
    return this->call(func, input.data(), input.size());
  }

  Buffer call(const std::string &func, const std::string &input) const {
    return this->call(func, (const uint8_t *)input.c_str(), input.size());
  }

  bool function_exists(const std::string &func) const {
    return extism_plugin_function_exists(this->context.get(), this->plugin,
                                         func.c_str());
  }
};

class Context {
public:
  std::shared_ptr<ExtismContext> pointer;

  Context() {
    this->pointer = std::shared_ptr<ExtismContext>(extism_context_new(),
                                                   extism_context_free);
  }

  Plugin plugin(const uint8_t *wasm, size_t length, bool with_wasi = false,
                std::vector<Function> functions = {}) const {
    return Plugin(this->pointer, wasm, length, with_wasi, functions);
  }

  Plugin plugin(const std::string &str, bool with_wasi = false,
                std::vector<Function> functions = {}) const {
    return Plugin(this->pointer, (const uint8_t *)str.c_str(), str.size(),
                  with_wasi, functions);
  }

  Plugin plugin(const std::vector<uint8_t> &data, bool with_wasi = false,
                std::vector<Function> functions = {}) const {
    return Plugin(this->pointer, data.data(), data.size(), with_wasi,
                  functions);
  }

#ifndef EXTISM_NO_JSON
  Plugin plugin(const Manifest &manifest, bool with_wasi = false,
                std::vector<Function> functions = {}) const {
    return Plugin(this->pointer, manifest, with_wasi, functions);
  }
#endif

  void reset() { extism_context_reset(this->pointer.get()); }
};

inline bool set_log_file(const char *filename, const char *level) {
  return extism_log_file(filename, level);
}

inline std::string version() { return std::string(extism_version()); }
} // namespace extism
