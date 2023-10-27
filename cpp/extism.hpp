#pragma once

#include <cstring>
#include <functional>
#include <map>
#include <memory>
#include <stdexcept>
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

template <typename T> class ManifestKey {
  bool is_set = false;

public:
  T value;
  ManifestKey(T x, bool is_set = false) : is_set(is_set) { value = x; }

  void set(T x) {
    value = x;
    is_set = true;
  }

  bool empty() const { return is_set == false; }
};

class Wasm {
  std::string _path;
  std::string _url;
  // TODO: add base64 encoded raw data
  ManifestKey<std::string> _hash =
      ManifestKey<std::string>(std::string(), false);

public:
  // Create Wasm pointing to a path
  static Wasm path(std::string s, std::string hash = std::string()) {
    Wasm w;
    w._path = s;
    if (!hash.empty()) {
      w._hash.set(hash);
    }
    return w;
  }

  // Create Wasm pointing to a URL
  static Wasm url(std::string s, std::string hash = std::string()) {
    Wasm w;
    w._url = s;
    if (!hash.empty()) {
      w._hash.set(hash);
    }
    return w;
  }

#ifndef EXTISM_NO_JSON
  Json::Value json() const {
    Json::Value doc;

    if (!this->_path.empty()) {
      doc["path"] = this->_path;
    } else if (!this->_url.empty()) {
      doc["url"] = this->_url;
    }

    if (!this->_hash.empty()) {
      doc["hash"] = this->_hash.value;
    }

    return doc;
  }
#endif
};

class Manifest {
public:
  Config config;
  std::vector<Wasm> wasm;
  ManifestKey<std::vector<std::string>> allowed_hosts;
  ManifestKey<std::map<std::string, std::string>> allowed_paths;
  ManifestKey<uint64_t> timeout_ms;

  // Empty manifest
  Manifest()
      : timeout_ms(0, false), allowed_hosts(std::vector<std::string>(), false),
        allowed_paths(std::map<std::string, std::string>(), false) {}

  // Create manifest with a single Wasm from a path
  static Manifest path(std::string s, std::string hash = std::string()) {
    Manifest m;
    m.add_wasm_path(s, hash);
    return m;
  }

  // Create manifest with a single Wasm from a URL
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

      for (auto s : this->allowed_hosts.value) {
        h.append(s);
      }
      doc["allowed_hosts"] = h;
    }

    if (!this->allowed_paths.empty()) {
      Json::Value h;
      for (auto k : this->allowed_paths.value) {
        h[k.first] = k.second;
      }
      doc["allowed_paths"] = h;
    }

    if (!this->timeout_ms.empty()) {
      doc["timeout_ms"] = Json::Value(this->timeout_ms.value);
    }

    Json::FastWriter writer;
    return writer.write(doc);
  }
#endif

  // Add Wasm from path
  void add_wasm_path(std::string s, std::string hash = std::string()) {
    Wasm w = Wasm::path(s, hash);
    this->wasm.push_back(w);
  }

  // Add Wasm from URL
  void add_wasm_url(std::string u, std::string hash = std::string()) {
    Wasm w = Wasm::url(u, hash);
    this->wasm.push_back(w);
  }

  // Add host to allowed hosts
  void allow_host(std::string host) {
    if (this->allowed_hosts.empty()) {
      this->allowed_hosts.set(std::vector<std::string>{});
    }
    this->allowed_hosts.value.push_back(host);
  }

  // Add path to allowed paths
  void allow_path(std::string src, std::string dest = std::string()) {
    if (this->allowed_paths.empty()) {
      this->allowed_paths.set(std::map<std::string, std::string>{});
    }

    if (dest.empty()) {
      dest = src;
    }
    this->allowed_paths.value[src] = dest;
  }

  // Set timeout
  void set_timeout_ms(uint64_t ms) { this->timeout_ms = ms; }

  // Set config key/value
  void set_config(std::string k, std::string v) { this->config[k] = v; }
};

class Error : public std::runtime_error {
public:
  Error(std::string msg) : std::runtime_error(msg) {}
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
typedef uint64_t MemoryHandle;

class CurrentPlugin {
  ExtismCurrentPlugin *pointer;

public:
  CurrentPlugin(ExtismCurrentPlugin *p) : pointer(p) {}

  uint8_t *memory() { return extism_current_plugin_memory(this->pointer); }
  uint8_t *memory(MemoryHandle offs) { return this->memory() + offs; }

  ExtismSize memoryLength(MemoryHandle offs) {
    return extism_current_plugin_memory_length(this->pointer, offs);
  }

  MemoryHandle alloc(ExtismSize size) {
    return extism_current_plugin_memory_alloc(this->pointer, size);
  }

  void free(MemoryHandle handle) {
    extism_current_plugin_memory_free(this->pointer, handle);
  }

  void returnString(Val &output, const std::string &s) {
    this->returnBytes(output, (const uint8_t *)s.c_str(), s.size());
  }

  void returnBytes(Val &output, const uint8_t *bytes, size_t len) {
    auto offs = this->alloc(len);
    memcpy(this->memory() + offs, bytes, len);
    output.v.i64 = offs;
  }

  uint8_t *inputBytes(Val &inp, size_t *length = nullptr) {
    if (inp.t != ValType::I64) {
      return nullptr;
    }
    if (length != nullptr) {
      *length = this->memoryLength(inp.v.i64);
    }
    return this->memory() + inp.v.i64;
  }

  std::string inputString(Val &inp) {
    size_t length = 0;
    char *buf = (char *)this->inputBytes(inp, &length);
    return std::string(buf, length);
  }
};

typedef std::function<void(CurrentPlugin, const std::vector<Val> &,
                           std::vector<Val> &, void *user_data)>
    FunctionType;

struct UserData {
  FunctionType func;
  void *user_data = NULL;
  std::function<void(void *)> free_user_data;
};

static void function_callback(ExtismCurrentPlugin *plugin,
                              const ExtismVal *inputs, ExtismSize n_inputs,
                              ExtismVal *outputs, ExtismSize n_outputs,
                              void *user_data) {
  UserData *data = (UserData *)user_data;
  const std::vector<Val> inp(inputs, inputs + n_inputs);
  std::vector<Val> outp(outputs, outputs + n_outputs);
  data->func(CurrentPlugin(plugin), inp, outp, data->user_data);

  for (ExtismSize i = 0; i < n_outputs; i++) {
    outputs[i] = outp[i];
  }
}

static void free_user_data(void *user_data) {
  UserData *data = (UserData *)user_data;
  if (data->user_data != NULL && data->free_user_data != NULL) {
    data->free_user_data(data->user_data);
  }
}

class Function {
  std::shared_ptr<ExtismFunction> func;
  std::string name;
  UserData user_data;

public:
  Function(std::string name, const std::vector<ValType> inputs,
           const std::vector<ValType> outputs, FunctionType f,
           void *user_data = NULL, std::function<void(void *)> free = nullptr)
      : name(name) {
    this->user_data.func = f;
    this->user_data.user_data = user_data;
    this->user_data.free_user_data = free;
    auto ptr = extism_function_new(
        this->name.c_str(), inputs.data(), inputs.size(), outputs.data(),
        outputs.size(), function_callback, &this->user_data, free_user_data);
    this->func = std::shared_ptr<ExtismFunction>(ptr, extism_function_free);
  }

  void setNamespace(std::string s) {
    extism_function_set_namespace(this->func.get(), s.c_str());
  }

  Function(const Function &f) { this->func = f.func; }

  ExtismFunction *get() { return this->func.get(); }
};

class CancelHandle {
  const ExtismCancelHandle *handle;

public:
  CancelHandle(const ExtismCancelHandle *x) : handle(x){};
  bool cancel() { return extism_plugin_cancel(this->handle); }
};

class Plugin {
  std::vector<Function> functions;

public:
  ExtismPlugin *plugin;
  // Create a new plugin
  Plugin(const uint8_t *wasm, ExtismSize length, bool with_wasi = false,
         std::vector<Function> functions = std::vector<Function>())
      : functions(functions) {
    std::vector<const ExtismFunction *> ptrs;
    for (auto i : this->functions) {
      ptrs.push_back(i.get());
    }

    char *errmsg = nullptr;
    this->plugin = extism_plugin_new(wasm, length, ptrs.data(), ptrs.size(),
                                     with_wasi, &errmsg);
    if (this->plugin == nullptr) {
      std::string s(errmsg);
      extism_plugin_new_error_free(errmsg);
      throw Error(s);
    }
  }

  Plugin(const std::string &str, bool with_wasi = false,
         std::vector<Function> functions = {})
      : Plugin((const uint8_t *)str.c_str(), str.size(), with_wasi, functions) {
  }

  Plugin(const std::vector<uint8_t> &data, bool with_wasi = false,
         std::vector<Function> functions = {})
      : Plugin(data.data(), data.size(), with_wasi, functions) {}

  CancelHandle cancelHandle() {
    return CancelHandle(extism_plugin_cancel_handle(this->plugin));
  }

#ifndef EXTISM_NO_JSON
  // Create a new plugin from Manifest
  Plugin(const Manifest &manifest, bool with_wasi = false,
         std::vector<Function> functions = {})
      : Plugin(manifest.json().c_str(), with_wasi, functions) {}

  ~Plugin() {
    extism_plugin_free(this->plugin);
    this->plugin = nullptr;
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
    bool b = extism_plugin_config(this->plugin, (const uint8_t *)json, length);
    if (!b) {
      const char *err = extism_plugin_error(this->plugin);
      throw Error(err == nullptr ? "Unable to update plugin config" : err);
    }
  }

  void config(const std::string &json) {
    this->config(json.c_str(), json.size());
  }

  // Call a plugin
  Buffer call(const std::string &func, const uint8_t *input,
              ExtismSize input_length) const {
    int32_t rc =
        extism_plugin_call(this->plugin, func.c_str(), input, input_length);
    if (rc != 0) {
      const char *error = extism_plugin_error(this->plugin);
      if (error == nullptr) {
        throw Error("extism_call failed");
      }

      throw Error(error);
    }

    ExtismSize length = extism_plugin_output_length(this->plugin);
    const uint8_t *ptr = extism_plugin_output_data(this->plugin);
    return Buffer(ptr, length);
  }

  // Call a plugin function with std::vector<uint8_t> input
  Buffer call(const std::string &func,
              const std::vector<uint8_t> &input) const {
    return this->call(func, input.data(), input.size());
  }

  // Call a plugin function with string input
  Buffer call(const std::string &func,
              const std::string &input = std::string()) const {
    return this->call(func, (const uint8_t *)input.c_str(), input.size());
  }

  // Returns true if the specified function exists
  bool functionExists(const std::string &func) const {
    return extism_plugin_function_exists(this->plugin, func.c_str());
  }
};

// Set global log file for plugins
inline bool setLogFile(const char *filename, const char *level) {
  return extism_log_file(filename, level);
}

// Get libextism version
inline std::string version() { return std::string(extism_version()); }
} // namespace extism
