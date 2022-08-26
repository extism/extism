import ffi from 'ffi-napi';
import path from 'path';
import url from 'url';

const __dirname = url.fileURLToPath(new URL('.', import.meta.url));

var lib = ffi.Library(
  'libextism',
  {
    extism_plugin_register: ['int32', ['string', 'uint64', 'bool']],
    extism_error: ['char*', ['int32']],
    extism_call: ['int32', ['int32', 'string', 'string', 'uint64']],
    extism_output_length: ['uint64', ['int32']],
    extism_output_get: ['void', ['int32', 'char*', 'uint64']],
    extism_log_file: ['bool', ['string', 'char*']],
    extism_plugin_config: ['void', ['int32', 'char*', 'uint64']],
  }
)

export function set_log_file(filename, level = null) {
  lib.extism_log_file(filename, level)
}

export class Plugin {
  constructor(data, wasi = false, config = null) {
    if (typeof data === "object" && data.wasm) {
      data = JSON.stringify(data);
    }
    let plugin = lib.extism_plugin_register(data, data.length, wasi);
    if (plugin < 0) {
      throw "Unable to load plugin";
    }
    this.id = plugin;

    if (config != null) {
      let s = JSON.stringify(config);
      lib.extism_plugin_config(this.id, s, s.length);
    }
  }

  call(name, input) {
    var rc = lib.extism_call(this.id, name, input, input.length);
    if (rc != 0) {
      var err = lib.extism_error(this.id);
      if (err.length == 0) {
        throw "extism_call failed";
      }
      throw err.toString();
    }

    var out_len = lib.extism_output_length(this.id);
    var buf = new Buffer.alloc(out_len);
    lib.extism_output_get(this.id, buf, out_len);
    return buf;
  }
}

