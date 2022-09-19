import ffi from 'ffi-napi';
import path from 'path';
import url from 'url';

const __dirname = url.fileURLToPath(new URL('.', import.meta.url));
let context = 'void*';
let _functions = {
  extism_context_new: [context, []],
  extism_context_free: ['void', [context]],
  extism_plugin_new: ['int32', [context, 'string', 'uint64', 'bool']],
  extism_plugin_update: ['bool', [context, 'int32', 'string', 'uint64', 'bool']],
  extism_error: ['char*', [context, 'int32']],
  extism_plugin_call: ['int32', [context, 'int32', 'string', 'string', 'uint64']],
  extism_plugin_output_length: ['uint64', [context, 'int32']],
  extism_plugin_output_data: ['uint8*', [context, 'int32']],
  extism_log_file: ['bool', ['string', 'char*']],
  extism_plugin_function_exists: ['bool', [context, 'int32', 'string']],
  extism_plugin_config: ['void', [context, 'int32', 'char*', 'uint64']],
  extism_plugin_free: ['void', [context, 'int32']],
  extism_context_reset: ['void', [context]],
};

function locate(paths) {
  for (var i = 0; i < paths.length; i++) {
    try {
      return ffi.Library(path.join(paths[i], 'libextism'), _functions);
    } catch (exn) {
      continue;
    }
  }

  throw 'Unable to locate libextism';
}

var searchPath = [
  __dirname,
  '/usr/local/lib',
  '/usr/lib',
  path.join(process.env.HOME, '.local', 'lib'),
];

if (process.env.EXTISM_PATH) {
  searchPath.unshift(path.join(process.env.EXTISM_PATH, 'lib'));
}

var lib = locate(searchPath);

// Set the log file and level
export function setLogFile(filename, level = null) {
  lib.extism_log_file(filename, level);
}

const pluginRegistry = new FinalizationRegistry(({ id, pointer }) => {
  lib.extism_plugin_free(pointer, id);
});


const contextRegistry = new FinalizationRegistry((pointer) => {
  lib.extism_context_free(pointer);
});

// Context manages plugins
export class Context {
  constructor() {
    this.pointer = lib.extism_context_new();

    contextRegistry.register(this, this.pointer, this);
  }

  // Create a new plugin, optionally enabling WASI
  plugin(data, wasi = false, config = null) {
    return new Plugin(this, data, wasi, config);
  }

  // Free a context, this should be called when it is
  // no longer needed
  free() {
    contextRegistry.unregister(this);

    lib.extism_context_free(this.pointer);
    this.pointer = null;
  }

  // Remove all registered plugins
  reset() {
    lib.extism_context_reset(this.pointer);
  }
}

// Plugin provides an interface for calling WASM functions
export class Plugin {
  constructor(ctx, data, wasi = false, config = null) {
    if (typeof data === 'object' && data.wasm) {
      data = JSON.stringify(data);
    }
    let plugin = lib.extism_plugin_new(ctx.pointer, data, data.length, wasi);
    if (plugin < 0) {
      var err = lib.extism_error(ctx.pointer, -1);
      if (err.length == 0) {
        throw "extism_context_plugin failed";
      }
      throw `Unable to load plugin: ${err.toString()}`;
    }
    this.id = plugin;
    this.ctx = ctx;
    pluginRegistry.register(this, { id: this.id, pointer: this.ctx.pointer }, this);

    if (config != null) {
      let s = JSON.stringify(config);
      lib.extism_plugin_config(this.ctx.pointer, this.id, s, s.length);
    }
  }

  // Update an existing plugin with new WASM or manifest
  update(data, wasi = false, config = null) {
    if (typeof data === 'object' && data.wasm) {
      data = JSON.stringify(data);
    }
    const ok = lib.extism_plugin_update(this.ctx.pointer, this.id, data, data.length, wasi);
    if (!ok) {
      var err = lib.extism_error(this.ctx.pointer, -1);
      if (err.length == 0) {
        throw "extism_plugin_update failed";
      }
      throw `Unable to update plugin: ${err.toString()}`;
    }

    if (config != null) {
      let s = JSON.stringify(config);
      lib.extism_plugin_config(this.ctx.pointer, this.id, s, s.length);
    }
  }

  // Check if a function exists
  functionExists(name) {
    return lib.extism_plugin_function_exists(this.ctx.pointer, this.id, name);
  }

  // Call a function by name with the given input
  async call(name, input) {
    return new Promise((resolve, reject) => {
      var rc = lib.extism_plugin_call(this.ctx.pointer, this.id, name, input, input.length);
      if (rc != 0) {
        var err = lib.extism_error(this.ctx.pointer, this.id);
        if (err.length == 0) {
          reject(`extism_plugin_call: "${name}" failed`);
        }
        reject(`Plugin error: ${err.toString()}, code: ${rc}`);
      }

      var out_len = lib.extism_plugin_output_length(this.ctx.pointer, this.id);
      var buf = Buffer.from(lib.extism_plugin_output_data(this.ctx.pointer, this.id).buffer, 0, out_len);
      resolve(buf);
    });
  }

  // Free a plugin, this should be called when the plugin is no longer needed
  free() {
    pluginRegistry.unregister(this);
    lib.extism_plugin_free(this.ctx.pointer, this.id);
    this.id = -1;
  }
}


