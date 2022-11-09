type MemoryBlock = { offset: bigint; length: bigint };

class Allocator {
  currentIndex: bigint;
  active: Record<number, MemoryBlock>;
  freed: MemoryBlock[];
  memory: Uint8Array;

  constructor(n: number) {
    this.currentIndex = BigInt(1);
    this.active = {};
    this.freed = [];
    this.memory = new Uint8Array(n);
  }

  reset() {
    this.currentIndex = BigInt(1);
    this.active = {};
    this.freed = [];
  }

  alloc(length: bigint): bigint {
    for (var i = 0; i < this.freed.length; i++) {
      let block = this.freed[i];
      if (block.length === length) {
        this.active[Number(block.offset)] = block;
        this.freed.splice(i, 1);
        return block.offset;
      } else if (block.length > length + BigInt(64)) {
        const newBlock = { offset: block.offset, length };
        block.offset += length;
        block.length -= length;
        return newBlock.offset;
      }
    }

    // Resize memory if needed
    // TODO: put a limit on the memory size
    if (BigInt(this.memory.length) < this.currentIndex + length) {
      const tmp = new Uint8Array(Number(this.currentIndex + length + BigInt(64)));
      tmp.set(this.memory);
      this.memory = tmp;
    }

    const offset = this.currentIndex;
    this.currentIndex += length;
    this.active[Number(offset)] = { offset, length };
    return offset;
  }

  getBytes(offset: bigint): Uint8Array | null {
    const block = this.active[Number(offset)];
    if (!block) {
      return null;
    }

    return new Uint8Array(this.memory.buffer, Number(offset), Number(block.length));
  }

  getString(offset: bigint): string | null {
    const bytes = this.getBytes(offset);
    if (bytes === null) {
      return null;
    }

    return new TextDecoder().decode(bytes);
  }

  allocBytes(data: Uint8Array): bigint {
    const offs = this.alloc(BigInt(data.length));
    const bytes = this.getBytes(offs);
    if (bytes === null) {
      this.free(offs);
      return BigInt(0);
    }

    bytes.set(data);
    return offs;
  }

  allocString(data: string): bigint {
    const bytes = new TextEncoder().encode(data);
    return this.allocBytes(bytes);
  }

  getLength(offset: bigint): bigint {
    const block = this.active[Number(offset)];
    if (!block) {
      return BigInt(0);
    }

    return block.length;
  }

  free(offset: bigint) {
    const block = this.active[Number(offset)];
    if (!block) {
      return;
    }

    delete this.active[Number(offset)];
    this.freed.push(block);
  }
}

function makeEnv(plugin: ExtismPluginCall): any {
  return {
    extism_alloc(n: bigint): bigint {
      return plugin.allocator.alloc(n);
    },
    extism_free(n: bigint) {
      plugin.allocator.free(n);
    },
    extism_load_u8(n: bigint): number {
      return plugin.allocator.memory[Number(n)];
    },
    extism_load_u32(n: bigint): number {
      debugger;
      return 0;
    },
    extism_load_u64(n: bigint): bigint {
      let cast = new DataView(plugin.allocator.memory.buffer, Number(n));
      return cast.getBigUint64(0, true);
    },
    extism_store_u8(offset: bigint, n: number) {
      plugin.allocator.memory[Number(offset)] = Number(n);
    },
    extism_store_u32(n: bigint, i: number) {
      debugger;
    },
    extism_store_u64(offset: bigint, n: bigint) {
      const tmp = new DataView(plugin.allocator.memory.buffer, Number(offset));
      tmp.setBigUint64(0, n, true);
    },
    extism_input_length(): bigint {
      return BigInt(plugin.input.length);
    },
    extism_input_load_u8(i: bigint): number {
      return plugin.input[Number(i)];
    },
    extism_input_load_u64(idx: bigint): bigint {
      let cast = new DataView(plugin.input.buffer, Number(idx));
      return cast.getBigUint64(0, true);
    },
    extism_output_set(offset: bigint, length: bigint) {
      const offs = Number(offset);
      const len = Number(length);
      plugin.output = plugin.allocator.memory.slice(offs, offs + len);
    },
    extism_error_set(i: bigint) {
      debugger;
    },
    extism_config_get(i: bigint): number {
      debugger;
      return 0;
    },
    extism_var_get(i: bigint): number {
      debugger;
      return 0;
    },
    extism_var_set(n: bigint, i: bigint) {
      debugger;
    },
    extism_http_request(n: bigint, i: bigint): number {
      debugger;
      return 0;
    },
    extism_length(i: bigint): bigint {
      return plugin.allocator.getLength(i);
    },
    extism_log_warn(i: number) {
      debugger;
    },
    extism_log_info(i: number) {
      debugger;
    },
    extism_log_debug(i: number) {
      debugger;
    },
    extism_log_error(i: number) {
      debugger;
    },
  };
}

/**
 * Represents a path or url to a WASM module
 */
export type ManifestWasmFile = {
  path: string;
  name?: string;
  hash?: string;
};

/**
 * Represents the raw bytes of a WASM file loaded into memory
 */
export type ManifestWasmData = {
  data: Uint8Array;
  name?: string;
  hash?: string;
};

/**
 * {@link ExtismPlugin} Config
 */
export type PluginConfig = Map<string, string>;

/**
 * The WASM to load as bytes or a path
 */
export type ManifestWasm = ManifestWasmFile | ManifestWasmData;

/**
 * The manifest which describes the {@link ExtismPlugin} code and
 * runtime constraints.
 *
 * @see [Extism > Concepts > Manifest](https://extism.org/docs/concepts/manifest)
 */
export type Manifest = {
  wasm: Array<ManifestWasm>;
  //memory?: ManifestMemory;
  config?: PluginConfig;
  allowed_hosts?: Array<string>;
};

/**
 * Can be a {@link Manifest} or just the raw bytes of the WASM module as an ArrayBuffer.
 * We recommend using {@link Manifest}
 */
type ManifestData = Manifest | ArrayBuffer;

/**
 * A Context is needed to create plugins. The Context
 * is where your plugins live. Freeing the context
 * frees all of the plugins in its scope.
 */
export default class ExtismContext {
  /**
   * Create a plugin managed by this context
   *
   * @param manifest - The {@link ManifestData} describing the plugin code and config
   * @param config - Config details for the plugin
   * @returns A new Plugin scoped to this Context
   */
  async newPlugin(manifest: ManifestData, config?: PluginConfig) {
    let moduleData: ArrayBuffer | null = null;
    if (manifest instanceof ArrayBuffer) {
      moduleData = manifest;
    } else if ((manifest as Manifest).wasm) {
      const wasmData = (manifest as Manifest).wasm;
      if (wasmData.length > 1) throw Error('This runtime only supports one module in Manifest.wasm');
      const wasm = wasmData[0];
      if ((wasm as ManifestWasmData).data) {
        moduleData = (wasm as ManifestWasmData).data;
      } else if ((wasm as ManifestWasmFile).path) {
        const response = await fetch((wasm as ManifestWasmFile).path);
        moduleData = await response.arrayBuffer();
        console.dir(moduleData);
      }
    }
    if (!moduleData) {
      throw Error(`Unsure how to interpret manifest ${manifest}`);
    }

    return new ExtismPlugin(moduleData, config);
  }
}

class ExtismPluginCall {
  input: Uint8Array;
  output: Uint8Array;
  allocator: Allocator;

  constructor(allocator: Allocator, input: Uint8Array, output: Uint8Array) {
    this.input = input;
    this.output = output;
    this.allocator = allocator;
  }
}

class ExtismPlugin {
  moduleData: ArrayBuffer;
  allocator: Allocator;
  config?: PluginConfig;

  constructor(moduleData: ArrayBuffer, config?: PluginConfig) {
    this.moduleData = moduleData;
    this.allocator = new Allocator(1024 * 1024);
    this.config = config;
  }

  async getExportedFunctions() {
    // we can make an empty call environment
    let empty = new Uint8Array();
    let call = new ExtismPluginCall(this.allocator, empty, empty);
    let module = await this._instantiateModule(call);
    return Object.keys(module.instance.exports).filter((f) => !f.startsWith('__') && f !== 'memory');
  }

  async call(func_name: string, input: Uint8Array | string): Promise<Uint8Array> {
    const output = new Uint8Array();
    let inputBytes: Uint8Array;
    if (typeof input === 'string') {
      inputBytes = new TextEncoder().encode(input);
    } else if (input instanceof Uint8Array) {
      inputBytes = input;
    } else {
      throw new Error('input should be string or Uint8Array');
    }
    this.allocator.reset();
    // TODO: only instantiate module once
    const call = new ExtismPluginCall(this.allocator, inputBytes, output);
    const module = await this._instantiateModule(call);
    let func = module.instance.exports[func_name];
    if (!func) {
      throw Error(`function does not exist ${func_name}`);
    }
    //@ts-ignore
    func();
    return call.output;
  }

  async _instantiateModule(call: ExtismPluginCall) {
    const environment = makeEnv(call);
    return await WebAssembly.instantiate(this.moduleData, { env: environment });
  }
}
