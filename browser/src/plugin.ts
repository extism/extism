import Allocator from './allocator';
import { PluginConfig } from './manifest';
import { WASI, Fd } from '@bjorn3/browser_wasi_shim';

export type ExtismFunction = any;

export class ExtismPlugin {
  moduleData: ArrayBuffer;
  allocator: Allocator;
  config?: PluginConfig;
  vars: Record<string, Uint8Array>;
  input: Uint8Array;
  output: Uint8Array;
  module?: WebAssembly.WebAssemblyInstantiatedSource;
  functions: Record<string, ExtismFunction>;

  constructor(moduleData: ArrayBuffer, functions: Record<string, ExtismFunction> = {}, config?: PluginConfig) {
    this.moduleData = moduleData;
    this.allocator = new Allocator(1024 * 1024);
    this.config = config;
    this.vars = {};
    this.input = new Uint8Array();
    this.output = new Uint8Array();
    this.functions = functions;
  }

  async getExports(): Promise<WebAssembly.Exports> {
    const module = await this._instantiateModule();
    return module.instance.exports;
  }

  async getImports(): Promise<WebAssembly.ModuleImportDescriptor[]> {
    const module = await this._instantiateModule();
    return WebAssembly.Module.imports(module.module);
  }

  async getInstance(): Promise<WebAssembly.Instance> {
    const module = await this._instantiateModule();
    return module.instance;
  }

  async call(func_name: string, input: Uint8Array | string): Promise<Uint8Array> {
    const module = await this._instantiateModule();

    if (typeof input === 'string') {
      this.input = new TextEncoder().encode(input);
    } else if (input instanceof Uint8Array) {
      this.input = input;
    } else {
      throw new Error('input should be string or Uint8Array');
    }

    this.allocator.reset();

    let func = module.instance.exports[func_name];
    if (!func) {
      throw Error(`function does not exist ${func_name}`);
    }
    //@ts-ignore
    func();
    return this.output;
  }

  async _instantiateModule(): Promise<WebAssembly.WebAssemblyInstantiatedSource> {
    if (this.module) {
      return this.module;
    }
    const environment = this.makeEnv();
    const args: Array<string> = [];
    const envVars: Array<string> = [];
    let fds: Fd[] = [
      // new XtermStdio(term), // stdin
      // new XtermStdio(term), // stdout
      // new XtermStdio(term), // stderr
    ];
    let wasi = new WASI(args, envVars, fds);
    let env = {
      wasi_snapshot_preview1: wasi.wasiImport,
      env: environment,
    };
    this.module = await WebAssembly.instantiate(this.moduleData, env);
    // normally we would call wasi.start here but it doesn't respect when there is
    // no _start function
    //@ts-ignore
    wasi.inst = this.module.instance;
    if (this.module.instance.exports._start) {
      //@ts-ignore
      this.module.instance.exports._start();
    }
    return this.module;
  }

  makeEnv(): any {
    const plugin = this;
    var env: any = {
      extism_alloc(n: bigint): bigint {
        return plugin.allocator.alloc(n);
      },
      extism_free(n: bigint) {
        plugin.allocator.free(n);
      },
      extism_load_u8(n: bigint): number {
        return plugin.allocator.memory[Number(n)];
      },
      extism_load_u64(n: bigint): bigint {
        let cast = new DataView(plugin.allocator.memory.buffer, Number(n));
        return cast.getBigUint64(0, true);
      },
      extism_store_u8(offset: bigint, n: number) {
        plugin.allocator.memory[Number(offset)] = Number(n);
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
        throw plugin.allocator.getString(i);
      },
      extism_config_get(i: bigint): bigint {
        if (typeof plugin.config === 'undefined') {
          return BigInt(0);
        }
        const key = plugin.allocator.getString(i);
        if (key === null) {
          return BigInt(0);
        }
        const value = plugin.config.get(key);
        if (typeof value === 'undefined') {
          return BigInt(0);
        }
        return plugin.allocator.allocString(value);
      },
      extism_var_get(i: bigint): bigint {
        const key = plugin.allocator.getString(i);
        if (key === null) {
          return BigInt(0);
        }
        const value = plugin.vars[key];
        if (typeof value === 'undefined') {
          return BigInt(0);
        }
        return plugin.allocator.allocBytes(value);
      },
      extism_var_set(n: bigint, i: bigint) {
        const key = plugin.allocator.getString(n);
        if (key === null) {
          return;
        }
        const value = plugin.allocator.getBytes(i);
        if (value === null) {
          return;
        }
        plugin.vars[key] = value;
      },
      extism_http_request(n: bigint, i: bigint): number {
        debugger;
        return 0;
      },
      extism_length(i: bigint): bigint {
        return plugin.allocator.getLength(i);
      },
      extism_log_warn(i: bigint) {
        const s = plugin.allocator.getString(i);
        console.warn(s);
      },
      extism_log_info(i: bigint) {
        const s = plugin.allocator.getString(i);
        console.log(s);
      },
      extism_log_debug(i: bigint) {
        const s = plugin.allocator.getString(i);
        console.debug(s);
      },
      extism_log_error(i: bigint) {
        const s = plugin.allocator.getString(i);
        console.error(s);
      },
    };

    for (const [name, func] of Object.entries(this.functions)) {
      env[name] = function () {
        return func.apply(plugin, arguments);
      };
    }

    return env;
  }
}
