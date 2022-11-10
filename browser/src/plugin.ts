import Allocator from './allocator';
import { PluginConfig } from './manifest';

class ExtismPluginCall {
  input: Uint8Array;
  output: Uint8Array;
  allocator: Allocator;

  constructor(allocator: Allocator, input: Uint8Array, output: Uint8Array) {
    this.input = input;
    this.output = output;
    this.allocator = allocator;
  }

  makeEnv(): any {
    return {
      extism_alloc(n: bigint): bigint {
        return this.allocator.alloc(n);
      },
      extism_free(n: bigint) {
        this.allocator.free(n);
      },
      extism_load_u8(n: bigint): number {
        return this.allocator.memory[Number(n)];
      },
      extism_load_u32(n: bigint): number {
        debugger;
        return 0;
      },
      extism_load_u64(n: bigint): bigint {
        let cast = new DataView(this.allocator.memory.buffer, Number(n));
        return cast.getBigUint64(0, true);
      },
      extism_store_u8(offset: bigint, n: number) {
        this.allocator.memory[Number(offset)] = Number(n);
      },
      extism_store_u32(n: bigint, i: number) {
        debugger;
      },
      extism_store_u64(offset: bigint, n: bigint) {
        const tmp = new DataView(this.allocator.memory.buffer, Number(offset));
        tmp.setBigUint64(0, n, true);
      },
      extism_input_length(): bigint {
        return BigInt(this.input.length);
      },
      extism_input_load_u8(i: bigint): number {
        return this.input[Number(i)];
      },
      extism_input_load_u64(idx: bigint): bigint {
        let cast = new DataView(this.input.buffer, Number(idx));
        return cast.getBigUint64(0, true);
      },
      extism_output_set(offset: bigint, length: bigint) {
        const offs = Number(offset);
        const len = Number(length);
        this.output = this.allocator.memory.slice(offs, offs + len);
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
        return this.allocator.getLength(i);
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
}

export default class ExtismPlugin {
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
    const environment = call.makeEnv();
    return await WebAssembly.instantiate(this.moduleData, { env: environment });
  }
}
