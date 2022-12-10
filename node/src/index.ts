import ffi from "ffi-napi";
import ref from "ref-napi";
import path from "path";

var ArrayType = require("ref-array-di")(ref)
var StructType = require("ref-struct-di")(ref)
var UnionType = require("ref-union-di")(ref)

const opaque = ref.types.void;
const context = ref.refType(opaque);

const function_t = ref.refType(opaque);
const pluginIndex = ref.types.int32;

let ValTypeArray = ArrayType(ref.types.int);
let PtrArray = new ArrayType('void*');

let ValUnion = new UnionType({
  i32: ref.types.uint32,
  i64: ref.types.uint64,
  f32: ref.types.float,
  f64: ref.types.double,
});

let Val = new StructType({
  t: ref.types.int,
  v: ValUnion,
});

let ValArray = ArrayType(Val);

const _functions = {
  extism_context_new: [context, []],
  extism_context_free: ["void", [context]],
  extism_plugin_new: [pluginIndex, [context, "string", "uint64", "bool"]],
  extism_plugin_new_with_functions: [pluginIndex, [context, "string", "uint64", PtrArray, "uint64", "bool"]],
  extism_plugin_update: [
    "bool",
    [context, pluginIndex, "string", "uint64", "bool"],
  ],
  extism_error: ["string", [context, pluginIndex]],
  extism_plugin_call: [
    "int32",
    [context, pluginIndex, "string", "string", "uint64"],
  ],
  extism_plugin_output_length: ["uint64", [context, pluginIndex]],
  extism_plugin_output_data: ["uint8*", [context, pluginIndex]],
  extism_log_file: ["bool", ["string", "char*"]],
  extism_plugin_function_exists: ["bool", [context, pluginIndex, "string"]],
  extism_plugin_config: ["void", [context, pluginIndex, "char*", "uint64"]],
  extism_plugin_free: ["void", [context, pluginIndex]],
  extism_context_reset: ["void", [context]],
  extism_version: ["string", []],
  extism_function_new: [function_t, ["string", ValTypeArray, "uint64", ValTypeArray, "uint64", "void*", "void*", "void*"]],
  extism_function_free: ["void", [function_t]],
};

export enum ValType {
  I32 = 0,
  I64,
  F32,
  F64,
  FuncRef,
  ExternRef
}

interface LibExtism {
  extism_context_new: () => Buffer;
  extism_context_free: (ctx: Buffer) => void;
  extism_plugin_new: (
    ctx: Buffer,
    data: string | Buffer,
    data_len: number,
    wasi: boolean
  ) => number;
  extism_plugin_new_with_functions: (
    ctx: Buffer,
    data: string | Buffer,
    data_len: number,
    functions: Buffer,
    nfunctions: number,
    wasi: boolean
  ) => number;
  extism_plugin_update: (
    ctx: Buffer,
    plugin_id: number,
    data: string | Buffer,
    data_len: number,
    wasi: boolean
  ) => boolean;
  extism_error: (ctx: Buffer, plugin_id: number) => string;
  extism_plugin_call: (
    ctx: Buffer,
    plugin_id: number,
    func: string,
    input: string,
    input_len: number
  ) => number;
  extism_plugin_output_length: (ctx: Buffer, plugin_id: number) => number;
  extism_plugin_output_data: (ctx: Buffer, plugin_id: Number) => Uint8Array;
  extism_log_file: (file: string, level: string) => boolean;
  extism_plugin_function_exists: (
    ctx: Buffer,
    plugin_id: number,
    func: string
  ) => boolean;
  extism_plugin_config: (
    ctx: Buffer,
    plugin_id: number,
    data: string | Buffer,
    data_len: number
  ) => void;
  extism_plugin_free: (ctx: Buffer, plugin_id: number) => void;
  extism_context_reset: (ctx: Buffer) => void;
  extism_version: () => string;
  extism_function_new: (
    name: string,
    inputs: Buffer,
    nInputs: number,
    outputs: Buffer,
    nOutputs: number,
    f: Buffer,
    user_data: Buffer | null,
    free: Buffer | null
  ) => Buffer;
  extism_function_free: (f: Buffer) => void;
}

function locate(paths: string[]): LibExtism {
  for (var i = 0; i < paths.length; i++) {
    try {
      // @ts-ignore
      return ffi.Library(path.join(paths[i], "libextism"), _functions);
    } catch (exn) {
      continue;
    }
  }

  throw "Unable to locate libextism";
}

const searchPath = [
  __dirname,
  "/usr/local/lib",
  "/usr/lib",
  path.join(process.env.HOME as string, ".local", "lib"),
];

if (process.env.EXTISM_PATH) {
  searchPath.unshift(path.join(process.env.EXTISM_PATH, "lib"));
}

const lib = locate(searchPath);

/**
 * Sets the logfile and level of the Extism runtime
 *
 * @param filename - The path to the logfile
 * @param level - The level, one of ('debug', 'error', 'info', 'trace')
 */
export function setLogFile(filename: string, level?: string) {
  lib.extism_log_file(filename, level || "info");
}

/**
 * Get the version of Extism
 *
 * @returns The version string of the Extism runtime
 */
export function extismVersion(): string {
  return lib.extism_version();
}

// @ts-ignore
const pluginRegistry = new FinalizationRegistry(({ id, pointer }) => {
  if (id && pointer) lib.extism_plugin_free(pointer, id);
});

// @ts-ignore
const contextRegistry = new FinalizationRegistry((pointer) => {
  if (pointer) lib.extism_context_free(pointer);
});

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
 * Memory options for the {@link Plugin}
 */
export type ManifestMemory = {
  max_pages?: number;
};

/**
 * {@link Plugin} Config
 */
export type PluginConfig = Map<string, string>;

/**
 * The WASM to load as bytes or a path
 */
export type ManifestWasm = ManifestWasmFile | ManifestWasmData;

/**
 * The manifest which describes the {@link Plugin} code and
 * runtime constraints.
 * 
 * @see [Extism > Concepts > Manifest](https://extism.org/docs/concepts/manifest)
 */
export type Manifest = {
  wasm: Array<ManifestWasm>;
  memory?: ManifestMemory;
  config?: PluginConfig;
  allowed_hosts?: Array<string>;
  allowed_paths?: Record<string, string>;
  timeout_ms?: number;
};

/**
 * Can be a {@link Manifest} or just the raw bytes of the WASM module.
 * We recommend using {@link Manifest}
 */
type ManifestData = Manifest | Buffer | string;

/**
 * A Context is needed to create plugins. The Context
 * is where your plugins live. Freeing the context
 * frees all of the plugins in its scope. We recommand managing
 * the context with {@link withContext}
 *
 * @see {@link withContext}
 *
 * @example
 * Use withContext to ensure your memory is cleaned up
 * ```
 * const output = await withContext(async (ctx) => {
 *   const plugin = ctx.plugin(manifest)
 *   return await plugin.call("func", "my-input")
 * })
 * ```
 *
 * @example
 * You can manage manually if you need a long-lived context
 * ```
 * const ctx = Context()
 * // free all the plugins and reset
 * ctx.reset()
 * // free everything
 * ctx.free()
 * ```
 */
export class Context {
  pointer: Buffer | null;

  /**
   * Construct a context
   */
  constructor() {
    this.pointer = lib.extism_context_new();
    contextRegistry.register(this, this.pointer, this);
  }

  /**
   * Create a plugin managed by this context
   *
   * @param manifest - The {@link Manifest} describing the plugin code and config
   * @param wasi - Set to `true` to enable WASI
   * @param config - Config details for the plugin
   * @returns A new Plugin scoped to this Context
   */
  plugin(manifest: ManifestData, wasi: boolean = false, functions: Function[] = [], config?: PluginConfig) {
    return new Plugin(this, manifest, wasi, functions, config);
  }

  /**
   * Frees the context. Should be called after the context is not needed to reclaim the memory.
   */
  free() {
    if (this.pointer) {
      contextRegistry.unregister(this);
      lib.extism_context_free(this.pointer);
    }
    this.pointer = null;
  }

  /**
   * Resets the context. This clears all the plugins but keeps the context alive.
   */
  reset() {
    if (this.pointer) lib.extism_context_reset(this.pointer);
  }
}

/**
 * Creates a context and gives you a scope to use it. This will ensure the context
 * and all its plugins are cleaned up for you when you are done.
 *
 * @param f - The callback function with the context
 * @returns Whatever your callback returns
 */
export async function withContext(f: (ctx: Context) => Promise<any>) {
  const ctx = new Context();

  try {
    const x = await f(ctx);
    ctx.free();
    return x;
  } catch (err) {
    ctx.free();
    throw err;
  }
}


export class Function {
  callback: any;
  ptr: Buffer;
  name: string;
  userData: Buffer;
  inputs: typeof ValTypeArray;
  outputs: typeof ValTypeArray;

  constructor(name: string, inputs: ValType[], outputs: ValType[], f: any, userData?: any) {
    this.callback = ffi.Callback("void", [ValArray, "uint64", ValArray, "uint64", "void*"],
      (inputs: Buffer, n_inputs, outputs: Buffer, n_outputs, user_data) => {
        let inputArr = [];
        let outputArr = [];

        for (var i = 0; i < n_inputs; i++) {
          inputArr.push(Val.get(inputs, i));
        }


        for (var i = 0; i < n_outputs; i++) {
          outputArr.push(Val.get(outputs, i));
        }
        
        let out = f(inputArr);

        if (!Array.isArray(out)){
          outputs[0] = out;
          return;
        }

        for (var i = 0; i < n_outputs; i++){
          outputs[i] = out[i];
        }  
      });
    this.name = name;
    this.inputs = new ValTypeArray(inputs);
    this.outputs = new ValTypeArray(outputs);
    this.ptr = lib.extism_function_new(
      this.name,
      this.inputs,
      this.inputs.length,
      this.outputs,
      this.outputs.length,
      this.callback,
      null, null
    );
    this.userData = userData;
  }
}

/**
 * A Plugin represents an instance of your WASM program from the given manifest.
 */
export class Plugin {
  id: number;
  ctx: Context;
  functions: typeof PtrArray;

  /**
   * Constructor for a plugin. @see {@link Context#plugin}.
   *
   * @param ctx - The context to manage this plugin
   * @param manifest - The {@link Manifest}
   * @param wasi - Set to true to enable WASI support
   * @param config - The plugin config
   */
  constructor(
    ctx: Context,
    manifest: ManifestData,
    wasi: boolean = false,
    functions: Function[] = [],
    config?: PluginConfig
  ) {
    let dataRaw: string | Buffer;
    if (Buffer.isBuffer(manifest) || typeof manifest === "string") {
      dataRaw = manifest;
    } else if (typeof manifest === "object" && manifest.wasm) {
      dataRaw = JSON.stringify(manifest);
    } else {
      throw Error(`Unknown manifest type ${typeof manifest}`);
    }
    if (!ctx.pointer) throw Error("No Context set");
    this.functions = new PtrArray(functions.length);
    for (var i = 0; i < functions.length; i++) {
      this.functions[i] = functions[i].ptr;
    }
    let plugin = lib.extism_plugin_new_with_functions(
      ctx.pointer,
      dataRaw,
      Buffer.byteLength(dataRaw, 'utf-8'),
      this.functions,
      functions.length,
      wasi
    );
    if (plugin < 0) {
      var err = lib.extism_error(ctx.pointer, -1);
      if (err.length === 0) {
        throw "extism_context_plugin failed";
      }
      throw `Unable to load plugin: ${err.toString()}`;
    }
    this.id = plugin;
    this.ctx = ctx;
    pluginRegistry.register(
      this,
      { id: this.id, pointer: this.ctx.pointer },
      this
    );

    if (config != null) {
      let s = JSON.stringify(config);
      lib.extism_plugin_config(ctx.pointer, this.id, s, Buffer.byteLength(s, 'utf-8'),);
    }
  }

  /**
   * Update an existing plugin with new WASM or manifest
   *
   * @param manifest - The new {@link Manifest} data
   * @param wasi - Set to true to enable WASI support
   * @param config - The new plugin config
   */
  update(manifest: ManifestData, wasi: boolean = false, config?: PluginConfig) {
    let dataRaw: string | Buffer;
    if (Buffer.isBuffer(manifest) || typeof manifest === "string") {
      dataRaw = manifest;
    } else if (typeof manifest === "object" && manifest.wasm) {
      dataRaw = JSON.stringify(manifest);
    } else {
      throw Error("Unknown manifest type type");
    }
    if (!this.ctx.pointer) throw Error("No Context set");
    const ok = lib.extism_plugin_update(
      this.ctx.pointer,
      this.id,
      dataRaw,
      Buffer.byteLength(dataRaw, 'utf-8'),
      wasi
    );
    if (!ok) {
      var err = lib.extism_error(this.ctx.pointer, -1);
      if (err.length === 0) {
        throw "extism_plugin_update failed";
      }
      throw `Unable to update plugin: ${err.toString()}`;
    }

    if (config != null) {
      let s = JSON.stringify(config);
      lib.extism_plugin_config(this.ctx.pointer, this.id, s, Buffer.byteLength(s, 'utf-8'),);
    }
  }

  /**
   * Check if a function exists by name
   *
   * @param functionName - The name of the function
   * @returns true if the function exists, false if not
   */

  functionExists(functionName: string) {
    if (!this.ctx.pointer) throw Error("No Context set");
    return lib.extism_plugin_function_exists(
      this.ctx.pointer,
      this.id,
      functionName
    );
  }

  /**
   * Invoke a plugin function with given name and input.
   *
   * @example
   * ```
   * const manifest = { wasm: [{ path: "/tmp/code.wasm" }] }
   * const plugin = ctx.plugin(manifest)
   * const output = await plugin.call("my_function", "some-input")
   * output.toString()
   * // => "output from the function"
   * ```
   *
   * @param functionName - The name of the function
   * @param input - The input data
   * @returns A Buffer repreesentation of the output
   */
  async call(functionName: string, input: string | Buffer): Promise<Buffer> {
    return new Promise<Buffer>((resolve, reject) => {
      if (!this.ctx.pointer) throw Error("No Context set");
      var rc = lib.extism_plugin_call(
        this.ctx.pointer,
        this.id,
        functionName,
        input.toString(),
        Buffer.byteLength(input, 'utf-8'),
      );
      if (rc !== 0) {
        var err = lib.extism_error(this.ctx.pointer, this.id);
        if (err.length === 0) {
          reject(`extism_plugin_call: "${functionName}" failed`);
        }
        reject(`Plugin error: ${err.toString()}, code: ${rc}`);
      }

      var out_len = lib.extism_plugin_output_length(this.ctx.pointer, this.id);
      var buf = Buffer.from(
        lib.extism_plugin_output_data(this.ctx.pointer, this.id).buffer,
        0,
        out_len
      );
      resolve(buf);
    });
  }

  /**
   * Free a plugin, this should be called when the plugin is no longer needed
   */
  free() {
    if (this.ctx.pointer && this.id !== -1) {
      pluginRegistry.unregister(this);
      lib.extism_plugin_free(this.ctx.pointer, this.id);
      this.id = -1;
    }
  }
}

