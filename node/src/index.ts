import ffi from "ffi-napi";
import ref from "ref-napi";
import path from "path";

var ArrayType = require("ref-array-di")(ref);
var StructType = require("ref-struct-di")(ref);
var UnionType = require("ref-union-di")(ref);

const plugin = "void*";
const opaque = ref.types.void;
const function_t = ref.refType(opaque);

let ValTypeArray = ArrayType(ref.types.int);
let PtrArray = new ArrayType(function_t);

let ValUnion = new UnionType({
  i32: ref.types.uint32,
  i64: ref.types.uint64,
  f32: ref.types.float,
  f64: ref.types.double,
});

/**
 * Val struct, low-level WebAssembly values
 */
let Val = new StructType({
  t: ref.types.int,
  v: ValUnion,
});

/**
 * Array of `Val`
 */
let ValArray = ArrayType(Val);

const _functions = {
  extism_plugin_new: [
    plugin,
    [
      "string",
      "uint64",
      PtrArray,
      "uint64",
      "bool",
      ref.refType(ref.types.char),
    ],
  ],
  extism_plugin_error: ["string", [plugin]],
  extism_plugin_call: [
    "int32",
    [plugin, "string", "string", "uint64"],
  ],
  extism_plugin_output_length: ["uint64", [plugin]],
  extism_plugin_output_data: ["uint8*", [plugin]],
  extism_log_file: ["bool", ["string", "char*"]],
  extism_plugin_function_exists: ["bool", [plugin, "string"]],
  extism_plugin_config: ["void", [plugin, "char*", "uint64"]],
  extism_plugin_free: ["void", [plugin]],
  extism_plugin_new_error_free: ["void", ["char*"]],
  extism_version: ["string", []],
  extism_function_new: [
    function_t,
    [
      "string",
      ValTypeArray,
      "uint64",
      ValTypeArray,
      "uint64",
      "void*",
      "void*",
      "void*",
    ],
  ],
  extism_function_free: ["void", [function_t]],
  extism_function_set_namespace: ["void", [function_t, "string"]],
  extism_current_plugin_memory: ["uint8*", ["void*"]],
  extism_current_plugin_memory_alloc: ["uint64", ["void*", "uint64"]],
  extism_current_plugin_memory_length: ["uint64", ["void*", "uint64"]],
  extism_current_plugin_memory_free: ["void", ["void*", "uint64"]],
  extism_plugin_cancel_handle: ["void*", [plugin]],
  extism_plugin_cancel: ["bool", ["void*"]],
};

/**
 * An enumeration of all possible `Val` types
 */
export enum ValType {
  I32 = 0,
  I64,
  F32,
  F64,
  V128,
  FuncRef,
  ExternRef,
}

interface LibExtism {
  extism_plugin_new: (
    data: string | Buffer,
    data_len: number,
    functions: Buffer,
    nfunctions: number,
    wasi: boolean,
    errmsg: Buffer | null,
  ) => Buffer;
  extism_plugin_error: (plugin: Buffer) => string;
  extism_plugin_call: (
    plugin: Buffer,
    func: string,
    input: string,
    input_len: number,
  ) => number;
  extism_plugin_output_length: (plugin: Buffer) => number;
  extism_plugin_output_data: (plugin: Buffer) => Uint8Array;
  extism_log_file: (file: string, level: string) => boolean;
  extism_plugin_function_exists: (
    plugin: Buffer,
    func: string,
  ) => boolean;
  extism_plugin_config: (
    plugin: Buffer,
    data: string | Buffer,
    data_len: number,
  ) => void;
  extism_plugin_free: (plugin: Buffer) => void;
  extism_plugin_new_error_free: (error: Buffer) => void;
  extism_version: () => string;
  extism_function_new: (
    name: string,
    inputs: Buffer,
    nInputs: number,
    outputs: Buffer,
    nOutputs: number,
    f: Buffer,
    user_data: Buffer | null,
    free: Buffer | null,
  ) => Buffer;
  extism_function_set_namespace: (f: Buffer, s: string) => void;
  extism_function_free: (f: Buffer) => void;
  extism_current_plugin_memory: (p: Buffer) => Buffer;
  extism_current_plugin_memory_alloc: (p: Buffer, n: number) => number;
  extism_current_plugin_memory_length: (p: Buffer, n: number) => number;
  extism_current_plugin_memory_free: (p: Buffer, n: number) => void;
  extism_plugin_cancel_handle: (p: Buffer) => Buffer;
  extism_plugin_cancel: (p: Buffer) => boolean;
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
const functionRegistry = new FinalizationRegistry((pointer) => {
  if (pointer) lib.extism_function_free(pointer);
});

// @ts-ignore
const pluginRegistry = new FinalizationRegistry((handle) => {
  handle();
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
 * Provides access to the plugin that is currently running from inside a {@link HostFunction}
 */
export class CurrentPlugin {
  pointer: Buffer;

  constructor(pointer: Buffer) {
    this.pointer = pointer;
  }

  /**
   * Access plugin's memory
   * @param offset - The offset in memory
   * @returns a pointer to the provided offset
   */
  memory(offset: number): Buffer {
    let length = lib.extism_current_plugin_memory_length(this.pointer, offset);
    return Buffer.from(
      lib.extism_current_plugin_memory(this.pointer).buffer,
      offset,
      length,
    );
  }

  /**
   * Allocate a new memory block
   * @param n - The number of bytes to allocate
   * @returns the offset to the newly allocated block
   */
  memoryAlloc(n: number): number {
    return lib.extism_current_plugin_memory_alloc(this.pointer, n);
  }

  /**
   * Free a memory block
   * @param offset - The offset of the block to free
   */
  memoryFree(offset: number) {
    return lib.extism_current_plugin_memory_free(this.pointer, offset);
  }

  /**
   * Get the length of a memory block
   * @param offset - The offset of the block
   * @returns the length of the block specified by `offset`
   */
  memoryLength(offset: number): number {
    return lib.extism_current_plugin_memory_length(this.pointer, offset);
  }

  /**
   * Return a string from a host function
   * @param output - The output to set
   * @param s - The string to return
   */
  returnString(output: typeof Val, s: string) {
    var offs = this.memoryAlloc(Buffer.byteLength(s));
    this.memory(offs).write(s);
    output.v.i64 = offs;
  }

  /**
   * Return bytes from a host function
   * @param output - The output to set
   * @param b - The buffer to return
   */
  returnBytes(output: typeof Val, b: Buffer) {
    var offs = this.memoryAlloc(b.length);
    this.memory(offs).fill(b);
    output.v.i64 = offs;
  }

  /**
   * Get bytes from host function parameter
   * @param input - The input to read
   */
  inputBytes(input: typeof Val): Buffer {
    return this.memory(input.v.i64);
  }

  /**
   * Get string from host function parameter
   * @param input - The input to read
   */
  inputString(input: typeof Val): string {
    return this.memory(input.v.i64).toString();
  }
}

/**
 * Allows for the host to define functions that can be called from WebAseembly
 */
export class HostFunction {
  callback: any;
  pointer: Buffer | null;
  name: string;
  userData: any[];
  inputs: typeof ValTypeArray;
  outputs: typeof ValTypeArray;

  constructor(
    name: string,
    inputs: ValType[],
    outputs: ValType[],
    f: any,
    ...userData: any
  ) {
    this.userData = userData;
    this.callback = ffi.Callback(
      "void",
      [
        "void*",
        ref.refType(Val),
        "uint64",
        ref.refType(Val),
        "uint64",
        "void*",
      ],
      (
        currentPlugin: Buffer,
        inputs: Buffer,
        nInputs: number,
        outputs: Buffer,
        nOutputs: number,
        user_data,
      ) => {
        let inputArr = [];
        let outputArr = [];

        for (var i = 0; i < nInputs; i++) {
          inputArr.push(Val.get(inputs, i));
        }

        for (var i = 0; i < nOutputs; i++) {
          outputArr.push(Val.get(outputs, i));
        }

        f(
          new CurrentPlugin(currentPlugin),
          inputArr,
          outputArr,
          ...this.userData,
        );

        for (var i = 0; i < nOutputs; i++) {
          Val.set(outputs, i, outputArr[i]);
        }
      },
    );
    this.name = name;
    this.inputs = new ValTypeArray(inputs);
    this.outputs = new ValTypeArray(outputs);
    this.pointer = lib.extism_function_new(
      this.name,
      this.inputs,
      this.inputs.length,
      this.outputs,
      this.outputs.length,
      this.callback,
      null,
      null,
    );
    this.userData = userData;
    functionRegistry.register(this, this.pointer, this.pointer);
  }

  /**
   * Set function namespace
   */
  setNamespace(name: string) {
    if (this.pointer !== null) {
      lib.extism_function_set_namespace(this.pointer, name);
    }
  }

  withNamespace(name: string): HostFunction {
    this.setNamespace(name);
    return this;
  }

  /**
   *  Free a host function - this should be called to cleanup the associated resources
   */
  free() {
    functionRegistry.unregister(this.pointer);
    if (this.pointer === null) {
      return;
    }

    lib.extism_function_free(this.pointer);
    this.pointer = null;
  }
}

/**
 * CancelHandle is used to cancel a running Plugin
 */
export class CancelHandle {
  handle: Buffer;

  constructor(handle: Buffer) {
    this.handle = handle;
  }

  /**
   * Cancel execution of the Plugin associated with the CancelHandle
   */
  cancel(): boolean {
    return lib.extism_plugin_cancel(this.handle);
  }
}

/**
 * A Plugin represents an instance of your WASM program from the given manifest.
 */
export class Plugin {
  plugin: Buffer | null;
  functions: typeof PtrArray;
  token: { plugin: Buffer | null };

  /**
   * Constructor for a plugin.
   *
   * @param manifest - The {@link Manifest}
   * @param wasi - Set to true to enable WASI support
   * @param functions - An array of {@link HostFunction}
   * @param config - The plugin config
   */
  constructor(
    manifest: ManifestData,
    wasi: boolean = false,
    functions: HostFunction[] = [],
    config?: PluginConfig,
  ) {
    let dataRaw: string | Buffer;
    if (Buffer.isBuffer(manifest) || typeof manifest === "string") {
      dataRaw = manifest;
    } else if (typeof manifest === "object" && manifest.wasm) {
      dataRaw = JSON.stringify(manifest);
    } else {
      throw Error(`Unknown manifest type ${typeof manifest}`);
    }
    this.functions = new PtrArray(functions.length);
    for (var i = 0; i < functions.length; i++) {
      this.functions[i] = functions[i].pointer;
    }
    const plugin = lib.extism_plugin_new(
      dataRaw,
      Buffer.byteLength(dataRaw, "utf-8"),
      this.functions,
      functions.length,
      wasi,
      null,
    );
    if (ref.address(plugin) === 0) {
      // TODO: handle error
      throw Error("Failed to create plugin");
    }
    this.plugin = plugin;
    this.token = { plugin: this.plugin };
    pluginRegistry.register(this, () => {
      this.free();
    }, this.token);

    if (config != null) {
      const s = JSON.stringify(config);
      lib.extism_plugin_config(
        this.plugin,
        s,
        Buffer.byteLength(s, "utf-8"),
      );
    }
  }

  /**
   * Return a new `CancelHandle`, which can be used to cancel a running Plugin
   */
  cancelHandle(): CancelHandle {
    if (this.plugin === null) {
      throw Error("Plugin already freed");
    }
    const handle = lib.extism_plugin_cancel_handle(this.plugin);
    return new CancelHandle(handle);
  }

  /**
   * Check if a function exists by name
   *
   * @param functionName - The name of the function
   * @returns true if the function exists, false if not
   */

  functionExists(functionName: string) {
    if (this.plugin === null) {
      throw Error("Plugin already freed");
    }
    return lib.extism_plugin_function_exists(
      this.plugin,
      functionName,
    );
  }

  /**
   * Invoke a plugin function with given name and input.
   *
   * @example
   * ```
   * const manifest = { wasm: [{ path: "/tmp/code.wasm" }] }
   * const plugin = new Plugin(manifest)
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
      if (this.plugin === null) {
        reject("Plugin already freed");
        return;
      }
      var rc = lib.extism_plugin_call(
        this.plugin,
        functionName,
        input.toString(),
        Buffer.byteLength(input, "utf-8"),
      );
      if (rc !== 0) {
        var err = lib.extism_plugin_error(this.plugin);
        if (!err || err.length === 0) {
          reject(`Plugin error: call to "${functionName}" failed`);
        }
        reject(`Plugin error: ${err.toString()}, code: ${rc}`);
      }

      var out_len = lib.extism_plugin_output_length(this.plugin);
      var buf = Buffer.from(
        lib.extism_plugin_output_data(this.plugin).buffer,
        0,
        out_len,
      );
      resolve(buf);
    });
  }

  /**
   * Free a plugin, this should be called when the plugin is no longer needed
   */
  free() {
    if (this.plugin !== null) {
      pluginRegistry.unregister(this.token);
      lib.extism_plugin_free(this.plugin);
      this.plugin = null;
    }
  }
}
