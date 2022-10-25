import ffi  from 'ffi-napi';
import path from 'path';
import url from 'url';

const __dirname = url.fileURLToPath(new URL('.', import.meta.url));
const context = 'void*';
const _functions = {
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
    extism_version: ['char*', []],
};

interface LibExtism {
    extism_context_new: () => Buffer;
    extism_context_free: (ctx: Buffer) => void;
    extism_plugin_new: (ctx: Buffer, data: string | Buffer, data_len: number, wasi: boolean) => number;
    extism_plugin_update: (ctx: Buffer, plugin_id: number, data: string | Buffer, data_len: number, wasi: boolean) => boolean;
    extism_error: (ctx: Buffer, plugin_id: number) => string;
    extism_plugin_call: (ctx: Buffer, plugin_id: number, func: string, input: string, input_len: number) => number;
    extism_plugin_output_length: (ctx: Buffer, plugin_id: number) => number;
    extism_plugin_output_data: (ctx: Buffer, plugin_id: Number) => Uint8Array;
    extism_log_file: (file: string, level: string) => boolean;
    extism_plugin_function_exists: (ctx: Buffer, plugin_id: number, func: string) => boolean;
    extism_plugin_config: (ctx: Buffer, plugin_id: number, data: string | Buffer, data_len: number) => void;
    extism_plugin_free: (ctx: Buffer, plugin_id: number) => void;
    extism_context_reset: (ctx: Buffer) => void;
    extism_version: () => string;
}

function locate(paths: string[]): LibExtism {
    for (var i = 0; i < paths.length; i++) {
        try {
            // @ts-ignore
            return ffi.Library(path.join(paths[i], 'libextism'), _functions);
        } catch (exn) {
            continue;
        }
    }

    throw 'Unable to locate libextism';
}

const searchPath = [
    __dirname,
    '/usr/local/lib',
    '/usr/lib',
    path.join(process.env.HOME as string, '.local', 'lib'),
];

if (process.env.EXTISM_PATH) {
    searchPath.unshift(path.join(process.env.EXTISM_PATH, 'lib'));
}

const lib = locate(searchPath);

// Set the log file and level
export function setLogFile(filename: string, level?: string) {
    lib.extism_log_file(filename, level || "info");
}

// Get the version of Extism
export function extismVersion(): string {
    return lib.extism_version().toString();
}

// @ts-ignore
const pluginRegistry = new FinalizationRegistry(({ id, pointer }) => {
    lib.extism_plugin_free(pointer, id);
});

// @ts-ignore
const contextRegistry = new FinalizationRegistry((pointer) => {
    lib.extism_context_free(pointer);
});

export type ManifestWasmFile = {
    path: string;
    name?: string;
    hash?: string;
}

export type ManifestWasmData = {
    data: Uint8Array;
    name?: string;
    hash?: string;
}

export type ManifestMemory = {
    max?: number;
}

export type PluginConfig = Map<string, string>;

export type ManifestWasm = ManifestWasmFile | ManifestWasmData;

export type Manifest = {
    wasm: Array<ManifestWasm>;
    memory: ManifestMemory;
    config?: PluginConfig;
    allowed_hosts?: Array<string>;
}

type ManifestData = Manifest | Buffer | string;


// Context manages plugins
export class Context {
    pointer: Buffer | null;

    constructor() {
        this.pointer = lib.extism_context_new();
        contextRegistry.register(this, this.pointer, this);
    }

    // Create a new plugin, optionally enabling WASI
    plugin(data: ManifestData, wasi: boolean = false, config?: PluginConfig) {
        return new Plugin(this, data, wasi, config);
    }

    // Free a context, this should be called when it is
    // no longer needed
    free() {
        if (this.pointer) {
            contextRegistry.unregister(this);
            lib.extism_context_free(this.pointer);
        }
        this.pointer = null;
    }

    // Remove all registered plugins
    reset() {
        if (this.pointer)
            lib.extism_context_reset(this.pointer);
    }
}

export async function withContext(f: (ctx: Context) => Promise<any>) {
    let ctx = new Context();

    try {
        let x = await f(ctx);
        ctx.free();
        return x;
    } catch (err) {
        ctx.free();
        throw err;
    }
}

// Plugin provides an interface for calling WASM functions
export class Plugin {
    id: number;
    ctx: Context;

    constructor(ctx: Context, data: ManifestData, wasi: boolean = false, config?: PluginConfig) {
        let dataRaw: string | Buffer;
        if (Buffer.isBuffer(data) || typeof data === 'string') {
            dataRaw = data
        } else if (typeof data === 'object' && data.wasm) {
            dataRaw = JSON.stringify(data)
        } else {
            throw Error(`Unknown manifest type ${typeof data}`);
        }
        if (!ctx.pointer) throw Error("No Context set");
        let plugin = lib.extism_plugin_new(ctx.pointer, dataRaw, dataRaw.length, wasi);
        if (plugin < 0) {
            var err = lib.extism_error(ctx.pointer, -1);
            if (err.length === 0) {
                throw "extism_context_plugin failed";
            }
            throw `Unable to load plugin: ${err.toString()}`;
        }
        this.id = plugin;
        this.ctx = ctx;
        pluginRegistry.register(this, { id: this.id, pointer: this.ctx.pointer }, this);

        if (config != null) {
            let s = JSON.stringify(config);
            lib.extism_plugin_config(ctx.pointer, this.id, s, s.length);
        }
    }

    // Update an existing plugin with new WASM or manifest
    update(data: ManifestData, wasi: boolean = false, config?: PluginConfig) {
        let dataRaw: string | Buffer;
        if (Buffer.isBuffer(data) || typeof data === 'string') {
            dataRaw = data
        } else if (typeof data === 'object' && data.wasm) {
            dataRaw = JSON.stringify(data)
        } else {
            throw Error("Unknown manifest type type");
        }
        if (!this.ctx.pointer) throw Error("No Context set");
        const ok = lib.extism_plugin_update(this.ctx.pointer, this.id, dataRaw, dataRaw.length, wasi);
        if (!ok) {
            var err = lib.extism_error(this.ctx.pointer, -1);
            if (err.length === 0) {
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
    functionExists(name: string) {
        if (!this.ctx.pointer) throw Error("No Context set");
        return lib.extism_plugin_function_exists(this.ctx.pointer, this.id, name);
    }

    // Call a function by name with the given input
    async call(name: string, input: string) {
        return new Promise((resolve, reject) => {
            if (!this.ctx.pointer) throw Error("No Context set");
            var rc = lib.extism_plugin_call(this.ctx.pointer, this.id, name, input, input.length);
            if (rc !== 0) {
                var err = lib.extism_error(this.ctx.pointer, this.id);
                if (err.length === 0) {
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
        if (this.ctx.pointer && this.id !== -1) {
            pluginRegistry.unregister(this);
            lib.extism_plugin_free(this.ctx.pointer, this.id);
            this.id = -1;
        }
    }
}
