
function unpackU64toU8(n: BigInt): Uint8Array {
    //@ts-ignore
    return new Uint8Array(new BigInt64Array([n]).buffer)
}

function packU8toU64(bytes: Uint8Array): BigInt {
    return (new BigInt64Array(bytes.buffer))[0]
}

function makeEnv(plugin: ExtismPluginCall): any {
    return {
        extism_alloc(n: BigInt): BigInt {
            return BigInt(50) // just pick a safe? number for now
        },
        extism_free (n: BigInt) {debugger},
        extism_load_u8(n: BigInt): number { debugger; return 0 },
        extism_load_u32(n: BigInt): number {debugger; return 0 },
        extism_load_u64(n: BigInt): BigInt { debugger;return BigInt(0)},
        extism_store_u8(offset: BigInt, n: number) {
            //@ts-ignore
            plugin.memory[offset] = Number(n)
        },
        extism_store_u32(n: BigInt, i: number) {debugger; },
        extism_store_u64(offset: BigInt, n: BigInt) {
            let parts = unpackU64toU8(n)
            parts.forEach((p, i) => {
                plugin.memory[i + Number(offset)] = p
            })
        },
        extism_input_length(): BigInt {
            //@ts-ignore
            return BigInt(plugin.input.length)
        },
        extism_input_load_u8(i: number): number {
            //@ts-ignore
            return plugin.input[i]
        },
        extism_input_load_u64(idx: BigInt) {
            const i = Number(idx)
            //@ts-ignore
            return packU8toU64(plugin.input.slice(i, i+8))
        },
        extism_output_set(offset: BigInt, len: number): number {
            //@ts-ignore
            offset = Number(offset)
            len = Number(len)
            //@ts-ignore
            plugin.output = plugin.memory.slice(offset, offset+len)
            return 0
        },
        extism_error_set(i: BigInt) { debugger; },
        extism_config_get(i: BigInt): number { debugger; return 0 },
        extism_var_get(i: BigInt): number {debugger;return 0},
        extism_var_set(n: BigInt, i: BigInt) {debugger;},
        extism_http_request(n: BigInt, i: BigInt): number {debugger;return 0},
        extism_length(i: BigInt): number {debugger;return 0},
        extism_log_warn(i: number) {debugger;},
        extism_log_info(i: number) {debugger;},
        extism_log_debug(i: number) {debugger;},
        extism_log_error(i: number) {debugger;},
    }
}

export default class ExtismContext {
    async newPlugin(url: string) {
        let response = await fetch(url)
        let buffer = await response.arrayBuffer()
        return new ExtismPlugin(buffer)
    }
}

class ExtismPluginCall {
    memory: Uint8Array
    input: Uint8Array
    output: Uint8Array

    constructor(memory: Uint8Array, input: Uint8Array, output: Uint8Array) {
        this.memory = memory
        this.input = input
        this.output = output
    }
}

class ExtismPlugin {
    moduleData: ArrayBuffer

    constructor(moduleData: ArrayBuffer) {
        this.moduleData = moduleData
    }

    async call(func_name: string, input: Uint8Array | string): Promise<Uint8Array> {
        const memory = new Uint8Array(10000)
        const output = new Uint8Array()
        let inputBytes: Uint8Array
        if (typeof input === 'string') {
            inputBytes = new TextEncoder().encode(input)
        } else if (input instanceof Uint8Array) {
            inputBytes = input
        } else {
            throw new Error("input should be string or Uint8Array")
        }
        const call = new ExtismPluginCall(memory, inputBytes, output)
        const environment = makeEnv(call)
        const module = await WebAssembly.instantiate(this.moduleData, { env: environment })
        let func = module.instance.exports[func_name]
        if (!func){
            throw Error(`function does not exist ${func_name}`)
        }
        //@ts-ignore
        func()
        return call.output
    }
}

