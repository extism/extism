
function unpackU64toU8(n: BigInt): Uint8Array {
    //@ts-ignore
    return new Uint8Array(new BigInt64Array([n]).buffer)
}

function packU8toU64(bytes: Uint8Array): BigInt {
    return (new BigInt64Array(bytes.buffer))[0]
}

function stringToBytes(s: string, offset: number, len: number): Uint8Array {
    return new Uint8Array(s.slice(offset, len).split("").map(c => c.charCodeAt(0)))
}

function makeEnv(plugin: ExtismPlugin): any {
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
            return plugin.input[i].charCodeAt(0)
        },
        extism_input_load_u64(idx: BigInt) {
            const i = Number(idx)
            //@ts-ignore
            return packU8toU64(stringToBytes(plugin.input, i, i+8))
        },
        extism_output_set(offset: BigInt, len: number): number {
            //@ts-ignore
            offset = Number(offset)
            len = Number(len)
            //@ts-ignore
            plugin.output = String.fromCharCode(...plugin.memory.slice(offset, offset+len))
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

export class ExtismPlugin {
    memory: Uint8Array
    module?: WebAssembly.WebAssemblyInstantiatedSource;
    input?: string
    output?: string

    constructor() {
      this.memory = new Uint8Array(100)
      this.module = undefined
      this.input = undefined
      this.output = undefined
    }

    async load(url: string) {
        let response = await fetch(url)
        let buffer = await response.arrayBuffer()
        let environment = makeEnv(this)
        //@ts-ignore
        this.module = await WebAssembly.instantiate(buffer, { env: environment })
    }

    call(func_name: string, input: string): string {
        this.input = input
        let func = this.module?.instance.exports[func_name]
        if (!func){
            throw Error(`function does not exist ${func_name}`)
        }
        //@ts-ignore
        func()
        let output = `${this.output}`
        this.output = undefined
        this.input = undefined
        return output
    }
}
