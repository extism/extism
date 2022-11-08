function stringToBytes(s: string, offset: number, len: number): Uint8Array {
    return new Uint8Array(s.slice(offset, len).split("").map(c => c.charCodeAt(0)))
}

type MemoryBlock = {offset: BigInt, length: BigInt}

class Allocator {
    currentIndex: BigInt
    active: Record<number, MemoryBlock>
    freed: MemoryBlock[]
    memory: Uint8Array
    
    constructor(n: number) {
        this.currentIndex = BigInt(1);
        this.active = {};
        this.freed = [];    
        this.memory = new Uint8Array(n);
    }

    reset(){
        this.currentIndex = BigInt(1);
        this.active = {};
        this.freed = [];
    }
    
    alloc(length: BigInt) : BigInt {
        for (var i = 0; i < this.freed.length; i++) {
            let block = this.freed[i];
            if (block.length === length){
                this.active[Number(block.offset)] = block;
                this.freed.splice(i, 1);
                return block.offset;    
            } else if (block.length > length + BigInt(64)){
                const newBlock = {offset: block.offset, length};
                block.offset += length;
                block.length -= length;
                return newBlock.offset;
            }
        }
        
        // Resize memory if needed
        // TODO: put a limit on the memory size
        if (BigInt(this.memory.length) < this.currentIndex + length){
            const tmp = new Uint8Array(this.currentIndex + length + BigInt(64));
            tmp.set(this.memory);
            this.memory = tmp;
        }
        
        const offset = this.currentIndex;
        this.currentIndex += length;
        this.active[Number(offset)] = {offset, length};
        return offset;
    }
    
    getLength(offset: BigInt) : BigInt {
        const block = this.active[Number(offset)];
        if (!block){
            return BigInt(0);
        }
        
        return block.length;
    }
    
    free(offset: BigInt) {
        const block = this.active[Number(offset)];
        if (!block){
            return;    
        }
        
        delete this.active[Number(offset)];
        this.freed.push(block);
    }
}

function makeEnv(plugin: ExtismPluginCall): any {
    return {
        extism_alloc(n: BigInt): BigInt {
            return plugin.allocator.alloc(n);
        },
        extism_free (n: BigInt) {
            plugin.allocator.free(n);
        },
        extism_load_u8(n: BigInt): number { 
            return plugin.allocator.memory[Number(n)];
        },
        extism_load_u32(n: BigInt): number {debugger; return 0 },
        extism_load_u64(n: BigInt): BigInt { 
            let cast = new DataView(plugin.allocator.memory.buffer, Number(n));
            return cast.getBigUint64(0, true);
        },
        extism_store_u8(offset: BigInt, n: number) {
            //@ts-ignore
            plugin.allocator.memory[offset] = Number(n)
        },
        extism_store_u32(n: BigInt, i: number) {debugger; },
        extism_store_u64(offset: BigInt, n: BigInt) {
            const tmp = new DataView(plugin.allocator.memory.buffer, Number(offset));
            tmp.setBigUint64(0, n, true);
        },
        extism_input_length(): BigInt {
            //@ts-ignore
            return BigInt(plugin.input.length)
        },
        extism_input_load_u8(i: number): number {
            //@ts-ignore
            return plugin.input[i]
        },
        extism_input_load_u64(idx: BigInt) : BigInt {
            //@ts-ignore
            let cast = new DataView(plugin.input.buffer, Number(idx));
            return cast.getBigUint64(0, true);
        },
        extism_output_set(offset: BigInt, len: number): number {
            //@ts-ignore
            offset = Number(offset);
            len = Number(len)
            //@ts-ignore
            plugin.output = plugin.allocator.memory.slice(offset, offset+len)
            return 0
        },
        extism_error_set(i: BigInt) { debugger; },
        extism_config_get(i: BigInt): number { debugger; return 0 },
        extism_var_get(i: BigInt): number {debugger;return 0},
        extism_var_set(n: BigInt, i: BigInt) {debugger;},
        extism_http_request(n: BigInt, i: BigInt): number {debugger;return 0},
        extism_length(i: BigInt): BigInt { return plugin.allocator.getLength(i); },
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
    input: Uint8Array
    output: Uint8Array
    allocator: Allocator

    constructor(allocator: Allocator, input: Uint8Array, output: Uint8Array) {
        this.input = input
        this.output = output
        this.allocator = allocator
    }
}

class ExtismPlugin {
    moduleData: ArrayBuffer
    allocator: Allocator

    constructor(moduleData: ArrayBuffer) {
        this.moduleData = moduleData
        this.allocator = new Allocator(1024 * 1024)
    }

    async getExportedFunctions() {
        // we can make an empty call environment
        let empty = new Uint8Array()
        let call = new ExtismPluginCall(this.allocator, empty, empty)
        let module = await this._instantiateModule(call)
        return Object.keys(module.instance.exports).filter(f => !f.startsWith("__") && f !== "memory")
    }

    async call(func_name: string, input: Uint8Array | string): Promise<Uint8Array> {
        const output = new Uint8Array()
        let inputBytes: Uint8Array
        if (typeof input === 'string') {
            inputBytes = new TextEncoder().encode(input)
        } else if (input instanceof Uint8Array) {
            inputBytes = input
        } else {
            throw new Error("input should be string or Uint8Array")
        }
        this.allocator.reset();
        // TODO: only instantiate module once
        const call = new ExtismPluginCall(this.allocator, inputBytes, output)
        const module = await this._instantiateModule(call)
        let func = module.instance.exports[func_name]
        if (!func){
            throw Error(`function does not exist ${func_name}`)
        }
        //@ts-ignore
        func()
        return call.output
    }

    async _instantiateModule(call: ExtismPluginCall) {
        const environment = makeEnv(call)
        return await WebAssembly.instantiate(this.moduleData, { env: environment })
    }
}

