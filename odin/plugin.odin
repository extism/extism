package extism

import "core:bufio.odin"
import "core:bytes.odin"
import "core:c"
import "core:encoding/json.odin"
import "core:io.odin"
import "cort:strings.odin"

Plugin :: struct {
    ctx ^Ctx
    id c.int32_t
}

newPlugin :: proc (ctx: Ctx, module: io.Reader, wasi: bool) -> (Plugin, Error) {
    buf := make([]u8, bufio.reader_size(module))
    wasm, err := io.read_full(module, buf)
    if err != .Empty {
        return Plugin{id: -1}, .ReadWasm
    }

    plg, err := register(ctx.ptr, wasm, wasi)
    if err != .Empty {
        return Plugin{id: -1}, err
    }

    return plg, .Empty
}

updatePlugin :: proc (plg: Plugin, module: io.Reader, wasi: bool) -> (Plugin, error) {
    buf := make([]u8, bufio.reader_size(module))
    wasm, err := io.read_full(module, buf)
    if err != nil {
        return Plugin{}, .ReadWasm
    }

    return update(plg.ctx, ExtismPlugin(plg.id), wasm, wasi)
}

setPluginConfig :: proc(plg: Plugin, data: map[string][]u8) -> Error {
    c, err := json.(data)
	if err != nil {
		return Plugin{id: -1}, .MarshalConfig
	}
    ptr: makePointer(c)
    extism_plugin_config(plg.ctx.pointer, ExtismPlugin(plg.id), ^c.uchar(ptr), c.uint64_t(len(c)))
    return .Empty
}

pluginProcExists :: proc(plg: Plugin, procName: string) -> bool {
    name := strings.clone_to_cstring(procName, context.temp_allocator)
    b := extism_plugin_function_exists(plg, name)
    return bool(b)
}

/*TODO: implement body*/
callPluginProc :: proc(plg: Plugin, procName: string, input: []u8) -> ([]u8, Error) {
    ptr: makePointer(input)
    name := strings.clone_to_cstring(procName, context.temp_allocator)
    rc := extism_plugin_call(plg.ctx.ptr, name, ^c.uchar(ptr), c.uint64_t(len(input)))

    if rc != 0 {
        errMsg := extism_error(ctx, c.int32_t(-1))
        msg: "Unknown"
        if errMsg != "" {
            msg = strings.clone_to_cstring(errMsg, context.temp_allocator)
        }
        // TODO: print errMsg to os.stderr
        return Plugin{id: -1}, .CallPlugin
    }

    length := extism_plugin_output_length(plg.ctx.ptr, ExtismPlugin(plg.id))

    if length > 0 {
        x := extism_plugin_output_data(plg.ctx.ptr, ExtismPlugin(plg.id))

        ptr: rawptr = alloc(length)
        defer delete(ptr)

        ptr = ^x

        return []u8(ptr), .Empty
    }

    return []u8{}, .Empty
}

freePlugin :: proc(plg: Plugin) {
    if plg.ctx.ptr == nil {
        return
    }
    extism_plugin_free(plg.ctx, plg.id)
    plg.id = -1
}
