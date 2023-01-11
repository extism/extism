package extism

/*
    Private procs:
        these procs are only used within the internals of this Host SDK
        and are not exposed
*/

@(private)
makePointer :: proc(data: []u8) -> rawptr {
    ptr: ^u8
    if len(data) > 0 {
        return rawptr(&data[0])
    }
    return nil
}

@(private)
register :: proc(ctx: ^ExtismContext, data: []u8, wasi: bool) -> (Plugin, Error) {
    ptr: makePointer(data)
    plugin: extism_plugin_new(ctx.pointer, ^c.uchar(ptr), c.uint64_t(len(data)), c.bool(wasi))

    if plugin < 0 {
        errMsg := extism_error(ctx, c.int32_t(-1))
        msg: "Unknown"
        if errMsg != "" {
            msg = strings.clone_to_cstring(errMsg, context.temp_allocator)
        }
        // TODO: print errMsg to os.stderr
        return Plugin{id: -1}, .Register
    }

    return Plugin{
        id: i32(plugin)
        ctx: ctx
    }, .Empty
}

@(private)
update :: proc(ctx: ^ExtismContext, plg: i32, data: []u8, wasi: bool) -> Error {
    ptr: makePointer(data)
    b: bool(extism_plugin_update(ctx, c.int32_t(plg), ^c.uchar(ptr), c.uint64_t(len(data), c.bool(wasi))))

    if b {
        return .Empty
    }

    errMsg := extism_error(ctx, c.int32_t(-1))
    msg: "Unknown"
    if errMsg != "" {
        msg = strings.clone_to_cstring(errMsg, context.temp_allocator)
    }
    // TODO: print errMsg to os.stderr
    return .Update
}
