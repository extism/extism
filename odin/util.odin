package extism

import "core:c"

when ODIN_OS == "windows" do foreign import Extism "extism.lib"
when ODIN_OS == "linux"   do foreign import Extism "extism.a"

foreign Extism {
    ExtismContext :: struct {}
    ExtismPlugin :: c.int32_t
    ExtismSize :: c.uint64_t
    extism_context_new :: proc() -> ^ExtismContext
    extism_context_free :: proc(ctx: ^ExtismContext) ---
    extism_context_reset :: proc(ctx: ^ExtismContext) ---
    extism_error :: proc(ctx ^ExtismContext, c.int32_t) -> c.string
    extism_log_file :: proc(filename, level) -> c.bool
    extism_plugin_config :: proc(ctx: ^ExtismContext, plugin: ExtismPlugin, json: ^c.uint8_t, json_size: ExtismSize) -> c.bool
    extism_plugin_call :: proc(plg_id: ExtismPlugin, funcName: cstring, input, ^c.uint8_t, data_len: ExtismSize) -> c.int32_t
    extism_plugin_free :: proc(ctx: ^ExtismContext, index: ExtismPlugin) ---
    extism_plugin_function_exists :: proc(ctx: ^ExtismContext, funcName: cstring) -> c.bool
    extism_plugin_new :: proc(ctx: ^ExtismContext, wasm: ^c.uint8_t, wasmSize: ExtismSize, withWasi: bool) -> ExtismPlugin
    extism_plugin_output_length :: proc(ctx: ^ExtismContext, plg_id: ExtismPlugin) -> ExtismSize
    extism_plugin_output_data :: proc(ctx: ^ExtismContext, plg_id: ExtismPlugin) -> c.uint8_t
    extism_plugin_output_length :: proc(ctx: ^ExtismContext, plg_id: ExtismPlugin) -> ExtismSize
    extism_plugin_output_data :: proc(ctx: ^ExtismContext, plg_id: ExtismPlugin) -> c.uint8_t
    extism_version :: proc() -> c.string
}

Error :: enum i32 {
    // an error occured reading the WASM module
    ReadWasm = 0
    // an error occured marshaling the config to JSON
    MarshalConfig
    // an error occured marshaling the manifest to JSON
    MarshalManifest
    // generic registering error
    Register
    // an error occured registering plugin
    RegisterPlugin
    // generic update error
    Update
    // an error occured updating plugin
    UpdatePlugin
    // an error occured registering plugin from manifest
    RegisterManifest
    // an error occured updating manifest
    UpdateManifest
    // an error occured calling plugin proc
    CallPlugin
    Empty = -1
}

extismVersion :: proc () -> string {
    return string(extism_version())
}

setLogFile :: proc(filename, level: string) -> bool {
    name := strings.clone_to_cstring(filename, context.temp_allocator)
    lvl := strings.clone_to_cstring(level, context.temp_allocator)
    r := extism_log_file(name, lvl)
    return bool(r)
}
