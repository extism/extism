fn main() {
    let fn_macro = "#define EXTISM_FUNCTION(N) extern void N(ExtismCurrentPlugin*, const ExtismVal*, ExtismSize, ExtismVal*, ExtismSize, void*)";
    let go_fn_macro = "#define EXTISM_GO_FUNCTION(N) extern void N(ExtismCurrentPlugin*, ExtismVal*, ExtismSize, ExtismVal*, ExtismSize, void*)";
    if let Ok(bindings) = cbindgen::Builder::new()
        .with_crate(".")
        .with_language(cbindgen::Language::C)
        .with_no_includes()
        .with_sys_include("stdint.h")
        .with_sys_include("stdbool.h")
        .with_pragma_once(true)
        .with_after_include(fn_macro)
        .with_after_include(go_fn_macro)
        .rename_item("Size", "ExtismSize")
        .rename_item("PluginIndex", "ExtismPlugin")
        .rename_item("Context", "ExtismContext")
        .rename_item("ValType", "ExtismValType")
        .rename_item("Plugin", "ExtismCurrentPlugin")
        .generate()
    {
        bindings.write_to_file("extism.h");
    }
}
