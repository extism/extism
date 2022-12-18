fn main() {
    if let Ok(bindings) = cbindgen::Builder::new()
        .with_crate(".")
        .with_language(cbindgen::Language::C)
        .with_no_includes()
        .with_sys_include("stdint.h")
        .with_sys_include("stdbool.h")
        .with_pragma_once(true)
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
