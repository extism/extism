use std::env;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    if let Ok(bindings) = cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_language(cbindgen::Language::C)
        .with_no_includes()
        .with_sys_include("stdint.h")
        .with_sys_include("stdbool.h")
        .with_pragma_once(true)
        .rename_item("Size", "ExtismSize")
        .rename_item("PluginIndex", "ExtismPlugin")
        .generate()
    {
        bindings.write_to_file("extism.h");
    }
}
