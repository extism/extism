fn main() {
    let manifest_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    println!("{:?}", manifest_dir);
    std::process::Command::new("cargo")
        .args(&[
            "build",
            "--release",
            "--target",
            "wasm32-unknown-unknown",
            "-p",
            "extism-runtime-kernel",
        ])
        .current_dir(manifest_dir.join("../kernel"))
        .status()
        .unwrap();

    std::fs::copy(
        manifest_dir.join("../kernel/target/wasm32-unknown-unknown/release/extism-runtime.wasm"),
        manifest_dir.join("src/extism-runtime.wasm"),
    )
    .unwrap();

    println!("cargo:rerun-if-changed=src/extism-runtime.wasm");

    let fn_macro = "
#define EXTISM_FUNCTION(N) extern void N(ExtismCurrentPlugin*, const ExtismVal*, ExtismSize, ExtismVal*, ExtismSize, void*)
#define EXTISM_GO_FUNCTION(N) extern void N(void*, ExtismVal*, ExtismSize, ExtismVal*, ExtismSize, uintptr_t)
";
    if let Ok(bindings) = cbindgen::Builder::new()
        .with_crate(".")
        .with_language(cbindgen::Language::C)
        .with_no_includes()
        .with_sys_include("stdint.h")
        .with_sys_include("stdbool.h")
        .with_pragma_once(true)
        .with_after_include(fn_macro)
        .rename_item("Size", "ExtismSize")
        .rename_item("PluginIndex", "ExtismPlugin")
        .rename_item("Context", "ExtismContext")
        .rename_item("ValType", "ExtismValType")
        .rename_item("ValUnion", "ExtismValUnion")
        .rename_item("Internal", "ExtismCurrentPlugin")
        .with_style(cbindgen::Style::Type)
        .generate()
    {
        bindings.write_to_file("extism.h");
    }
}
