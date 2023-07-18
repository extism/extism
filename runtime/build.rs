fn main() {
    println!("cargo:rerun-if-changed=src/extism-runtime.wasm");
    let dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());

    // Attempt to build the kernel, this is only done as a convenience when developing the
    // kernel an should not be relied on. When changes are made to the kernel run
    // `sh build.sh` in the `kernel/` directory to ensure it run successfully.
    let _ = std::process::Command::new("bash")
        .args(&["build.sh"])
        .current_dir(dir.join("../kernel"))
        .status()
        .unwrap();

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
