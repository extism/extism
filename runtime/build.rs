fn main() {
    let fn_macro = "
#define EXTISM_FUNCTION(N) extern void N(ExtismCurrentPlugin*, const ExtismVal*, ExtismSize, ExtismVal*, ExtismSize, void*)
#define EXTISM_GO_FUNCTION(N) extern void N(void*, ExtismVal*, ExtismSize, ExtismVal*, ExtismSize, uintptr_t)

/** The return code from extism_plugin_call used to signal a successful call with no errors */
#define EXTISM_SUCCESS 0

/** An alias for I64 to signify an Extism pointer */
#define PTR I64
";
    if let Ok(x) = cbindgen::Builder::new()
        .with_crate(".")
        .with_language(cbindgen::Language::C)
        .with_no_includes()
        .with_sys_include("stdint.h")
        .with_sys_include("stdbool.h")
        .with_pragma_once(true)
        .with_cpp_compat(true)
        .with_after_include(fn_macro)
        .rename_item("Size", "ExtismSize")
        .rename_item("ValType", "ExtismValType")
        .rename_item("ValUnion", "ExtismValUnion")
        .rename_item("CurrentPlugin", "ExtismCurrentPlugin")
        .rename_item("CancelHandle", "ExtismCancelHandle")
        .rename_item("Plugin", "ExtismPlugin")
        .rename_item("Function", "ExtismFunction")
        .with_style(cbindgen::Style::Type)
        .generate()
    {
        x.write_to_file("extism.h");
    }
}
