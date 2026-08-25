use std::env;
use std::path::PathBuf;

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let src = manifest_dir.join("wasm3");

    println!("cargo:rerun-if-changed={}", src.display());
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=VERSION");

    let mut build = cc::Build::new();
    build.include(&src);
    build.file(src.join("m3_bind.c"));
    build.file(src.join("m3_code.c"));
    build.file(src.join("m3_compile.c"));
    build.file(src.join("m3_core.c"));
    build.file(src.join("m3_env.c"));
    build.file(src.join("m3_exec.c"));
    build.file(src.join("m3_function.c"));
    build.file(src.join("m3_info.c"));
    build.file(src.join("m3_module.c"));
    build.file(src.join("m3_parse.c"));
    build.file(src.join("m3_validate.c"));

    let wasi = env::var("CARGO_FEATURE_WASI").is_ok();
    if wasi {
        build.define("d_m3HasWASI", None);
        build.file(src.join("m3_api_wasi.c"));
        build.file(src.join("m3_api_libc.c"));
    }

    // gnu99: m3_exec.c uses computed goto / other GNU extensions.
    build.flag_if_supported("-std=gnu99");
    build.flag_if_supported("-Wno-unused-function");
    build.flag_if_supported("-Wno-unused-variable");
    build.flag_if_supported("-Wno-unused-parameter");
    build.flag_if_supported("-Wno-missing-field-initializers");
    build.flag_if_supported("-Wno-sign-compare");
    build.flag_if_supported("-Wno-parentheses");
    build.flag_if_supported("-fomit-frame-pointer");

    if env::var("PROFILE").as_deref() == Ok("release") {
        build.opt_level(3);
        build.define("NDEBUG", Some("1"));
    } else {
        build.define("DEBUG", Some("1"));
    }

    build.warnings(false);
    build.compile("wasm3");

    let target = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target != "windows" {
        println!("cargo:rustc-link-lib=m");
    }

    let version =
        std::fs::read_to_string(manifest_dir.join("VERSION")).unwrap_or_else(|_| "unknown".into());
    println!("cargo:rustc-env=WASM3_VERSION={}", version.trim());
}
