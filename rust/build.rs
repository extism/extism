fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    if std::path::PathBuf::from("libextism.so").exists() {
        std::process::Command::new("cp")
            .arg("libextism.so")
            .arg(&out_dir)
            .status()
            .unwrap();
    } else {
        std::process::Command::new("cp")
            .arg("libextism.dylib")
            .arg(&out_dir)
            .status()
            .unwrap();
    }
    println!("cargo:rustc-link-search={}", out_dir);
    println!("cargo:rustc-link-lib=extism");
    println!("cargo:rerun-if-changed=libextism.so");
    println!("cargo:rerun-if-changed=libextism.dylib");
}
