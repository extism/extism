fn main() {
    println!("cargo:rustc-link-search=/usr/local/lib");

    if let Ok(home) = std::env::var("HOME") {
        let path = std::path::PathBuf::from(home).join(".local").join("lib");
        println!("cargo:rustc-link-search={}", path.display());
    }

    println!("cargo:rustc-link-lib=extism");
}
