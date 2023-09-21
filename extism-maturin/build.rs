fn filter_lines(line: &&str) -> bool {
    let line = line.trim();

    if line.is_empty() {
        return false;
    }

    let not_macro = line.chars().nth(0).unwrap() != '#';

    if cfg!(target_os = "macos") {
        return not_macro && !line.starts_with("typedef __builtin_va_list ");
    } else if cfg!(target_os = "windows") {
        return not_macro
            && !line.starts_with("__pragma")
            && !line.contains("uintptr_t")
            && !line.contains("intptr_t")
            && !line.contains("size_t")
            && !line.contains("ptrdiff_t");
    }

    not_macro
}

fn main() {
    println!("cargo:rerun-if-changed=src/extism.c");
    println!("cargo:rerun-if-changed=../runtime/extism.h");

    let data = String::from_utf8(
        cc::Build::new()
            .file("src/extism.c")
            .warnings(false)
            .extra_warnings(false)
            .expand(),
    )
    .unwrap();
    let data: Vec<&str> = data.split('\n').collect();
    let data: String = data
        .into_iter()
        .filter(filter_lines)
        .collect::<Vec<&str>>()
        .join("\n\n");

    std::fs::create_dir_all("target").unwrap();
    std::fs::write("target/header.h", data).unwrap();
}

