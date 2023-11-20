use std::borrow::Cow;

fn rewrite(line: &'_ str) -> Option<Cow<'_, str>> {
    let line = line.trim();

    if line.is_empty() {
        return None;
    }

    if line.starts_with('#') {
        return None;
    }

    if cfg!(target_os = "macos") {
        if line.starts_with("typedef __builtin_va_list ") {
            return None;
        }
    } else if cfg!(target_os = "windows") {
        if line.contains("__gnuc_va_list")
            || line.starts_with("__pragma")
            || line.contains("__attribute__")
            || line.contains("uintptr_t")
            || line.contains("intptr_t")
            || line.contains("size_t")
            || line.contains("ptrdiff_t")
        {
            return None;
        }

        return Some(Cow::Owned(line.replace("__attribute__((__cdecl__))", "")));
    };

    Some(Cow::Borrowed(line))
}

fn main() {
    println!("cargo:rerun-if-changed=src/extism.c");
    println!("cargo:rerun-if-changed=../runtime/extism.h");
    std::fs::copy("../runtime/extism.h", "src/extism.h").unwrap();

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
        .filter_map(rewrite)
        .collect::<Vec<Cow<'_, str>>>()
        .join("\n\n");

    std::fs::write("../target/header.h", data).unwrap();
}
