use extism::*;
use sha2::Digest;
use std::fmt::Write;

fn hex(data: &[u8]) -> String {
    let mut s = String::new();
    for &byte in data {
        write!(&mut s, "{:02x}", byte).unwrap();
    }
    s
}

fn main() -> Result<(), Error> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("usage: extism-compile plugin.wasm path/to/cache");
        anyhow::bail!("Not enough arguments");
    }
    let input = std::fs::read(&args[0])?;
    let hash = sha2::Sha256::digest(&input);
    let engine = wasmtime::Engine::new(&DebugOptions::default().into())?;
    let (_, data) = compile(&engine, input)?;
    if args.len() > 1 {
        let path = std::path::PathBuf::from(&args[1]);
        if path.is_dir() {
            std::fs::write(path.join(hex(&hash)), data)?;
        } else {
            std::fs::write(&path, data)?;
        }
    } else {
        std::fs::write(
            std::path::PathBuf::from(&args[0]).with_extension("cwasm"),
            data,
        )?;
    }
    Ok(())
}
