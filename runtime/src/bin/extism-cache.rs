use extism::*;
use sha2::Digest;
use std::{fmt::Write, path::PathBuf};

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
        eprintln!("usage:\n\textism-cache add path/to/cache plugin.wasm\n\textism-cache gc path/to/cache <num_days>\n\t");
        anyhow::bail!("Not enough arguments");
    }
    let cmd = &args[0];
    let cache = Cache::new(Some(PathBuf::from(&args[1])));
    match cmd.as_str() {
        "add" => {
            let input = std::fs::read(&args[2])?;
            let hash = sha2::Sha256::digest(&input);
            let engine = wasmtime::Engine::new(&DebugOptions::default().into())?;
            let (_, data) = compile(&engine, input)?;
            if args.len() > 2 {
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
        }
        "gc" => {
            let days: u64 = args[1].parse()?;
            let d = std::time::Duration::from_secs(24 * 60 * 60 * days);
            cache.gc(d)?;
        }
        name => {
            eprintln!("Invalid command: {name}")
        }
    }
    Ok(())
}
