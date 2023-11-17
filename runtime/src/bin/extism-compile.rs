use extism::*;

fn main() -> Result<(), Error> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("usage: extism-compile plugin.wasm plugin.cwasm");
        anyhow::bail!("Not enough arguments");
    }
    let input = std::fs::read(&args[0])?;
    let data = compile(input, None)?;
    if args.len() > 1 {
        std::fs::write(&args[1], data)?;
    } else {
        std::fs::write(
            std::path::PathBuf::from(&args[0]).with_extension("cwasm"),
            data,
        )?;
    }
    Ok(())
}
