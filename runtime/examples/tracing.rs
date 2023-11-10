use extism::*;

const WASM: &[u8] = include_bytes!("../../wasm/code.wasm");
fn main() -> Result<(), Error> {
    tracing_subscriber::fmt::init();
    tracing::info!("Hello from tracing!");
    let mut plugin = PluginBuilder::new(WASM).build()?;
    let _: () = plugin.call("count_vowels", "aaa")?;

    Ok(())
}
