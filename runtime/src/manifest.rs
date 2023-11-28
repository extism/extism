use std::collections::BTreeMap;
use std::fmt::Write as FmtWrite;
use std::io::Read;

use sha2::Digest;

use crate::plugin::WasmInput;
use crate::*;

fn hex(data: &[u8]) -> String {
    let mut s = String::new();
    for &byte in data {
        write!(&mut s, "{:02x}", byte).unwrap();
    }
    s
}

fn check_hash(hash: &Option<String>, data: &[u8]) -> Result<Option<String>, Error> {
    match hash {
        None => Ok(None),
        Some(hash) => {
            let digest = sha2::Sha256::digest(data);
            let hex = hex(&digest);
            if &hex != hash {
                return Err(anyhow::format_err!(
                    "Hash mismatch, found {} but expected {}",
                    hex,
                    hash
                ));
            }
            Ok(Some(hex))
        }
    }
}

const WASM: &[u8] = include_bytes!("extism-runtime.wasm");

/// Convert from manifest to a wasmtime Module
fn to_module(engine: &Engine, wasm: &extism_manifest::Wasm) -> Result<(String, Module), Error> {
    match wasm {
        extism_manifest::Wasm::File { path, meta } => {
            if cfg!(not(feature = "register-filesystem")) {
                return Err(anyhow::format_err!("File-based registration is disabled"));
            }

            // Figure out a good name for the file
            let name = match &meta.name {
                None => {
                    let name = path.with_extension("");
                    name.file_name().unwrap().to_string_lossy().to_string()
                }
                Some(n) => n.clone(),
            };

            // Load file
            let mut buf = Vec::new();
            let mut file = std::fs::File::open(path)?;
            file.read_to_end(&mut buf)?;

            check_hash(&meta.hash, &buf)?;
            Ok((name, Module::new(engine, buf)?))
        }
        extism_manifest::Wasm::Data { meta, data } => {
            check_hash(&meta.hash, data)?;
            Ok((
                meta.name.as_deref().unwrap_or("main").to_string(),
                Module::new(engine, data)?,
            ))
        }
        #[allow(unused)]
        extism_manifest::Wasm::Url {
            req:
                extism_manifest::HttpRequest {
                    url,
                    headers,
                    method,
                },
            meta,
        } => {
            // Get the file name
            let file_name = url.split('/').last().unwrap_or_default();
            let name = match &meta.name {
                Some(name) => name.as_str(),
                None => {
                    let mut name = "main";
                    if let Some(n) = file_name.strip_suffix(".wasm") {
                        name = n;
                    }

                    if let Some(n) = file_name.strip_suffix(".wat") {
                        name = n;
                    }
                    name
                }
            };

            #[cfg(not(feature = "register-http"))]
            {
                return anyhow::bail!("HTTP registration is disabled");
            }

            #[cfg(feature = "register-http")]
            {
                // Setup request
                let mut req = ureq::request(method.as_deref().unwrap_or("GET"), url);

                for (k, v) in headers.iter() {
                    req = req.set(k, v);
                }

                // Fetch WASM code
                let mut r = req.call()?.into_reader();
                let mut data = Vec::new();
                r.read_to_end(&mut data)?;

                // Check hash against manifest
                check_hash(&meta.hash, &data)?;

                // Convert fetched data to module
                let module = Module::new(engine, data)?;

                Ok((name.to_string(), module))
            }
        }
    }
}

const WASM_MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];

pub(crate) fn load(
    engine: &Engine,
    input: WasmInput<'_>,
) -> Result<(extism_manifest::Manifest, BTreeMap<String, Module>), Error> {
    let mut mods = BTreeMap::new();
    mods.insert(EXTISM_ENV_MODULE.to_string(), Module::new(engine, WASM)?);

    match input {
        WasmInput::Data(data) => {
            let has_magic = data.len() >= 4 && data[0..4] == WASM_MAGIC;
            let is_wat = data.starts_with(b"(module") || data.starts_with(b";;");
            if !has_magic && !is_wat {
                trace!("Loading manifest");
                if let Ok(s) = std::str::from_utf8(&data) {
                    let t = if let Ok(t) = toml::from_str::<extism_manifest::Manifest>(s) {
                        trace!("Manifest is TOML");
                        modules(engine, &t, &mut mods)?;
                        t
                    } else if let Ok(t) = serde_json::from_str::<extism_manifest::Manifest>(s) {
                        trace!("Manifest is JSON");
                        modules(engine, &t, &mut mods)?;
                        t
                    } else {
                        anyhow::bail!("Unknown manifest format");
                    };
                    return Ok((t, mods));
                }
            }

            let m = Module::new(engine, data)?;
            mods.insert("main".to_string(), m);
            Ok((Default::default(), mods))
        }
        WasmInput::Manifest(m) => {
            trace!("Loading from existing manifest");
            modules(engine, &m, &mut mods)?;
            Ok((m, mods))
        }
        WasmInput::ManifestRef(m) => {
            trace!("Loading from existing manifest");
            modules(engine, m, &mut mods)?;
            Ok((m.clone(), mods))
        }
    }
}

pub(crate) fn modules(
    engine: &Engine,
    manifest: &extism_manifest::Manifest,
    modules: &mut BTreeMap<String, Module>,
) -> Result<(), Error> {
    if manifest.wasm.is_empty() {
        return Err(anyhow::format_err!("No wasm files specified"));
    }

    // If there's only one module, it should be called `main`
    if manifest.wasm.len() == 1 {
        let (_, m) = to_module(engine, &manifest.wasm[0])?;
        modules.insert("main".to_string(), m);
        return Ok(());
    }

    for f in &manifest.wasm {
        let (name, m) = to_module(engine, f)?;
        trace!("Found module {}", name);
        modules.insert(name, m);
    }

    Ok(())
}
