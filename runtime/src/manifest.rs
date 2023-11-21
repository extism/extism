use std::collections::BTreeMap;
use std::fmt::Write as FmtWrite;
use std::io::Read;
use std::path::{Path, PathBuf};

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

#[allow(unused)]
fn cache_add_file(hash: &str, data: &[u8]) -> Result<(), Error> {
    let cache_dir = std::env::temp_dir().join("extism-cache");
    let _ = std::fs::create_dir(&cache_dir);
    let file = cache_dir.join(hash);
    if file.exists() {
        return Ok(());
    }
    std::fs::write(file, data)?;
    Ok(())
}

fn cache_get_file(hash: &str) -> Result<Option<Vec<u8>>, Error> {
    let cache_dir = std::env::temp_dir().join("extism-cache");
    let file = cache_dir.join(hash);
    if file.exists() {
        let r = std::fs::read(file)?;
        return Ok(Some(r));
    }

    Ok(None)
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
fn to_module(
    engine: &Engine,
    cache_dir: Option<&Path>,
    debug_opts: &DebugOptions,
    wasm: &extism_manifest::Wasm,
) -> Result<(String, Module), Error> {
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

            let hash = check_hash(&meta.hash, &buf)?;
            Ok((
                name,
                precompile_or_get_cached(&engine, cache_dir, debug_opts, &buf, hash)?,
            ))
        }
        extism_manifest::Wasm::Data { meta, data } => {
            let hash = check_hash(&meta.hash, data)?;
            Ok((
                meta.name.as_deref().unwrap_or("main").to_string(),
                precompile_or_get_cached(&engine, cache_dir, debug_opts, &data, hash)?,
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

            if let Some(h) = &meta.hash {
                if let Ok(Some(data)) = cache_get_file(h) {
                    let hash = check_hash(&meta.hash, &data)?;
                    let module =
                        precompile_or_get_cached(&engine, cache_dir, debug_opts, &data, hash)?;
                    return Ok((name.to_string(), module));
                }
            }

            #[cfg(not(feature = "register-http"))]
            {
                return Err(anyhow::format_err!("HTTP registration is disabled"));
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

                // Try to cache file
                if let Some(hash) = &meta.hash {
                    cache_add_file(hash, &data);
                }

                let hash = check_hash(&meta.hash, &data)?;

                // Convert fetched data to module
                let module = precompile_or_get_cached(&engine, cache_dir, debug_opts, &data, hash)?;

                Ok((name.to_string(), module))
            }
        }
    }
}

const WASM_MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];

fn precompile_or_get_cached<'a>(
    engine: &Engine,
    dir: Option<&Path>,
    debug_opts: &DebugOptions,
    data: &'a [u8],
    hash: Option<String>,
) -> Result<Module, Error> {
    let hash = hash.unwrap_or_else(|| {
        let digest = sha2::Sha256::digest(data);
        hex(&digest)
    });

    if let Some(dir) = dir {
        let path = dir.join(hash);
        if !path.exists() {
            let (m, compiled) = crate::compile(data, Some(debug_opts.clone()))?;
            std::fs::write(&path, &compiled)?;
            return Ok(m);
        }

        unsafe { Ok(Module::from_trusted_file(&engine, path)?) }
    } else {
        Ok(Module::new(engine, data)?)
    }
}

pub(crate) fn load<'a>(
    engine: &Engine,
    input: WasmInput<'a>,
    cache_dir: Option<PathBuf>,
    debug_opts: &DebugOptions,
) -> Result<(extism_manifest::Manifest, BTreeMap<String, Module>), Error> {
    if let Some(dir) = &cache_dir {
        let _ = std::fs::create_dir(&dir);
    }
    let mut mods = BTreeMap::new();
    mods.insert(
        EXTISM_ENV_MODULE.to_string(),
        precompile_or_get_cached(engine, cache_dir.as_deref(), debug_opts, WASM, None)?,
    );

    match input {
        WasmInput::Data(data) => {
            let has_magic = data.len() >= 4 && data[0..4] == WASM_MAGIC;
            let is_wast = data.starts_with(b"(module") || data.starts_with(b";;");
            if !has_magic && !is_wast {
                trace!("Loading manifest");
                if let Ok(s) = std::str::from_utf8(&data) {
                    let t = if let Ok(t) = toml::from_str::<extism_manifest::Manifest>(s) {
                        trace!("Manifest is TOML");
                        modules(engine, cache_dir.as_deref(), debug_opts, &t, &mut mods)?;
                        t
                    } else if let Ok(t) = serde_json::from_str::<extism_manifest::Manifest>(s) {
                        trace!("Manifest is JSON");
                        modules(engine, cache_dir.as_deref(), debug_opts, &t, &mut mods)?;
                        t
                    } else {
                        anyhow::bail!("Unknown manifest format");
                    };
                    return Ok((t, mods));
                }
            }

            let m = if !has_magic {
                trace!("Deserializing module");
                unsafe { Module::deserialize(engine, data)? }
            } else {
                trace!("Loading WASM module bytes");
                precompile_or_get_cached(engine, cache_dir.as_deref(), debug_opts, WASM, None)?
            };

            mods.insert("main".to_string(), m);
            Ok((Default::default(), mods))
        }
        WasmInput::Manifest(m) => {
            trace!("Loading from existing manifest");
            modules(engine, cache_dir.as_deref(), debug_opts, &m, &mut mods)?;
            Ok((m, mods))
        }
        WasmInput::ManifestRef(m) => {
            trace!("Loading from existing manifest");
            modules(engine, cache_dir.as_deref(), debug_opts, &m, &mut mods)?;
            Ok((m.clone(), mods))
        }
    }
}

pub(crate) fn modules(
    engine: &Engine,
    cache_dir: Option<&Path>,
    debug_opts: &DebugOptions,
    manifest: &extism_manifest::Manifest,
    modules: &mut BTreeMap<String, Module>,
) -> Result<(), Error> {
    if manifest.wasm.is_empty() {
        return Err(anyhow::format_err!("No wasm files specified"));
    }

    // If there's only one module, it should be called `main`
    if manifest.wasm.len() == 1 {
        let (_, m) = to_module(engine, cache_dir, debug_opts, &manifest.wasm[0])?;
        modules.insert("main".to_string(), m);
        return Ok(());
    }

    for f in &manifest.wasm {
        let (name, m) = to_module(engine, cache_dir, debug_opts, f)?;
        trace!("Found module {}", name);
        modules.insert(name, m);
    }

    Ok(())
}
