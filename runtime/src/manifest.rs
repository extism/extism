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
    cache: &Cache,
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
            Ok((name, cache.precompile_or_get(&engine, &buf, hash)?))
        }
        extism_manifest::Wasm::Data { meta, data } => {
            let hash = check_hash(&meta.hash, data)?;
            Ok((
                meta.name.as_deref().unwrap_or("main").to_string(),
                cache.precompile_or_get(&engine, &data, hash)?,
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
                let hash = check_hash(&meta.hash, &data)?;

                // Convert fetched data to module
                let module = cache.precompile_or_get(&engine, &data, hash)?;

                Ok((name.to_string(), module))
            }
        }
    }
}

const WASM_MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];

pub(crate) fn load<'a>(
    engine: &Engine,
    input: WasmInput<'a>,
    cache: &Cache,
) -> Result<(extism_manifest::Manifest, BTreeMap<String, Module>), Error> {
    let mut mods = BTreeMap::new();
    mods.insert(
        EXTISM_ENV_MODULE.to_string(),
        cache.precompile_or_get(engine, WASM, None)?,
    );

    match input {
        WasmInput::Data(data) => {
            let has_magic = data.len() >= 4 && data[0..4] == WASM_MAGIC;
            let is_wat = data.starts_with(b"(module") || data.starts_with(b";;");
            if !has_magic && !is_wat {
                trace!("Loading manifest");
                if let Ok(s) = std::str::from_utf8(&data) {
                    let t = if let Ok(t) = toml::from_str::<extism_manifest::Manifest>(s) {
                        trace!("Manifest is TOML");
                        modules(engine, cache, &t, &mut mods)?;
                        t
                    } else if let Ok(t) = serde_json::from_str::<extism_manifest::Manifest>(s) {
                        trace!("Manifest is JSON");
                        modules(engine, cache, &t, &mut mods)?;
                        t
                    } else {
                        anyhow::bail!("Unknown manifest format");
                    };
                    return Ok((t, mods));
                }
            }

            let m = cache.precompile_or_get(engine, &data, None)?;

            mods.insert("main".to_string(), m);
            Ok((Default::default(), mods))
        }
        WasmInput::Manifest(m) => {
            trace!("Loading from existing manifest");
            modules(engine, cache, &m, &mut mods)?;
            Ok((m, mods))
        }
        WasmInput::ManifestRef(m) => {
            trace!("Loading from existing manifest");
            modules(engine, cache, &m, &mut mods)?;
            Ok((m.clone(), mods))
        }
    }
}

pub(crate) fn modules(
    engine: &Engine,
    cache: &Cache,
    manifest: &extism_manifest::Manifest,
    modules: &mut BTreeMap<String, Module>,
) -> Result<(), Error> {
    if manifest.wasm.is_empty() {
        return Err(anyhow::format_err!("No wasm files specified"));
    }

    // If there's only one module, it should be called `main`
    if manifest.wasm.len() == 1 {
        let (_, m) = to_module(engine, cache, &manifest.wasm[0])?;
        modules.insert("main".to_string(), m);
        return Ok(());
    }

    for f in &manifest.wasm {
        let (name, m) = to_module(engine, cache, f)?;
        trace!("Found module {}", name);
        modules.insert(name, m);
    }

    Ok(())
}

/// Handles caching compiled code
#[derive(Debug, Clone, Default)]
pub struct Cache {
    pub(crate) dir: Option<PathBuf>,
}

impl Cache {
    /// Create a new cache at the specified path
    pub fn new(dir: Option<PathBuf>) -> Cache {
        let dir = if dir.is_none() {
            if let Ok(d) = std::env::var("EXTISM_CACHE_DIR") {
                Some(PathBuf::from(d))
            } else {
                None
            }
        } else {
            dir
        };
        if let Some(dir) = &dir {
            let _ = std::fs::create_dir(&dir);
        }
        Cache { dir }
    }

    /// Path on disk, or `None` if caching is disabled
    pub fn path(&self) -> Option<&Path> {
        self.dir.as_deref()
    }

    /// Returns `true` when the cache is enabled
    pub fn enabled(&self) -> bool {
        self.dir.is_some()
    }

    /// Removes all plugins created before the specified date
    pub fn remove_created_before(&self, created: std::time::SystemTime) -> Result<(), Error> {
        if let Some(dir) = &self.dir {
            for file in std::fs::read_dir(&dir)? {
                if let Ok(file) = file {
                    if let Ok(metadata) = file.metadata() {
                        if metadata.created()? < created {
                            std::fs::remove_file(file.path())?;
                        }
                    } else {
                        let _ = std::fs::remove_file(file.path());
                    }
                }
            }
        }

        Ok(())
    }

    /// Removes all plugins that haven't been accessed since the specified date.
    /// NOTE: some systems may not support last access time for files, `Cache::remove_created_before`
    /// can be used instead.
    pub fn gc(&self, access: std::time::Duration) -> Result<(), Error> {
        if let Some(dir) = &self.dir {
            for file in std::fs::read_dir(&dir)? {
                if let Ok(file) = file {
                    if let Ok(metadata) = file.metadata() {
                        if metadata.accessed()? < std::time::SystemTime::now() - access {
                            std::fs::remove_file(file.path())?;
                        }
                    } else {
                        let _ = std::fs::remove_file(file.path());
                    }
                }
            }
        }

        Ok(())
    }

    /// Find a cached module from the original Wasm data
    pub fn find(&self, engine: &Engine, module: impl AsRef<[u8]>) -> Result<Option<Module>, Error> {
        if let Some(dir) = &self.dir {
            let digest = sha2::Sha256::digest(module.as_ref());
            let hash = hex(&digest);
            let path = dir.join(hash);
            if path.exists() {
                unsafe { Ok(Some(Module::from_trusted_file(&engine, path)?)) }
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    /// Precompile a Wasm module or get the cached pre-compiled module if it's available
    pub fn precompile_or_get<'a>(
        &self,
        engine: &Engine,
        data: &'a [u8],
        hash: Option<String>,
    ) -> Result<Module, Error> {
        let hash = hash.unwrap_or_else(|| {
            let digest = sha2::Sha256::digest(data);
            hex(&digest)
        });

        let has_magic = data.len() >= 4 && data[0..4] == WASM_MAGIC;
        let is_wat = data.starts_with(b"(module") || data.starts_with(b";;");

        if has_magic || is_wat {
            if let Some(dir) = &self.dir {
                let path = dir.join(hash);
                if !path.exists() {
                    let (m, compiled) = crate::compile(engine, data)?;
                    std::fs::write(&path, &compiled)?;
                    return Ok(m);
                }

                unsafe { Ok(Module::from_trusted_file(&engine, path)?) }
            } else {
                Ok(Module::new(&engine, data)?)
            }
        } else {
            trace!("Found precompiled Wasm module");
            unsafe { Ok(Module::deserialize(&engine, data)?) }
        }
    }
}
