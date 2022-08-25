use std::collections::BTreeMap;
use std::fmt::Write as FmtWrite;
use std::io::Read;

use sha2::Digest;

use crate::*;

#[derive(Default, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
pub struct Manifest(extism_manifest::Manifest);

fn hex(data: &[u8]) -> String {
    let mut s = String::new();
    for &byte in data {
        write!(&mut s, "{:02x}", byte).unwrap();
    }
    s
}

#[allow(unused)]
fn cache_add_file(hash: &str, data: &[u8]) -> Result<(), Error> {
    let cache_dir = std::env::temp_dir().join("exitsm-cache");
    let _ = std::fs::create_dir(&cache_dir);
    let file = cache_dir.join(hash);
    if file.exists() {
        return Ok(());
    }
    std::fs::write(file, data)?;
    Ok(())
}

fn cache_get_file(hash: &str) -> Result<Option<Vec<u8>>, Error> {
    let cache_dir = std::env::temp_dir().join("exitsm-cache");
    let file = cache_dir.join(hash);
    if file.exists() {
        let r = std::fs::read(file)?;
        return Ok(Some(r));
    }

    Ok(None)
}

fn check_hash(hash: &Option<String>, data: &[u8]) -> Result<(), Error> {
    match hash {
        None => Ok(()),
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
            Ok(())
        }
    }
}

fn hash_url(url: &str) -> String {
    let digest = sha2::Sha256::digest(url.as_bytes());
    hex(&digest)
}

fn to_module(
    engine: &Engine,
    wasm: &extism_manifest::ManifestWasm,
) -> Result<(String, Module), Error> {
    match wasm {
        extism_manifest::ManifestWasm::File { path, name, hash } => {
            if cfg!(not(feature = "register-filesystem")) {
                return Err(anyhow::format_err!("File-based registration is disabled"));
            }

            let name = match name {
                None => {
                    let name = path.with_extension("");
                    name.file_name().unwrap().to_string_lossy().to_string()
                }
                Some(n) => n.clone(),
            };

            let mut buf = Vec::new();
            let mut file = std::fs::File::open(path)?;
            file.read_to_end(&mut buf)?;

            check_hash(hash, &buf)?;

            Ok((name, Module::new(engine, buf)?))
        }
        extism_manifest::ManifestWasm::Data { name, data, hash } => {
            check_hash(hash, data)?;
            Ok((
                name.as_deref().unwrap_or("main").to_string(),
                Module::new(engine, data)?,
            ))
        }
        #[allow(unused)]
        extism_manifest::ManifestWasm::Url {
            name,
            url,
            header,
            method,
            hash,
        } => {
            let file_name = url.split('/').last().unwrap();
            let name = match name {
                Some(name) => name.as_str(),
                None => {
                    let mut name = "main";
                    if let Some(n) = file_name.strip_suffix(".wasm") {
                        name = n;
                    }

                    if let Some(n) = file_name.strip_suffix(".wast") {
                        name = n;
                    }
                    name
                }
            };

            let url_hash = hash_url(url);
            if let Some(h) = hash {
                if let Ok(Some(data)) = cache_get_file(h) {
                    check_hash(hash, &data)?;
                    let module = Module::new(engine, data)?;
                    return Ok((name.to_string(), module));
                }
            }

            #[cfg(not(feature = "register-http"))]
            {
                return Err(anyhow::format_err!("HTTP registration is disabled"));
            }

            #[cfg(feature = "register-http")]
            {
                let url_hash = hash_url(url);
                let mut req = ureq::request(method.as_deref().unwrap_or("GET"), url);

                for (k, v) in header.iter() {
                    req = req.set(k, v);
                }

                // Fetch WASM code
                let mut r = req.call()?.into_reader();
                let mut data = Vec::new();
                r.read_to_end(&mut data)?;

                if let Some(hash) = hash {
                    cache_add_file(hash, &data);
                }

                check_hash(hash, &data)?;

                // Convert fetched data to module
                let module = Module::new(engine, data)?;
                Ok((name.to_string(), module))
            }
        }
    }
}

const WASM_MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];

impl Manifest {
    pub fn new(engine: &Engine, data: &[u8]) -> Result<(Self, BTreeMap<String, Module>), Error> {
        let has_magic = data.len() >= 4 && data[0..4] == WASM_MAGIC;
        let is_wast = data.starts_with(b"(module") || data.starts_with(b";;");
        if !has_magic && !is_wast {
            if let Ok(t) = toml::from_slice::<Self>(data) {
                let m = t.modules(engine)?;
                return Ok((t, m));
            }

            if let Ok(t) = serde_json::from_slice::<Self>(data) {
                let m = t.modules(engine)?;
                return Ok((t, m));
            }
        }

        let m = Module::new(engine, data)?;
        let mut modules = BTreeMap::new();
        modules.insert("main".to_string(), m);
        Ok((Manifest::default(), modules))
    }

    fn modules(&self, engine: &Engine) -> Result<BTreeMap<String, Module>, Error> {
        if self.0.wasm.is_empty() {
            return Err(anyhow::format_err!("No wasm files specified"));
        }

        let mut modules = BTreeMap::new();
        if self.0.wasm.len() == 1 {
            let (_, m) = to_module(engine, &self.0.wasm[0])?;
            modules.insert("main".to_string(), m);
            return Ok(modules);
        }

        for f in &self.0.wasm {
            let (name, m) = to_module(engine, f)?;
            modules.insert(name, m);
        }

        Ok(modules)
    }
}

impl AsRef<extism_manifest::Manifest> for Manifest {
    fn as_ref(&self) -> &extism_manifest::Manifest {
        &self.0
    }
}

impl AsMut<extism_manifest::Manifest> for Manifest {
    fn as_mut(&mut self) -> &mut extism_manifest::Manifest {
        &mut self.0
    }
}
