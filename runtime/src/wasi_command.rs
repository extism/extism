use crate::Error;
use crate::FromBytesOwned;
use crate::ToBytes;
use wasi_common::pipe::{ReadPipe, WritePipe};

pub struct WASICommand {
    engine: wasmtime::Engine,
    module: wasmtime::Module,
}

impl WASICommand {
    pub fn from(engine: wasmtime::Engine, module: wasmtime::Module) -> WASICommand {
        WASICommand { engine, module }
    }

    pub fn from_bytes(data: &[u8]) -> WASICommand {
        let config = wasmtime::Config::new();
        let engine = wasmtime::Engine::new(&config).unwrap();
        let module = wasmtime::Module::from_binary(&engine, data).unwrap();
        WASICommand::from(engine, module)
    }

    pub fn run<'a, T: ToBytes<'a>, U: FromBytesOwned, V: FromBytesOwned>(
        &self,
        envs: &[(String, String)],
        args: &[String],
        stdin: T,
    ) -> Result<(U, V), Error> {
        let bytes = stdin.to_bytes()?;
        let stdin = ReadPipe::from(bytes.as_ref());
        let stdout = WritePipe::new_in_memory();
        let stderr = WritePipe::new_in_memory();
        let wasi_ctx = wasmtime_wasi::WasiCtxBuilder::new()
            .args(args)?
            .envs(envs)?
            .stdin(Box::new(stdin.clone()))
            .stdout(Box::new(stdout.clone()))
            .stderr(Box::new(stderr.clone()))
            .build();

        let mut store = wasmtime::Store::new(&self.engine, wasi_ctx);
        let mut linker = wasmtime::Linker::new(&self.engine);
        wasmtime_wasi::add_to_linker(&mut linker, |wasi| wasi)?;
        let instance = linker.instantiate(&mut store, &self.module)?;
        let function_name = "_start";
        let f = instance
            .get_func(&mut store, function_name)
            .expect("function exists");
        f.call(&mut store, &[], &mut []).unwrap();
        drop(store);
        let contents: Vec<u8> = stdout.try_into_inner().unwrap().into_inner();
        let stderr_contents: Vec<u8> = stderr.try_into_inner().unwrap().into_inner();
        let stdout_output = U::from_bytes_owned(&contents)?;
        let stderr_output = V::from_bytes_owned(&stderr_contents)?;
        Ok((stdout_output, stderr_output))
    }
}
