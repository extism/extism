//! Raw `wasm3.h` FFI for Wasm3 v0.9.0.
//!
//! Hand-written so we do not need libclang/bindgen at build time.

#![allow(non_camel_case_types, non_snake_case, clippy::missing_safety_doc)]

use std::os::raw::{c_char, c_void};

pub type M3Result = *const c_char;

pub type IM3Environment = *mut M3Environment;
pub type IM3Runtime = *mut M3Runtime;
pub type IM3Module = *mut M3Module;
pub type IM3Function = *mut M3Function;
pub type IM3Global = *mut M3Global;
pub type IM3BacktraceFrame = *mut M3BacktraceFrame;
pub type IM3BacktraceInfo = *mut M3BacktraceInfo;
pub type IM3TaggedValue = *mut M3TaggedValue;
pub type IM3ImportContext = *mut M3ImportContext;

#[repr(C)]
pub struct M3Environment {
    _opaque: [u8; 0],
}

#[repr(C)]
pub struct M3Runtime {
    _opaque: [u8; 0],
}

#[repr(C)]
pub struct M3Module {
    _opaque: [u8; 0],
}

#[repr(C)]
pub struct M3Function {
    _opaque: [u8; 0],
}

#[repr(C)]
pub struct M3Global {
    _opaque: [u8; 0],
}

#[repr(C)]
pub struct M3ErrorInfo {
    pub result: M3Result,
    pub runtime: IM3Runtime,
    pub module: IM3Module,
    pub function: IM3Function,
    pub file: *const c_char,
    pub line: u32,
    pub message: *const c_char,
}

#[repr(C)]
pub struct M3BacktraceFrame {
    pub module_offset: u32,
    pub function: IM3Function,
    pub next: IM3BacktraceFrame,
}

#[repr(C)]
pub struct M3BacktraceInfo {
    pub frames: IM3BacktraceFrame,
    pub last_frame: IM3BacktraceFrame,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum M3ValueType {
    None = 0,
    I32 = 1,
    I64 = 2,
    F32 = 3,
    F64 = 4,
    V128 = 5,
    FuncRef = 6,
    ExternRef = 7,
    /// Sentinel from `wasm3.h` (`c_m3Type_unknown` / `c_m3Type_count`).
    Unknown = 8,
}

#[repr(C)]
pub union M3ValueUnion {
    pub i32: u32,
    pub i64: u64,
    pub f32: f32,
    pub f64: f64,
    pub ref_: *const c_void,
}

#[repr(C)]
pub struct M3TaggedValue {
    pub type_: M3ValueType,
    pub value: M3ValueUnion,
}

#[repr(C)]
pub struct M3ImportContext {
    pub userdata: *mut c_void,
    pub function: IM3Function,
}

/// Host function. Return `null` on success, or an error string / `m3Err_*`.
///
/// `_sp[0 .. nreturns)` are result slots; arguments follow. `_sp` advances in
/// 8-byte slots even for i32.
pub type M3RawCall = unsafe extern "C" fn(
    runtime: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    mem: *mut c_void,
) -> M3Result;

extern "C" {
    pub fn m3_NewEnvironment() -> IM3Environment;
    pub fn m3_FreeEnvironment(i_environment: IM3Environment);

    pub fn m3_NewRuntime(
        io_environment: IM3Environment,
        i_stackSizeInBytes: u32,
        i_userdata: *mut c_void,
    ) -> IM3Runtime;
    pub fn m3_FreeRuntime(i_runtime: IM3Runtime);

    pub fn m3_GetMemory(
        i_runtime: IM3Runtime,
        o_memorySizeInBytes: *mut u32,
        i_memoryIndex: u32,
    ) -> *mut u8;
    pub fn m3_GetMemorySize(i_runtime: IM3Runtime) -> u32;
    pub fn m3_GetUserData(i_runtime: IM3Runtime) -> *mut c_void;

    pub fn m3_ParseModule(
        i_environment: IM3Environment,
        o_module: *mut IM3Module,
        i_wasmBytes: *const u8,
        i_numWasmBytes: u32,
    ) -> M3Result;
    pub fn m3_FreeModule(i_module: IM3Module);
    pub fn m3_LoadModule(io_runtime: IM3Runtime, io_module: IM3Module) -> M3Result;
    pub fn m3_CompileModule(io_module: IM3Module) -> M3Result;
    pub fn m3_RunStart(i_module: IM3Module) -> M3Result;

    pub fn m3_LinkRawFunction(
        io_module: IM3Module,
        i_moduleName: *const c_char,
        i_functionName: *const c_char,
        i_signature: *const c_char,
        i_function: M3RawCall,
    ) -> M3Result;
    pub fn m3_LinkRawFunctionEx(
        io_module: IM3Module,
        i_moduleName: *const c_char,
        i_functionName: *const c_char,
        i_signature: *const c_char,
        i_function: M3RawCall,
        i_userdata: *const c_void,
    ) -> M3Result;

    pub fn m3_GetModuleName(i_module: IM3Module) -> *const c_char;
    pub fn m3_SetModuleName(i_module: IM3Module, name: *const c_char);
    pub fn m3_GetModuleRuntime(i_module: IM3Module) -> IM3Runtime;

    pub fn m3_FindFunction(
        o_function: *mut IM3Function,
        i_runtime: IM3Runtime,
        i_functionName: *const c_char,
    ) -> M3Result;

    pub fn m3_GetArgCount(i_function: IM3Function) -> u32;
    pub fn m3_GetRetCount(i_function: IM3Function) -> u32;
    pub fn m3_GetArgType(i_function: IM3Function, i_index: u32) -> M3ValueType;
    pub fn m3_GetRetType(i_function: IM3Function, i_index: u32) -> M3ValueType;

    pub fn m3_Call(
        i_function: IM3Function,
        i_argc: u32,
        i_argptrs: *const *const c_void,
    ) -> M3Result;
    pub fn m3_GetResults(
        i_function: IM3Function,
        i_retc: u32,
        o_retptrs: *const *mut c_void,
    ) -> M3Result;

    pub fn m3_GetErrorInfo(i_runtime: IM3Runtime, o_info: *mut M3ErrorInfo);
    pub fn m3_ResetErrorInfo(i_runtime: IM3Runtime);

    pub fn m3_GetFunctionName(i_function: IM3Function) -> *const c_char;
    pub fn m3_GetFunctionModule(i_function: IM3Function) -> IM3Module;

    pub fn m3_GetBacktrace(i_runtime: IM3Runtime) -> IM3BacktraceInfo;
}

#[cfg(feature = "wasi")]
extern "C" {
    pub fn m3_LinkWASI(io_module: IM3Module) -> M3Result;
}
