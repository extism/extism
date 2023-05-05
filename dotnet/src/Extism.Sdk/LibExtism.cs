using System.Runtime.InteropServices;

namespace Extism.Sdk.Native;

/// <summary>
/// A union type for host function argument/return values.
/// </summary>
[StructLayout(LayoutKind.Explicit)]
public struct ExtismValUnion
{
    /// <summary>
    /// Set this for 32 bit integers
    /// </summary>
    [FieldOffset(0)]
    public int i32;

    /// <summary>
    /// Set this for 64 bit integers
    /// </summary>
    [FieldOffset(0)]
    public long i64;

    /// <summary>
    /// Set this for 32 bit floats
    /// </summary>
    [FieldOffset(0)]
    public float f32;

    /// <summary>
    /// Set this for 64 bit floats
    /// </summary>
    [FieldOffset(0)]
    public double f64;
}

/// <summary>
/// Represents Wasm data types that Extism can understand
/// </summary>
public enum ExtismValType : byte
{
    /// <summary>
    /// Signed 32 bit integer. Equivalent of <see cref="int"/> or <see cref="uint"/>
    /// </summary>
    I32,

    /// <summary>
    /// Signed 64 bit integer. Equivalent of <see cref="long"/> or <see cref="ulong"/>
    /// </summary>
    I64,

    /// <summary>
    /// Floating point 32 bit integer. Equivalent of <see cref="float"/>
    /// </summary>
    F32,

    /// <summary>
    /// Floating point 64 bit integer. Equivalent of <see cref="double"/>
    /// </summary>
    F64,

    /// <summary>
    /// A 128 bit number.
    /// </summary>
    V128,

    /// <summary>
    /// A reference to opaque data in the Wasm instance.
    /// </summary>
    FuncRef,

    /// <summary>
    /// A reference to opaque data in the Wasm instance.
    /// </summary>
    ExternRef
}

/// <summary>
/// `ExtismVal` holds the type and value of a function argument/return
/// </summary>
[StructLayout(LayoutKind.Sequential)]
public struct ExtismVal
{
    /// <summary>
    /// The type for the argument
    /// </summary>
    public ExtismValType t;

    /// <summary>
    /// The value for the argument
    /// </summary>
    public ExtismValUnion v;
}

/// <summary>
/// Functions exposed by the native Extism library.
/// </summary>
internal static class LibExtism
{
    /// <summary>
    /// A `Context` is used to store and manage plugins.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    internal struct ExtismContext { }

    /// <summary>
    /// Host function signature
    /// </summary>
    /// <param name="plugin"></param>
    /// <param name="inputs"></param>
    /// <param name="n_inputs"></param>
    /// <param name="outputs"></param>
    /// <param name="n_outputs"></param>
    /// <param name="data"></param>
    unsafe internal delegate void InternalExtismFunction(nint plugin, ExtismVal* inputs, uint n_inputs, ExtismVal* outputs, uint n_outputs, IntPtr data);

    /// <summary>
    /// Returns a pointer to the memory of the currently running plugin.
    /// NOTE: this should only be called from host functions.
    /// </summary>
    /// <param name="plugin"></param>
    /// <returns></returns>
    [DllImport("extism", EntryPoint = "extism_current_plugin_memory")]
    internal static extern IntPtr extism_current_plugin_memory(nint plugin);

    /// <summary>
    /// Allocate a memory block in the currently running plugin
    /// </summary>
    /// <param name="plugin"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    [DllImport("extism", EntryPoint = "extism_current_plugin_memory_alloc")]
    internal static extern IntPtr extism_current_plugin_memory_alloc(nint plugin, long n);

    /// <summary>
    /// Get the length of an allocated block.
    /// NOTE: this should only be called from host functions.
    /// </summary>
    /// <param name="plugin"></param>
    /// <param name="n"></param>
    /// <returns></returns>
    [DllImport("extism", EntryPoint = "extism_current_plugin_memory_length")]
    internal static extern long extism_current_plugin_memory_length(nint plugin, long n);

    /// <summary>
    /// Get the length of an allocated block.
    /// NOTE: this should only be called from host functions.
    /// </summary>
    /// <param name="plugin"></param>
    /// <param name="ptr"></param>
    [DllImport("extism", EntryPoint = "extism_current_plugin_memory_free")]
    internal static extern void extism_current_plugin_memory_free(nint plugin, IntPtr ptr);

    /// <summary>
    /// Create a new host function.
    /// </summary>
    /// <param name="name">function name, this should be valid UTF-8</param>
    /// <param name="inputs">argument types</param>
    /// <param name="nInputs">number of argument types</param>
    /// <param name="outputs">return types</param>
    /// <param name="nOutputs">number of return types</param>
    /// <param name="func">the function to call</param>
    /// <param name="userData">a pointer that will be passed to the function when it's called this value should live as long as the function exists</param>
    /// <param name="freeUserData">a callback to release the `user_data` value when the resulting `ExtismFunction` is freed.</param>
    /// <returns></returns>
    [DllImport("extism", EntryPoint = "extism_function_new")]
    unsafe internal static extern IntPtr extism_function_new(string name, ExtismValType* inputs, long nInputs, ExtismValType* outputs, long nOutputs, InternalExtismFunction func, IntPtr userData, IntPtr freeUserData);

    /// <summary>
    /// Set the namespace of an <see cref="ExtismFunction"/>
    /// </summary>
    /// <param name="ptr"></param>
    /// <param name="namespace"></param>
    [DllImport("extism", EntryPoint = "extism_function_set_namespace")]
    internal static extern void extism_function_set_namespace(IntPtr ptr, string @namespace);

    /// <summary>
    /// Free an <see cref="ExtismFunction"/>
    /// </summary>
    /// <param name="ptr"></param>
    [DllImport("extism", EntryPoint = "extism_function_free")]
    internal static extern void extism_function_free(IntPtr ptr);

    /// <summary>
    /// Create a new context.
    /// </summary>
    /// <returns>A pointer to the newly created context.</returns>
    [DllImport("extism")]
    unsafe internal static extern ExtismContext* extism_context_new();

    /// <summary>
    /// Remove a context from the registry and free associated memory.
    /// </summary>
    /// <param name="context"></param>
    [DllImport("extism")]
    unsafe internal static extern void extism_context_free(ExtismContext* context);

    /// <summary>
    /// Load a WASM plugin.
    /// </summary>
    /// <param name="context">Pointer to the context the plugin will be associated with.</param>
    /// <param name="wasm">A WASM module (wat or wasm) or a JSON encoded manifest.</param>
    /// <param name="wasmSize">The length of the `wasm` parameter.</param>
    /// <param name="functions">Array of host function pointers.</param>
    /// <param name="nFunctions">Number of host functions.</param>
    /// <param name="withWasi">Enables/disables WASI.</param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe internal static extern int extism_plugin_new(ExtismContext* context, byte* wasm, int wasmSize, IntPtr* functions, int nFunctions, bool withWasi);

    /// <summary>
    /// Update a plugin, keeping the existing ID.
    /// Similar to <see cref="extism_plugin_new"/> but takes an `plugin` argument to specify which plugin to update.
    /// Memory for this plugin will be reset upon update.
    /// </summary>
    /// <param name="context">Pointer to the context the plugin is associated with.</param>
    /// <param name="plugin">Pointer to the plugin you want to update.</param>
    /// <param name="wasm">A WASM module (wat or wasm) or a JSON encoded manifest.</param>
    /// <param name="wasmSize">The length of the `wasm` parameter.</param>
    /// <param name="functions">Array of host function pointers.</param>
    /// <param name="nFunctions">Number of host functions.</param>
    /// <param name="withWasi">Enables/disables WASI.</param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe internal static extern bool extism_plugin_update(ExtismContext* context, int plugin, byte* wasm, long wasmSize, Span<IntPtr> functions, long nFunctions, bool withWasi);

    /// <summary>
    /// Remove a plugin from the registry and free associated memory.
    /// </summary>
    /// <param name="context">Pointer to the context the plugin is associated with.</param>
    /// <param name="plugin">Pointer to the plugin you want to free.</param>
    [DllImport("extism")]
    unsafe internal static extern void extism_plugin_free(ExtismContext* context, int plugin);

    /// <summary>
    /// Remove all plugins from the registry.
    /// </summary>
    /// <param name="context"></param>
    [DllImport("extism")]
    unsafe internal static extern void extism_context_reset(ExtismContext* context);

    /// <summary>
    /// Update plugin config values, this will merge with the existing values.
    /// </summary>
    /// <param name="context">Pointer to the context the plugin is associated with.</param>
    /// <param name="plugin">Pointer to the plugin you want to update the configurations for.</param>
    /// <param name="json">The configuration JSON encoded in UTF8.</param>
    /// <param name="jsonLength">The length of the `json` parameter.</param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe internal static extern bool extism_plugin_config(ExtismContext* context, int plugin, byte* json, int jsonLength);

    /// <summary>
    /// Returns true if funcName exists.
    /// </summary>
    /// <param name="context"></param>
    /// <param name="plugin"></param>
    /// <param name="funcName"></param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe internal static extern bool extism_plugin_function_exists(ExtismContext* context, int plugin, string funcName);

    /// <summary>
    /// Call a function.
    /// </summary>
    /// <param name="context"></param>
    /// <param name="plugin"></param>
    /// <param name="funcName">The function to call.</param>
    /// <param name="data">Input data.</param>
    /// <param name="dataLen">The length of the `data` parameter.</param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe internal static extern int extism_plugin_call(ExtismContext* context, int plugin, string funcName, byte* data, int dataLen);

    /// <summary>
    /// Get the error associated with a Context or Plugin, if plugin is -1 then the context error will be returned.
    /// </summary>
    /// <param name="context"></param>
    /// <param name="plugin">A plugin pointer, or -1 for the context error.</param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe internal static extern IntPtr extism_error(ExtismContext* context, nint plugin);

    /// <summary>
    /// Get the length of a plugin's output data.
    /// </summary>
    /// <param name="context"></param>
    /// <param name="plugin"></param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe internal static extern long extism_plugin_output_length(ExtismContext* context, int plugin);

    /// <summary>
    /// Get the plugin's output data.
    /// </summary>
    /// <param name="context"></param>
    /// <param name="plugin"></param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe internal static extern IntPtr extism_plugin_output_data(ExtismContext* context, int plugin);

    /// <summary>
    /// Set log file and level.
    /// </summary>
    /// <param name="filename"></param>
    /// <param name="logLevel"></param>
    /// <returns></returns>
    [DllImport("extism")]
    internal static extern bool extism_log_file(string filename, string logLevel);

    /// <summary>
    /// Get the Extism version string.
    /// </summary>
    /// <returns></returns>
    [DllImport("extism", EntryPoint = "extism_version")]
    internal static extern IntPtr extism_version();

    /// <summary>
    /// Extism Log Levels
    /// </summary>
    internal static class LogLevels
    {
        /// <summary>
        /// Designates very serious errors.
        /// </summary>
        internal const string Error = "Error";

        /// <summary>
        /// Designates hazardous situations.
        /// </summary>
        internal const string Warn = "Warn";

        /// <summary>
        /// Designates useful information.
        /// </summary>
        internal const string Info = "Info";

        /// <summary>
        /// Designates lower priority information.
        /// </summary>
        internal const string Debug = "Debug";

        /// <summary>
        /// Designates very low priority, often extremely verbose, information.
        /// </summary>
        internal const string Trace = "Trace";
    }
}
