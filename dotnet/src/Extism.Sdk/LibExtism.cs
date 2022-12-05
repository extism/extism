using System.Runtime.InteropServices;

namespace Extism.Sdk.Native;

/// <summary>
/// Functions exposed by the native Extism library.
/// </summary>
internal static class LibExtism
{
    /// <summary>
    /// Create a new context.
    /// </summary>
    /// <returns>A pointer to the newly created context.</returns>
    [DllImport("extism")]
    public static extern IntPtr extism_context_new();

    /// <summary>
    /// Remove a context from the registry and free associated memory.
    /// </summary>
    /// <param name="context"></param>
    [DllImport("extism")]
    public static extern void extism_context_free(IntPtr context);

    /// <summary>
    /// Load a WASM plugin.
    /// </summary>
    /// <param name="context">Pointer to the context the plugin will be associated with.</param>
    /// <param name="wasm">A WASM module (wat or wasm) or a JSON encoded manifest.</param>
    /// <param name="wasmSize">The length of the `wasm` parameter.</param>
    /// <param name="withWasi">Enables/disables WASI.</param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe public static extern IntPtr extism_plugin_new(IntPtr context, byte* wasm, int wasmSize, bool withWasi);

    /// <summary>
    /// Update a plugin, keeping the existing ID.
    /// Similar to <see cref="extism_plugin_new"/> but takes an `plugin` argument to specify which plugin to update.
    /// Memory for this plugin will be reset upon update.
    /// </summary>
    /// <param name="context">Pointer to the context the plugin is associated with.</param>
    /// <param name="plugin">Pointer to the plugin you want to update.</param>
    /// <param name="wasm">A WASM module (wat or wasm) or a JSON encoded manifest.</param>
    /// <param name="wasmLength">The length of the `wasm` parameter.</param>
    /// <param name="withWasi">Enables/disables WASI.</param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe public static extern bool extism_plugin_update(IntPtr context, IntPtr plugin, byte* wasm, int wasmLength, bool withWasi);

    /// <summary>
    /// Remove a plugin from the registry and free associated memory.
    /// </summary>
    /// <param name="context">Pointer to the context the plugin is associated with.</param>
    /// <param name="plugin">Pointer to the plugin you want to free.</param>
    [DllImport("extism")]
    public static extern void extism_plugin_free(IntPtr context, IntPtr plugin);

    /// <summary>
    /// Remove all plugins from the registry.
    /// </summary>
    /// <param name="context"></param>
    [DllImport("extism")]
    public static extern void extism_context_reset(IntPtr context);

    /// <summary>
    /// Update plugin config values, this will merge with the existing values.
    /// </summary>
    /// <param name="context">Pointer to the context the plugin is associated with.</param>
    /// <param name="plugin">Pointer to the plugin you want to update the configurations for.</param>
    /// <param name="json">The configuration JSON encoded in UTF8.</param>
    /// <param name="jsonLength">The length of the `json` parameter.</param>
    /// <returns></returns>
    [DllImport("extism")]
    unsafe public static extern bool extism_plugin_config(IntPtr context, IntPtr plugin, byte* json, int jsonLength);

    /// <summary>
    /// Returns true if funcName exists.
    /// </summary>
    /// <param name="context"></param>
    /// <param name="plugin"></param>
    /// <param name="funcName"></param>
    /// <returns></returns>
    [DllImport("extism")]
    public static extern bool extism_plugin_function_exists(IntPtr context, IntPtr plugin, string funcName);

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
    unsafe public static extern int extism_plugin_call(IntPtr context, IntPtr plugin, string funcName, byte* data, int dataLen);

    /// <summary>
    /// Get the error associated with a Context or Plugin, if plugin is -1 then the context error will be returned.
    /// </summary>
    /// <param name="context"></param>
    /// <param name="plugin">A plugin pointer, or -1 for the context error.</param>
    /// <returns></returns>
    [DllImport("extism")]
    public static extern IntPtr extism_error(IntPtr context, nint plugin);

    /// <summary>
    /// Get the length of a plugin's output data.
    /// </summary>
    /// <param name="context"></param>
    /// <param name="plugin"></param>
    /// <returns></returns>
    [DllImport("extism")]
    public static extern long extism_plugin_output_length(IntPtr context, IntPtr plugin);

    /// <summary>
    /// Get the plugin's output data.
    /// </summary>
    /// <param name="context"></param>
    /// <param name="plugin"></param>
    /// <returns></returns>
    [DllImport("extism")]
    public static extern IntPtr extism_plugin_output_data(IntPtr context, IntPtr plugin);

    /// <summary>
    /// Set log file and level.
    /// </summary>
    /// <param name="filename"></param>
    /// <param name="logLevel"></param>
    /// <returns></returns>
    [DllImport("extism")]
    public static extern bool extism_log_file(string filename, string logLevel);

    /// <summary>
    /// Get the Extism version string.
    /// </summary>
    /// <returns></returns>
    [DllImport("extism", EntryPoint = "extism_version")]
    public static extern IntPtr extism_version();

    /// <summary>
    /// Extism Log Levels
    /// </summary>
    public static class LogLevels
    {
        /// <summary>
        /// Designates very serious errors.
        /// </summary>
        public const string Error = "Error";

        /// <summary>
        /// Designates hazardous situations.
        /// </summary>
        public const string Warn = "Warn";

        /// <summary>
        /// Designates useful information.
        /// </summary>
        public const string Info = "Info";

        /// <summary>
        /// Designates lower priority information.
        /// </summary>
        public const string Debug = "Debug";

        /// <summary>
        /// Designates very low priority, often extremely verbose, information.
        /// </summary>
        public const string Trace = "Trace";
    }
}