using System.Runtime.InteropServices;

namespace ExtismSharp.Native;

public static class LibExtism
{
    [DllImport("extism")]
    public static extern IntPtr extism_context_new();
    [DllImport("extism")]
    public static extern void extism_context_free(IntPtr context);

    [DllImport("extism")]
    unsafe public static extern IntPtr extism_plugin_new(IntPtr context, byte* wasm, int wasmSize, bool withWasi);

    [DllImport("extism")]
    unsafe public static extern bool extism_plugin_update(IntPtr context, IntPtr plugin, byte* wasm, int wasmSize, bool withWasi);

    [DllImport("extism")]
    public static extern void extism_plugin_free(IntPtr context, IntPtr plugin);

    [DllImport("extism")]
    public static extern void extism_context_reset(IntPtr context);

    [DllImport("extism")]
    unsafe public static extern bool extism_plugin_config(IntPtr context, IntPtr plugin, byte* json, int jsonSize);

    [DllImport("extism")]
    public static extern bool extism_plugin_function_exists(IntPtr context, IntPtr plugin, string funcName);

    [DllImport("extism")]
    unsafe public static extern int extism_plugin_call(IntPtr context, IntPtr plugin, string funcName, byte* data, int dataLen);

    [DllImport("extism")]
    public static extern IntPtr extism_error(IntPtr context, nint plugin);

    [DllImport("extism")]
    public static extern long extism_plugin_output_length(IntPtr context, IntPtr plugin);

    [DllImport("extism")]
    public static extern IntPtr extism_plugin_output_data(IntPtr context, IntPtr plugin);

    [DllImport("extism")]
    public static extern bool extism_log_file(string filename, string logLevel);

    [DllImport("extism", EntryPoint = "extism_version")]
    public static extern IntPtr extism_version();
}