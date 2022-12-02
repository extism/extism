using System.Runtime.InteropServices;

namespace Extism.Sdk.Native;

/// <summary>
/// Represents a WASM Extism plugin.
/// </summary>
public class Plugin : IDisposable
{
    private readonly Context _context;
    private bool _disposed;

    internal Plugin(Context context, IntPtr handle)
    {
        _context = context;
        NativeHandle = handle;
    }

    /// <summary>
    /// A pointer to the native Plugin struct.
    /// </summary>
    public IntPtr NativeHandle { get; }

    /// <summary>
    /// Update a plugin, keeping the existing ID.
    /// </summary>
    /// <param name="wasm">The plugin WASM bytes.</param>
    /// <param name="withWasi">Enable/Disable WASI.</param>
    /// <returns></returns>
    unsafe public bool Update(ReadOnlySpan<byte> wasm, bool withWasi)
    {
        fixed (byte* wasmPtr = wasm)
        {
            return LibExtism.extism_plugin_update(_context.NativeHandle, NativeHandle, wasmPtr, wasm.Length, withWasi);
        }
    }

    /// <summary>
    ///  Update plugin config values, this will merge with the existing values.
    /// </summary>
    /// <param name="json">The configuration JSON encoded in UTF8.</param>
    /// <returns></returns>
    unsafe public bool SetConfig(ReadOnlySpan<byte> json)
    {
        fixed (byte* jsonPtr = json)
        {
            return LibExtism.extism_plugin_config(_context.NativeHandle, NativeHandle, jsonPtr, json.Length);
        }
    }

    /// <summary>
    /// Checks if a specific function exists in the current plugin.
    /// </summary>
    /// <param name="name"></param>
    /// <returns></returns>
    public bool FunctionExists(string name)
    {
        return LibExtism.extism_plugin_function_exists(_context.NativeHandle, NativeHandle, name);
    }

    /// <summary>
    /// Calls a function in the current plugin and returns a status.
    /// If the status represents an error, call <see cref="GetError"/> to get the error.
    /// Othewise, call <see cref="OutputData"/> to get the function's output data.
    /// </summary>
    /// <param name="functionName"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    unsafe public int CallFunction(string functionName, Span<byte> data)
    {
        fixed (byte* dataPtr = data)
        {
            return LibExtism.extism_plugin_call(_context.NativeHandle, NativeHandle, functionName, dataPtr, data.Length);
        }
    }

    /// <summary>
    /// Get the length of a plugin's output data.
    /// </summary>
    /// <returns></returns>
    public int OutputLength()
    {
        return (int)LibExtism.extism_plugin_output_length(_context.NativeHandle, NativeHandle);
    }

    /// <summary>
    /// Get the plugin's output data.
    /// </summary>
    /// <returns></returns>
    public ReadOnlySpan<byte> OutputData()
    {
        var length = OutputLength();

        unsafe
        {
            var ptr = LibExtism.extism_plugin_output_data(_context.NativeHandle, NativeHandle).ToPointer();
            return new Span<byte>(ptr, length);
        }
    }

    /// <summary>
    /// Get the error associated with the current plugin.
    /// </summary>
    /// <returns></returns>
    public string? GetError()
    {
        var result = LibExtism.extism_error(_context.NativeHandle, NativeHandle);
        return Marshal.PtrToStringUTF8(result);
    }

    /// <summary>
    /// Frees all resources held by this Plugin.
    /// </summary>
    public void Dispose()
    {
        Dispose(true);
        GC.SuppressFinalize(this);
    }

    /// <summary>
    /// Frees all resources held by this Plugin.
    /// </summary>
    protected virtual void Dispose(bool disposing)
    {
        if (_disposed) return;

        if (disposing)
        {
            // Free up any managed resources here
        }

        // Free up unamanged resources
        LibExtism.extism_plugin_free(_context.NativeHandle, NativeHandle);

        _disposed = true;
    }

    /// <summary>
    /// Destructs the current Plugin and frees all resources used by it.
    /// </summary>
    ~Plugin()
    {
        Dispose(false);
    }
}