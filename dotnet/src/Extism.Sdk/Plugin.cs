using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;

namespace Extism.Sdk.Native;

/// <summary>
/// Represents a WASM Extism plugin.
/// </summary>
public class Plugin : IDisposable
{
    private const int DisposedMarker = 1;

    private readonly Context _context;
    private int _disposed;

    internal Plugin(Context context, IntPtr handle)
    {
        _context = context;
        NativeHandle = handle;
    }

    /// <summary>
    /// A pointer to the native Plugin struct.
    /// </summary>
    internal IntPtr NativeHandle { get; }

    /// <summary>
    /// Update a plugin, keeping the existing ID.
    /// </summary>
    /// <param name="wasm">The plugin WASM bytes.</param>
    /// <param name="withWasi">Enable/Disable WASI.</param>
    unsafe public bool Update(ReadOnlySpan<byte> wasm, bool withWasi)
    {
        CheckNotDisposed();

        fixed (byte* wasmPtr = wasm)
        {
            return LibExtism.extism_plugin_update(_context.NativeHandle, NativeHandle, wasmPtr, wasm.Length, withWasi);
        }
    }

    /// <summary>
    ///  Update plugin config values, this will merge with the existing values.
    /// </summary>
    /// <param name="json">The configuration JSON encoded in UTF8.</param>
    unsafe public bool SetConfig(ReadOnlySpan<byte> json)
    {
        CheckNotDisposed();

        fixed (byte* jsonPtr = json)
        {
            return LibExtism.extism_plugin_config(_context.NativeHandle, NativeHandle, jsonPtr, json.Length);
        }
    }

    /// <summary>
    /// Checks if a specific function exists in the current plugin.
    /// </summary>
    public bool FunctionExists(string name)
    {
        CheckNotDisposed();

        return LibExtism.extism_plugin_function_exists(_context.NativeHandle, NativeHandle, name);
    }

    /// <summary>
    /// Calls a function in the current plugin and returns a status.
    /// If the status represents an error, call <see cref="GetError"/> to get the error.
    /// Othewise, call <see cref="OutputData"/> to get the function's output data.
    /// </summary>
    /// <param name="functionName">Name of the function in the plugin to invoke.</param>
    /// <param name="data">A buffer to provide as input to the function.</param>
    /// <returns>The exit code of the function.</returns>
    /// <exception cref="ExtismException"></exception>
    unsafe public ReadOnlySpan<byte> CallFunction(string functionName, ReadOnlySpan<byte> data)
    {
        CheckNotDisposed();

        fixed (byte* dataPtr = data)
        {
            int response = LibExtism.extism_plugin_call(_context.NativeHandle, NativeHandle, functionName, dataPtr, data.Length);
            if (response == 0) {
                return OutputData();
            } else {
                var errorMsg = GetError();
                if (errorMsg != null) {
                    throw new ExtismException(errorMsg);
                } else {
                    throw new ExtismException("Call to Extism failed");
                }
            }
        }
    }

    /// <summary>
    /// Get the length of a plugin's output data.
    /// </summary>
    /// <returns></returns>
    internal int OutputLength()
    {
        CheckNotDisposed();

        return (int)LibExtism.extism_plugin_output_length(_context.NativeHandle, NativeHandle);
    }

    /// <summary>
    /// Get the plugin's output data.
    /// </summary>
    internal ReadOnlySpan<byte> OutputData()
    {
        CheckNotDisposed();

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
    internal string? GetError()
    {
        CheckNotDisposed();

        var result = LibExtism.extism_error(_context.NativeHandle, NativeHandle);
        return Marshal.PtrToStringUTF8(result);
    }

    /// <summary>
    /// Frees all resources held by this Plugin.
    /// </summary>
    public void Dispose()
    {
        if (Interlocked.Exchange(ref _disposed, DisposedMarker) == DisposedMarker)
        {
            // Already disposed.
            return;
        }

        Dispose(true);
        GC.SuppressFinalize(this);
    }

    /// <summary>
    /// Throw an appropriate exception if the plugin has been disposed.
    /// </summary>
    /// <exception cref="ObjectDisposedException"></exception>
    protected void CheckNotDisposed()
    {
        Interlocked.MemoryBarrier();
        if (_disposed == DisposedMarker)
        {
            ThrowDisposedException();
        }
    }

    [DoesNotReturn]
    private static void ThrowDisposedException()
    {
        throw new ObjectDisposedException(nameof(Plugin));
    }

    /// <summary>
    /// Frees all resources held by this Plugin.
    /// </summary>
    protected virtual void Dispose(bool disposing)
    {
        if (disposing)
        {
            // Free up any managed resources here
        }

        // Free up unmanaged resources
        LibExtism.extism_plugin_free(_context.NativeHandle, NativeHandle);
    }

    /// <summary>
    /// Destructs the current Plugin and frees all resources used by it.
    /// </summary>
    ~Plugin()
    {
        Dispose(false);
    }
}