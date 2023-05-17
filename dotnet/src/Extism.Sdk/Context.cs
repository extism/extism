using System.Collections.Concurrent;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;

namespace Extism.Sdk.Native;

/// <summary>
/// Represents an Extism context through which you can load <see cref="Plugin"/>s.
/// </summary>
public unsafe class Context : IDisposable
{
    private readonly ConcurrentDictionary<int, Plugin> _plugins = new ConcurrentDictionary<int, Plugin>();

    private const int DisposedMarker = 1;

    private int _disposed;

    /// <summary>
    /// Initialize a new Extism Context.
    /// </summary>
    public Context()
    {
        unsafe
        {
            NativeHandle = LibExtism.extism_context_new();
        }
    }

    /// <summary>
    /// Native pointer to the Extism Context.
    /// </summary>
    internal LibExtism.ExtismContext* NativeHandle { get; }

    /// <summary>
    /// Loads an Extism <see cref="Plugin"/>.
    /// </summary>
    /// <param name="wasm">A WASM module (wat or wasm) or a JSON encoded manifest.</param>
    /// <param name="functions">List of host functions expected by the plugin.</param>
    /// <param name="withWasi">Enable/Disable WASI.</param>
    public Plugin CreatePlugin(ReadOnlySpan<byte> wasm, HostFunction[] functions, bool withWasi)
    {
        CheckNotDisposed();

        var functionHandles = functions.Select(f => f.NativeHandle).ToArray();

        unsafe
        {
            fixed (byte* wasmPtr = wasm)
            fixed (IntPtr* functionsPtr = functionHandles)
            {
                var index = LibExtism.extism_plugin_new(NativeHandle, wasmPtr, wasm.Length, functionsPtr, functions.Length, withWasi);
                if (index == -1)
                {
                    var errorMsg = GetError();
                    if (errorMsg != null)
                    {
                        throw new ExtismException(errorMsg);
                    }
                    else
                    {
                        throw new ExtismException("Failed to create plugin.");
                    }
                }

                return _plugins[index] = new Plugin(this, functions, index);
            }
        }
    }

    /// <summary>
    /// Get a plugin by index.
    /// </summary>
    /// <param name="index">Index of plugin.</param>
    /// <returns></returns>
    public Plugin GetPlugin(int index)
    {
        return _plugins[index];
    }

    /// <summary>
    /// Remove all plugins from this <see cref="Context"/>'s registry.
    /// </summary>
    public void Reset()
    {
        CheckNotDisposed();

        LibExtism.extism_context_reset(NativeHandle);
    }

    /// <summary>
    /// Get this this <see cref="Context"/>'s last error.
    /// </summary>
    /// <returns></returns>
    internal string? GetError()
    {
        CheckNotDisposed();

        var result = LibExtism.extism_error(NativeHandle, -1);
        return Marshal.PtrToStringUTF8(result);
    }

    /// <summary>
    /// Frees all resources held by this Context.
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
        throw new ObjectDisposedException(nameof(Context));
    }

    /// <summary>
    /// Frees all resources held by this Context.
    /// </summary>
    protected virtual void Dispose(bool disposing)
    {
        if (disposing)
        {
            // Free up any managed resources here
        }

        foreach (var plugin in _plugins.Values)
        {
            plugin.Dispose();
        }

        // Free up unmanaged resources
        LibExtism.extism_context_free(NativeHandle);
    }

    /// <summary>
    /// Destructs the current Context and frees all resources used by it.
    /// </summary>
    ~Context()
    {
        Dispose(false);
    }

    /// <summary>
    /// Get the Extism version string.
    /// </summary>
    public static string GetExtismVersion()
    {
        var pointer = LibExtism.extism_version();
        return Marshal.PtrToStringUTF8(pointer);
    }

    /// <summary>
    /// Set Extism's log file and level. This is applied for all <see cref="Context"/>s.
    /// </summary>
    /// <param name="logPath">Log file; can be 'stdout' or 'stderr' to write logs to the console.</param>
    /// <param name="level">The log level to write at.</param>
    public static bool SetExtismLogFile(string logPath, LogLevel level)
    {
        var logLevel = level switch
        {
            LogLevel.Error => LibExtism.LogLevels.Error,
            LogLevel.Warning => LibExtism.LogLevels.Warn,
            LogLevel.Info => LibExtism.LogLevels.Info,
            LogLevel.Debug => LibExtism.LogLevels.Debug,
            LogLevel.Trace => LibExtism.LogLevels.Trace,
            _ => throw new NotImplementedException(),
        };

        return LibExtism.extism_log_file(logPath, logLevel);
    }
}
