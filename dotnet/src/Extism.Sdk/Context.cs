using System.Runtime.InteropServices;

namespace Extism.Sdk.Native;

/// <summary>
/// Represents an Extism context through which you can load <see cref="Plugin"/>s.
/// </summary>
public class Context : IDisposable
{
    private bool _disposed;
    public Context()
    {
        NativeHandle = LibExtism.extism_context_new();
    }

    public IntPtr NativeHandle { get; }

    /// <summary>
    /// Loads an Extism <see cref="Plugin"/>.
    /// </summary>
    /// <param name="wasm">The plugin WASM bytes.</param>
    /// <param name="withWasi">Enable/Disable WASI.</param>
    /// <returns></returns>
    public Plugin CreatePlugin(ReadOnlySpan<byte> wasm, bool withWasi)
    {
        unsafe
        {
            fixed (byte* wasmPtr = wasm)
            {
                var plugin = LibExtism.extism_plugin_new(NativeHandle, wasmPtr, wasm.Length, withWasi);
                return new Plugin(this, plugin);
            }
        }
    }

    /// <summary>
    /// Remove all plugins from this <see cref="Context"/>'s registry.
    /// </summary>
    public void Reset()
    {
        LibExtism.extism_context_reset(NativeHandle);
    }

    /// <summary>
    /// Get this this <see cref="Context"/>'s last error.
    /// </summary>
    /// <returns></returns>
    public string? GetError()
    {
        var result = LibExtism.extism_error(NativeHandle, -1);
        return Marshal.PtrToStringUTF8(result);
    }

    public void Dispose()
    {
        Dispose(true);
        GC.SuppressFinalize(this);
    }

    protected virtual void Dispose(bool disposing)
    {
        if (_disposed) return;

        if (disposing)
        {
            // Free up any managed resources here
        }

        // Free up unamanged resources
        LibExtism.extism_context_free(NativeHandle);

        _disposed = true;
    }

    ~Context()
    {
        Dispose(false);
    }

    /// <summary>
    /// Get the Extism version string.
    /// </summary>
    /// <returns></returns>
    public static string GetExtismVersion()
    {
        var pointer = LibExtism.extism_version();
        return Marshal.PtrToStringUTF8(pointer);
    }

    /// <summary>
    /// Set Extism's log file and level. This is applied for all <see cref="Context"/>s.
    /// </summary>
    /// <param name="logPath"></param>
    /// <param name="level"></param>
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

public enum LogLevel
{
    /// <summary>
    /// Designates very serious errors.
    /// </summary>
    Error,

    /// <summary>
    /// Designates hazardous situations.
    /// </summary>
    Warning,

    /// <summary>
    /// Designates useful information.
    /// </summary>
    Info,

    /// <summary>
    /// Designates lower priority information.
    /// </summary>
    Debug,

    /// <summary>
    /// Designates very low priority, often extremely verbose, information.
    /// </summary>
    Trace
}
