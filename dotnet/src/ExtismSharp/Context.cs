using System.Runtime.InteropServices;

namespace ExtismSharp.Native;

public class Context : IDisposable
{
    private bool _disposed;
    public Context()
    {
        NativeHandle = LibExtism.extism_context_new();
    }

    public IntPtr NativeHandle { get; }

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

    public void Reset()
    {
        LibExtism.extism_context_reset(NativeHandle);
    }

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
}
