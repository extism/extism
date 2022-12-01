using System.Runtime.InteropServices;

namespace ExtismSharp.Native;

public class Plugin : IDisposable
{
    private readonly Context _context;
    private bool _disposed;

    internal Plugin(Context context, IntPtr handle)
    {
        _context = context;
        NativeHandle = handle;
    }

    public IntPtr NativeHandle { get; }

    unsafe public bool Update(ReadOnlySpan<byte> wasm, bool withWasi)
    {
        fixed (byte* wasmPtr = wasm)
        {
            return LibExtism.extism_plugin_update(_context.NativeHandle, NativeHandle, wasmPtr, wasm.Length, withWasi);
        }
    }

    unsafe public bool SetConfig(ReadOnlySpan<byte> json)
    {
        fixed (byte* jsonPtr = json)
        {
            return LibExtism.extism_plugin_config(_context.NativeHandle, NativeHandle, jsonPtr, json.Length);
        }
    }

    public bool FunctionExists(string name)
    {
        return LibExtism.extism_plugin_function_exists(_context.NativeHandle, NativeHandle, name);
    }

    unsafe public int CallFunction(string name, Span<byte> data)
    {
        fixed (byte* dataPtr = data)
        {
            return LibExtism.extism_plugin_call(_context.NativeHandle, NativeHandle, name, dataPtr, data.Length);
        }
    }

    public int OutputLength()
    {
        return (int)LibExtism.extism_plugin_output_length(_context.NativeHandle, NativeHandle);
    }

    public ReadOnlySpan<byte> OutputData()
    {
        var length = OutputLength();

        unsafe
        {
            var ptr = LibExtism.extism_plugin_output_data(_context.NativeHandle, NativeHandle).ToPointer();
            return new Span<byte>(ptr, length);
        }
    }

    public string? GetError()
    {
        var result = LibExtism.extism_error(_context.NativeHandle, NativeHandle);
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
        LibExtism.extism_plugin_free(_context.NativeHandle, NativeHandle);

        _disposed = true;
    }

    ~Plugin()
    {
        Dispose(false);
    }
}