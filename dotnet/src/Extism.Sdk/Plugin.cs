using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.Json;

namespace Extism.Sdk.Native;

/// <summary>
/// Represents a WASM Extism plugin.
/// </summary>
public unsafe class Plugin : IDisposable
{
    private const int DisposedMarker = 1;

    private readonly HostFunction[] _functions;
    private int _disposed;

    /// <summary>
    /// Native pointer to the Extism Plugin.
    /// </summary>
    internal LibExtism.ExtismPlugin* NativeHandle { get; }

    /// <summary>
    /// Create a plugin from a Manifest.
    /// </summary>
    /// <param name="manifest"></param>
    /// <param name="functions"></param>
    /// <param name="withWasi"></param>
    public Plugin(Manifest manifest, HostFunction[] functions, bool withWasi)
    {
        _functions = functions;

        var options = new JsonSerializerOptions
        {
            DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull,
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        };

        options.Converters.Add(new WasmSourceConverter());
        var json = JsonSerializer.Serialize(manifest, options);

        var bytes = Encoding.UTF8.GetBytes(json);

        var functionHandles = functions.Select(f => f.NativeHandle).ToArray();
        fixed (byte* wasmPtr = bytes)
        fixed (IntPtr* functionsPtr = functionHandles)
        {
            NativeHandle = Initialize(wasmPtr, bytes.Length, functions, withWasi, functionsPtr);
        }
    }

    /// <summary>
    /// Create and load a plugin from a byte array.
    /// </summary>
    /// <param name="wasm">A WASM module (wat or wasm) or a JSON encoded manifest.</param>
    /// <param name="functions">List of host functions expected by the plugin.</param>
    /// <param name="withWasi">Enable/Disable WASI.</param>
    public Plugin(ReadOnlySpan<byte> wasm, HostFunction[] functions, bool withWasi)
    {
        _functions = functions;

        var functionHandles = functions.Select(f => f.NativeHandle).ToArray();
        fixed (byte* wasmPtr = wasm)
        fixed (IntPtr* functionsPtr = functionHandles)
        {
            NativeHandle = Initialize(wasmPtr, wasm.Length, functions, withWasi, functionsPtr);
        }
    }

    private unsafe LibExtism.ExtismPlugin* Initialize(byte* wasmPtr, int wasmLength, HostFunction[] functions, bool withWasi, IntPtr* functionsPtr)
    {
        char** errorMsgPtr;

        var handle = LibExtism.extism_plugin_new(wasmPtr, (ulong)wasmLength, functionsPtr, (ulong)functions.Length, withWasi, out errorMsgPtr);
        if (handle == null)
        {
            var msg = "Unable to create plugin";

            if (errorMsgPtr is not null)
            {
                msg = Marshal.PtrToStringAnsi(new IntPtr(errorMsgPtr));
            }

            throw new ExtismException(msg);
        }

        return handle;
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
            return LibExtism.extism_plugin_config(NativeHandle, jsonPtr, json.Length);
        }
    }

    /// <summary>
    /// Checks if a specific function exists in the current plugin.
    /// </summary>
    unsafe public bool FunctionExists(string name)
    {
        CheckNotDisposed();

        return LibExtism.extism_plugin_function_exists(NativeHandle, name);
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
            int response = LibExtism.extism_plugin_call(NativeHandle, functionName, dataPtr, data.Length);
            if (response == 0)
            {
                return OutputData();
            }
            else
            {
                var errorMsg = GetError();
                if (errorMsg != null)
                {
                    throw new ExtismException(errorMsg);
                }
                else
                {
                    throw new ExtismException("Call to Extism failed");
                }
            }
        }
    }

    /// <summary>
    /// Get the length of a plugin's output data.
    /// </summary>
    /// <returns></returns>
    unsafe internal int OutputLength()
    {
        CheckNotDisposed();

        return (int)LibExtism.extism_plugin_output_length(NativeHandle);
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
            var ptr = LibExtism.extism_plugin_output_data(NativeHandle).ToPointer();
            return new Span<byte>(ptr, length);
        }
    }

    /// <summary>
    /// Get the error associated with the current plugin.
    /// </summary>
    /// <returns></returns>
    unsafe internal string? GetError()
    {
        CheckNotDisposed();

        var result = LibExtism.extism_plugin_error(NativeHandle);
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
    unsafe protected virtual void Dispose(bool disposing)
    {
        if (disposing)
        {
            // Free up any managed resources here
        }

        // Free up unmanaged resources
        LibExtism.extism_plugin_free(NativeHandle);
    }

    /// <summary>
    /// Destructs the current Plugin and frees all resources used by it.
    /// </summary>
    ~Plugin()
    {
        Dispose(false);
    }

    /// <summary>
    /// Get Extism Runtime version.
    /// </summary>
    /// <returns></returns>
    public static string ExtismVersion()
    {
        var version = LibExtism.extism_version();
        return Marshal.PtrToStringAnsi(version);

    }
}
