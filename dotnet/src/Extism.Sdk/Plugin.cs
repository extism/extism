using System.Diagnostics;
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
    private readonly IntPtr _cancelHandle;

    internal Plugin(Context context, int pluginIndex, IntPtr cancelHandle)
    {
        _context = context;
        PluginIndex = pluginIndex;
        _cancelHandle = cancelHandle;
    }

    /// <summary>
    /// This plugin's current index in the Context
    /// </summary>
    internal Int32 PluginIndex { get; }

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
            return LibExtism.extism_plugin_update(_context.NativeHandle, PluginIndex, wasmPtr, wasm.Length, null, 0, withWasi);
        }
    }
    
    /// <summary>
    /// Request to cancel a currently-executing plugin at the next epoch.
    /// </summary>
    /// <param name="wasm">The plugin WASM bytes.</param>
    /// <param name="withWasi">Enable/Disable WASI.</param>
    unsafe public void Cancel() 
    {
        CheckNotDisposed();
        LibExtism.extism_plugin_cancel(_cancelHandle);
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
            return LibExtism.extism_plugin_config(_context.NativeHandle, PluginIndex, jsonPtr, json.Length);
        }
    }

    /// <summary>
    /// Checks if a specific function exists in the current plugin.
    /// </summary>
    unsafe public bool FunctionExists(string name)
    {
        CheckNotDisposed();

        return LibExtism.extism_plugin_function_exists(_context.NativeHandle, PluginIndex, name);
    }

    /// <summary>
    /// Calls a function in the current plugin and returns the plugin's output bytes.
    /// </summary>
    /// <param name="functionName">Name of the function in the plugin to invoke.</param>
    /// <param name="data">A buffer to provide as input to the function.</param>
    /// <returns>A buffer with the plugin's output bytes.</returns>
    /// <exception cref="ExtismException"></exception>
    unsafe public ReadOnlySpan<byte> CallFunction(string functionName, ReadOnlySpan<byte> data)
    {
        CheckNotDisposed();

        fixed (byte* dataPtr = data)
        {
            int response = LibExtism.extism_plugin_call(_context.NativeHandle, PluginIndex, functionName, dataPtr, data.Length);
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

    public async Task<byte[]> CallFunctionAsync(string functionName, byte[] data, int? timeoutMs = null, CancellationToken? cancellationToken = null)
    {

        // If we don't set a timeout or a cancellation token, our watcher thread will run forever even after it leaves scope.
        // A task does not get automatically canceled or garbage collected if it leaves scope.
        // we need to make our own internal cancellation to ensure that we cancel this thread when we're done with it before exiting the function.
        CancellationTokenSource internalTokenSource = new CancellationTokenSource();


        // Create an async function that will run forever or until timeoutMs,
        // but will exit early if the external or internal cancellation tokens are canceled.
        // Check token for cancellation every 10 ms to minimize CPU utilization.
        // Even though the Task.Run gets a cancellation token, it doesn't continually check for cancellation,
        // only checks once at the beginning to determine if it should run the task or not,
        // so we must also check within this task to ensure the token hasn't been canceled.
        var runUntilTimeoutOrCancelled = async () =>
        {
            // If no timeout is set, save the resources of having a stopwatch and just run forever until task is canceled.
            if (timeoutMs == null)
            {
                while (true)
                {
                    if (internalTokenSource.IsCancellationRequested || (cancellationToken?.IsCancellationRequested ?? false))
                    {
                        break;
                    }

                    try
                    { 
                        await Task.Delay(1000, cancellationToken ?? internalTokenSource.Token);
                    }
                    catch (Exception e)
                    {
                    }
                }
            }
            else
            {
                var executionTime = Stopwatch.StartNew();
                while (executionTime.ElapsedMilliseconds < timeoutMs)
                {
                    if (internalTokenSource.IsCancellationRequested || (cancellationToken?.IsCancellationRequested ?? false))
                    {
                        break;
                    }
                    try
                    { 
                        await Task.Delay(1000, cancellationToken ?? internalTokenSource.Token);
                    }
                    catch (Exception e)
                    {
                    }
                }
            }
        };

        // Create tasks for invoking called function as well as for running a timeout / cancelled checker.
        // Note, we are not awaiting the task here.  We will await the tasks later in parallel to determine when they are completed.

        Task cancellableTimeoutTask;
        Task<byte[]> executeFunctionInternalTask;

        // If cancellation token has already been canceled, don't execute any code.
        if (cancellationToken?.IsCancellationRequested ?? false)
        {
            throw new TaskCanceledException();
        }

        cancellableTimeoutTask = Task.Run(runUntilTimeoutOrCancelled);
        executeFunctionInternalTask = Task.Run(() => { return CallFunction(functionName, data).ToArray(); });

        // Race the 2 tasks.  When they're complete, either executeFunctionInternal
        // will have completed with a result, or the cancellableTimeout will have run to completion
        // meaning the task was either cancelled or timed out.
        await Task.WhenAny(executeFunctionInternalTask, cancellableTimeoutTask);

        if (executeFunctionInternalTask.IsCompletedSuccessfully)
        {
            internalTokenSource.Cancel(); // Cancel internal token so the background task will terminate.
            await cancellableTimeoutTask; // Wait for the cancellation monitor task to exit gracefully
            return await executeFunctionInternalTask;
        }
        
        internalTokenSource.Cancel(); // Cancel internal token so the background task will terminate.
        await cancellableTimeoutTask; // Wait for the task to exit gracefully
        Cancel(); // Cancel the plugin so Extism can free it when we exit
        
        // If an exception was thrown by the task, rethrow it.
        if (executeFunctionInternalTask.Exception.InnerException != null)
        {
            throw executeFunctionInternalTask.Exception.InnerException;
        }
        // Dispose();
        // Throw an exception so the caller knows that something timed out.
        throw new TaskCanceledException();
    }

    /// <summary>
    /// Get the length of a plugin's output data.
    /// </summary>
    /// <returns></returns>
    unsafe internal int OutputLength()
    {
        CheckNotDisposed();

        return (int)LibExtism.extism_plugin_output_length(_context.NativeHandle, PluginIndex);
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
            var ptr = LibExtism.extism_plugin_output_data(_context.NativeHandle, PluginIndex).ToPointer();
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

        var result = LibExtism.extism_error(_context.NativeHandle, PluginIndex);
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
        LibExtism.extism_plugin_free(_context.NativeHandle, PluginIndex);
    }

    /// <summary>
    /// Destructs the current Plugin and frees all resources used by it.
    /// </summary>
    ~Plugin()
    {
        Dispose(false);
    }
}