using Extism.Sdk.Native;

using System.Diagnostics.CodeAnalysis;

namespace Extism.Sdk
{
    /// <summary>
    /// A host function signature.
    /// </summary>
    /// <param name="plugin">Plugin Index</param>
    /// <param name="inputs">Input parameters</param>
    /// <param name="outputs">Output parameters, the host function can change this.</param>
    /// <param name="userData">A data passed in during Host Function creation.</param>
    public delegate void ExtismFunction(CurrentPlugin plugin, ExtismVal[] inputs, ExtismVal[] outputs, IntPtr userData);

    public class CurrentPlugin
    {
        public CurrentPlugin(nint nativeHandle)
        {
            NativeHandle = nativeHandle;
        }

        internal nint NativeHandle { get; }

        /// <summary>
        /// Returns a pointer to the memory of the currently running plugin.
        /// NOTE: this should only be called from host functions.
        /// </summary>
        /// <returns></returns>
        public nint GetMemory()
        {
            return LibExtism.extism_current_plugin_memory(NativeHandle);
        }

        public void FreeBlock(nint pointer)
        {
            LibExtism.extism_current_plugin_memory_free(NativeHandle, pointer);
        }

        /// <summary>
        /// Allocate a memory block in the currently running plugin.
        /// 
        /// </summary>
        /// <param name="length"></param>
        /// <returns></returns>
        public nint AllocateBlock(long length)
        {
            return LibExtism.extism_current_plugin_memory_alloc(NativeHandle, length);
        }

        /// <summary>
        /// Get the length of an allocated block.
        /// NOTE: this should only be called from host functions.
        /// </summary>
        /// <param name="pointer"></param>
        /// <returns></returns>
        public long BlockLength(nint pointer)
        {
            return LibExtism.extism_current_plugin_memory_length(NativeHandle, pointer);
        }

    }

    /// <summary>
    /// A function provided by the host that plugins can call.
    /// </summary>
    public class HostFunction : IDisposable
    {
        private const int DisposedMarker = 1;
        private int _disposed;

        /// <summary>
        /// Registers a Host Function.
        /// </summary>
        /// <param name="name">The literal name of the function, how it would be called from a <see cref="Plugin"/>.</param>
        /// <param name="inputTypes">The types of the input arguments/parameters the <see cref="Plugin"/> caller will provide.</param>
        /// <param name="outputTypes">The types of the output returned from the host function to the <see cref="Plugin"/>.</param>
        /// <param name="userData">An opaque pointer to an object from the host, accessible to the <see cref="Plugin"/>.
        /// NOTE: it is the shared responsibility of the host and <see cref="Plugin"/> to cast/dereference this value properly.</param>
        /// <param name="hostFunction"></param>
        unsafe public HostFunction(
            string name,
            ExtismValType[] inputTypes,
            ExtismValType[] outputTypes,
            IntPtr userData,
            ExtismFunction hostFunction)
        {
            fixed (ExtismValType* inputs = inputTypes)
            fixed (ExtismValType* outputs = outputTypes)
            {
                NativeHandle = LibExtism.extism_function_new(name, inputs, inputTypes.Length, outputs, outputTypes.Length, CallbackImpl, userData, IntPtr.Zero);
            }

            void CallbackImpl(
                nint plugin,
                ExtismVal[] inputs,
                uint n_inputs,
                ExtismVal[] outputs,
                uint n_outputs, IntPtr data)
            {
                hostFunction(new CurrentPlugin(plugin), inputs, outputs, data);
            }
        }

        internal IntPtr NativeHandle { get; }

        /// <summary>
        /// Set the namespace of a <see cref="HostFunction"/>.
        /// </summary>
        /// <param name="ns"></param>
        public void SetNamespace(string ns)
        {
            LibExtism.extism_function_set_namespace(NativeHandle, ns);
        }

        /// <summary>
        /// Frees all resources held by this Host Function.
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
        /// Throw an appropriate exception if the Host Function has been disposed.
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
            throw new ObjectDisposedException(nameof(HostFunction));
        }

        /// <summary>
        /// Frees all resources held by this Host Function.
        /// </summary>
        unsafe protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                // Free up any managed resources here
            }

            // Free up unmanaged resources
            LibExtism.extism_function_free(NativeHandle);
        }

        /// <summary>
        /// Destructs the current Host Function and frees all resources used by it.
        /// </summary>
        ~HostFunction()
        {
            Dispose(false);
        }
    }
}