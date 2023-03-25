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
    public delegate void ExtismFunction(int plugin, ExtismVal[] inputs, ExtismVal[] outputs, IntPtr userData);

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
        /// <param name="name">Name of the function.</param>
        /// <param name="inputTypes">Parameter types.</param>
        /// <param name="outputTypes">Return types.</param>
        /// <param name="userData">A data that will be passed into the host function when called by plugins.</param>
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
                int plugin,
                ExtismVal[] inputs,
                uint n_inputs,
                ExtismVal[] outputs,
                uint n_outputs, IntPtr data)
            {
                hostFunction(plugin, inputs, outputs, data);
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