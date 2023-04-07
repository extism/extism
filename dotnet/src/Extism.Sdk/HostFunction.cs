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
    public delegate void ExtismFunction(CurrentPlugin plugin, Span<ExtismVal> inputs, Span<ExtismVal> outputs, IntPtr userData);

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
        /// <param name="functionName">The literal name of the function, how it would be called from a <see cref="Plugin"/>.</param>
        /// <param name="inputTypes">The types of the input arguments/parameters the <see cref="Plugin"/> caller will provide.</param>
        /// <param name="outputTypes">The types of the output returned from the host function to the <see cref="Plugin"/>.</param>
        /// <param name="userData">An opaque pointer to an object from the host, accessible to the <see cref="Plugin"/>.
        /// NOTE: it is the shared responsibility of the host and <see cref="Plugin"/> to cast/dereference this value properly.</param>
        /// <param name="hostFunction"></param>
        public HostFunction(
            string functionName,
            Span<ExtismValType> inputTypes,
            Span<ExtismValType> outputTypes,
            IntPtr userData,
            ExtismFunction hostFunction) :
            this(functionName, "", inputTypes, outputTypes, userData, hostFunction)
        {

        }

        /// <summary>
        /// Registers a Host Function.
        /// </summary>
        /// <param name="functionName">The literal name of the function, how it would be called from a <see cref="Plugin"/>.</param>
        /// <param name="namespace">Function namespace.</param>
        /// <param name="inputTypes">The types of the input arguments/parameters the <see cref="Plugin"/> caller will provide.</param>
        /// <param name="outputTypes">The types of the output returned from the host function to the <see cref="Plugin"/>.</param>
        /// <param name="userData">An opaque pointer to an object from the host, accessible to the <see cref="Plugin"/>.
        /// NOTE: it is the shared responsibility of the host and <see cref="Plugin"/> to cast/dereference this value properly.</param>
        /// <param name="hostFunction"></param>
        unsafe public HostFunction(
            string functionName,
            string @namespace,
            Span<ExtismValType> inputTypes,
            Span<ExtismValType> outputTypes,
            IntPtr userData,
            ExtismFunction hostFunction)
        {
            fixed (ExtismValType* inputs = inputTypes)
            fixed (ExtismValType* outputs = outputTypes)
            {
                NativeHandle = LibExtism.extism_function_new(functionName, inputs, inputTypes.Length, outputs, outputTypes.Length, CallbackImpl, userData, IntPtr.Zero);
            }

            if (!string.IsNullOrEmpty(functionName))
            {
                LibExtism.extism_function_set_namespace(NativeHandle, @namespace);
            }

            void CallbackImpl(
                nint plugin,
                ExtismVal* inputsPtr,
                uint n_inputs,
                ExtismVal* outputsPtr,
                uint n_outputs,
                IntPtr data)
            {
                var outputs = new Span<ExtismVal>(outputsPtr, (int)n_outputs);
                var inputs = new Span<ExtismVal>(inputsPtr, (int)n_inputs);

                hostFunction(new CurrentPlugin(plugin), inputs, outputs, data);
            }
        }

        internal IntPtr NativeHandle { get; }

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