using Extism.Sdk.Native;

using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using System.Text;

namespace Extism.Sdk
{
    /// <summary>
    /// A host function signature.
    /// </summary>
    /// <param name="plugin">Plugin Index</param>
    /// <param name="inputs">Input parameters</param>
    /// <param name="outputs">Output parameters, the host function can change this.</param>
    /// <param name="userData">A data passed in during Host Function creation.</param>
    public delegate void ExtismFunction(CurrentPlugin plugin, ExtismVal[] inputs, Span<ExtismVal> outputs, IntPtr userData);

    /// <summary>
    /// Represents the current plugin. Can only be used within <see cref="HostFunction"/>s.
    /// </summary>
    public class CurrentPlugin
    {
        internal CurrentPlugin(nint nativeHandle)
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


        /// <summary>
        /// Reads a string from a memory block using UTF8.
        /// </summary>
        /// <param name="pointer"></param>
        /// <returns></returns>
        public string ReadString(nint pointer)
        {
            return ReadString(pointer, Encoding.UTF8);
        }

        /// <summary>
        /// Reads a string form a memory block.
        /// </summary>
        /// <param name="pointer"></param>
        /// <param name="encoding"></param>
        /// <returns></returns>
        public string ReadString(nint pointer, Encoding encoding)
        {
            var buffer = ReadBytes(pointer);

            return encoding.GetString(buffer);
        }

        /// <summary>
        /// Returns a span of bytes for a given block.
        /// </summary>
        /// <param name="pointer"></param>
        /// <returns></returns>
        public unsafe Span<byte> ReadBytes(nint pointer)
        {
            var mem = GetMemory();
            var length = (int)BlockLength(pointer);
            var ptr = (byte*)mem + pointer;

            return new Span<byte>(ptr, length);
        }

        /// <summary>
        /// Writes a string into the current plugin memory using UTF-8 encoding and returns the pointer of the block.
        /// </summary>
        /// <param name="value"></param>
        public nint WriteString(string value)
            => WriteString(value, Encoding.UTF8);

        /// <summary>
        /// Writes a string into the current plugin memory and returns the pointer of the block.
        /// </summary>
        /// <param name="value"></param>
        /// <param name="encoding"></param>
        public nint WriteString(string value, Encoding encoding)
        {
            var bytes = encoding.GetBytes(value);
            var pointer = AllocateBlock(bytes.Length);
            WriteBytes(pointer, bytes);

            return pointer;
        }

        /// <summary>
        /// Writes a byte array into a block of memory.
        /// </summary>
        /// <param name="pointer"></param>
        /// <param name="bytes"></param>
        public void WriteBytes(nint pointer, byte[] bytes)
        {
            var length = BlockLength(pointer);
            if (length < bytes.Length)
            {
                throw new InvalidOperationException("Destination block length is less than source block length.");
            }

            var mem = GetMemory();
            Marshal.Copy(bytes, 0, mem + pointer, bytes.Length);
        }

        /// <summary>
        /// Frees a block of memory belonging to the current plugin.
        /// </summary>
        /// <param name="pointer"></param>
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
                ExtismVal* outputs,
                uint n_outputs,
                IntPtr data)
            {
                var buffer = new Span<ExtismVal>(outputs, (int)n_outputs);

                hostFunction(new CurrentPlugin(plugin), inputs, buffer, data);
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