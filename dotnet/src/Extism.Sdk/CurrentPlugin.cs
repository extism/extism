using Extism.Sdk.Native;

using System.Text;

namespace Extism.Sdk
{
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
        public unsafe void WriteBytes(nint pointer, Span<byte> bytes)
        {
            var length = BlockLength(pointer);
            if (length < bytes.Length)
            {
                throw new InvalidOperationException("Destination block length is less than source block length.");
            }

            var mem = GetMemory();
            var ptr = (void*)(mem + pointer);
            var destination = new Span<byte>(ptr, bytes.Length);

            bytes.CopyTo(destination);
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
}