using Extism.Sdk.Native;

namespace Extism.Sdk
{
    public delegate void ExtismFunction(IntPtr plugin, Span<ExtismVal> inputs, Span<ExtismVal> outputs, IntPtr data);

    public class HostFunction
    {
        // TODO: Make disposable

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
                Native = LibExtism.extism_function_new(name, inputs, inputTypes.Length, outputs, outputTypes.Length, CallbackImpl, userData, IntPtr.Zero);
            }

            void CallbackImpl(
                IntPtr plugin,
                Span<ExtismVal> inputs,
                uint n_inputs,
                Span<ExtismVal> outputs,
                uint n_outputs, IntPtr data)
            {
                hostFunction(plugin, inputs, outputs, data);
            }
        }

        internal IntPtr Native { get; }
    }
}