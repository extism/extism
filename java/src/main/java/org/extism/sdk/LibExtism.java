package org.extism.sdk;

import com.sun.jna.*;

/**
 * Wrapper around the Extism library.
 */
public interface LibExtism extends Library {

    /**
     * Holds the extism library instance.
     * Resolves the extism library based on the resolution algorithm defined in {@link com.sun.jna.NativeLibrary}.
     */
    LibExtism INSTANCE = Native.load("extism", LibExtism.class);

    interface InternalExtismFunction extends Callback {
        void invoke(
                Pointer currentPlugin,
                ExtismVal inputs,
                int nInputs,
                ExtismVal outputs,
                int nOutputs,
                Pointer data
        );
    }

    @Structure.FieldOrder({"t", "v"})
    class ExtismVal extends Structure {
        public int t;
        public ExtismValUnion v;
    }

    class ExtismValUnion extends Union {
        public int i32;
        public long i64;
        public float f32;
        public double f64;
    }

    enum ExtismValType {
        I32(0),
        I64(1),
        F32(2),
        F64(3),
        V128(4),
        FuncRef(5),
        ExternRef(6);

        public final int v;

        ExtismValType(int value) {
            this.v = value;
        }
    }

    Pointer extism_function_new(String name,
                                int[] inputs,
                                int nInputs,
                                int[] outputs,
                                int nOutputs,
                                InternalExtismFunction func,
                                Pointer userData,
                                Pointer freeUserData);

    /**
     * Get the length of an allocated block
     * NOTE: this should only be called from host functions.
     */
    int extism_current_plugin_memory_length(Pointer plugin, long n);

    /**
     * Returns a pointer to the memory of the currently running plugin
     * NOTE: this should only be called from host functions.
     */
    Pointer extism_current_plugin_memory(Pointer plugin);

    /**
     * Allocate a memory block in the currently running plugin
     * NOTE: this should only be called from host functions.
     */
    int extism_current_plugin_memory_alloc(Pointer plugin, long n);

    /**
     * Free an allocated memory block
     * NOTE: this should only be called from host functions.
     */
    void extism_current_plugin_memory_free(Pointer plugin, long ptr);

        /**
     * Create a new context
     */
    Pointer extism_context_new();

    /**
     * Free a context
     */
    void extism_context_free(Pointer contextPointer);

    /**
     * Remove all plugins from the registry.
     *
     * @param contextPointer
     */
    void extism_context_reset(Pointer contextPointer);

    /**
     * Sets the logger to the given path with the given level of verbosity
     *
     * @param path     The file path of the logger
     * @param logLevel The level of the logger
     * @return true if successful
     */
    boolean extism_log_file(String path, String logLevel);

    /**
     * Returns the error associated with a @{@link Context} or @{@link Plugin}, if {@code pluginId} is {@literal -1} then the context error will be returned
     *
     * @param contextPointer
     * @param pluginId
     * @return
     */
    String extism_error(Pointer contextPointer, int pluginId);

    /**
     * Create a new plugin.
     *
     * @param contextPointer pointer to the {@link Context}.
     * @param wasm           is a WASM module (wat or wasm) or a JSON encoded manifest
     * @param wasmSize       the length of the `wasm` parameter
     * @param functions      host functions
     * @param nFunctions     the number of host functions
     * @param withWASI       enables/disables WASI
     * @return id of the plugin or {@literal -1} in case of error
     */
    int extism_plugin_new(Pointer contextPointer, byte[] wasm, long wasmSize, Pointer[] functions, int nFunctions, boolean withWASI);

    /**
     * Returns the Extism version string
     */
    String extism_version();


    /**
     * Calls a function from the @{@link Plugin} at the given {@code pluginIndex}.
     *
     * @param contextPointer
     * @param pluginIndex
     * @param function_name  is the function to call
     * @param data           is the data input data
     * @param dataLength     is the data input data length
     * @return the result code of the plugin call. {@literal -1} in case of error, {@literal 0} otherwise.
     */
    int extism_plugin_call(Pointer contextPointer, int pluginIndex, String function_name, byte[] data, int dataLength);

    /**
     * Returns the length of a plugin's output data.
     *
     * @param contextPointer
     * @param pluginIndex
     * @return the length of the output data in bytes.
     */
    int extism_plugin_output_length(Pointer contextPointer, int pluginIndex);

    /**
     * Returns the plugin's output data.
     *
     * @param contextPointer
     * @param pluginIndex
     * @return
     */
    Pointer extism_plugin_output_data(Pointer contextPointer, int pluginIndex);

    /**
     * Update a plugin, keeping the existing ID.
     * Similar to {@link #extism_plugin_new(Pointer, byte[], long, Pointer[], int, boolean)} but takes an {@code pluginIndex} argument to specify which plugin to update.
     * Note: Memory for this plugin will be reset upon update.
     *
     * @param contextPointer
     * @param pluginIndex
     * @param wasm
     * @param length
     * @param functions      host functions
     * @param nFunctions     the number of host functions
     * @param withWASI
     * @return {@literal true} if update was successful
     */
    boolean extism_plugin_update(Pointer contextPointer, int pluginIndex, byte[] wasm, int length, Pointer[] functions, int nFunctions, boolean withWASI);

    /**
     * Remove a plugin from the registry and free associated memory.
     *
     * @param contextPointer
     * @param pluginIndex
     */
    void extism_plugin_free(Pointer contextPointer, int pluginIndex);

    /**
     * Update plugin config values, this will merge with the existing values.
     *
     * @param contextPointer
     * @param pluginIndex
     * @param json
     * @param jsonLength
     * @return {@literal true} if update was successful
     */
    boolean extism_plugin_config(Pointer contextPointer, int pluginIndex, byte[] json, int jsonLength);
}
