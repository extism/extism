package org.extism.sdk;

import com.sun.jna.Memory;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;

// Represents a plugin
public class Plugin {

    // The ExtismContext that the plugin belongs to
    private Context context;

    // The index of the plugin
    private int index;

    // Create a new plugin
    public Plugin(Context context, byte[] wasm, boolean withWASI) {
        this.context = context;
        IntByReference pluginIndex = new IntByReference();
        LibExtism.INSTANCE.extism_plugin_new(context.getPointer(), wasm, wasm.length, withWASI, pluginIndex);
        this.index = pluginIndex.getValue();
    }

    public int getIndex() {
        return this.index;
    }

    // Call a plugin
    public byte[] call(String function_name, byte[] inputData) {
        int _result = LibExtism.INSTANCE.extism_plugin_call(this.context.getPointer(), this.index, function_name, inputData, inputData.length);
        int length = LibExtism.INSTANCE.extism_plugin_output_length(this.context.getPointer(), this.index);
        Pointer output = LibExtism.INSTANCE.extism_plugin_output_data(this.context.getPointer(), this.index);
        return output.getByteArray(0, length);
    }

    // Update a plugin, keeping the existing ID
    public boolean update(byte[] wasm, boolean withWASI) {
        return LibExtism.INSTANCE.extism_plugin_update(this.context.getPointer(), this.index, wasm, wasm.length,
                withWASI);
    }

    // Remove a plugin from the registry and free associated memory
    public void free() {
        LibExtism.INSTANCE.extism_plugin_free(this.context.getPointer(), this.index);
    }

    // Update plugin config values, this will merge with the existing values
    public boolean config(byte[] json) {
        return LibExtism.INSTANCE.extism_plugin_config(this.context.getPointer(), this.index, json, json.length);
    }
}