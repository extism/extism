package org.extism.sdk;

import org.extism.sdk.manifest.Manifest;

import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;

// Represents a plugin
public class Plugin {
    // The ExtismContext that the plugin belongs to
    private Context context;

    // The index of the plugin
    private int index;

    /**
     * Constructor for a Plugin. Only expose internally. Plugins should be created and
     * managed from {@link org.extism.sdk.Context}.
     * 
     * @param context The context to manage the plugin
     * @param manifest The manifest for the plugin
     * @param withWASI Set to true to enable WASI
     */
    protected Plugin(Context context, Manifest manifest, boolean withWASI) {
        byte[] manifestJson = manifest.toJson().getBytes();
        this.context = context;
        IntByReference pluginIndex = new IntByReference();
        LibExtism.INSTANCE.extism_plugin_new(context.getPointer(), manifestJson, manifestJson.length, withWASI, pluginIndex);
        this.index = pluginIndex.getValue();
    }

    /**
     * Getter for the internal index pointer to this plugin
     * @return 
     */
    protected int getIndex() {
        return this.index;
    }

    /**
     * Invoke a function with the given name and input
     * 
     * @param function_name The name of the exported function to invoke
     * @param inputData The raw bytes representing any input data
     * @return A byte array representing the raw output data
     */
    public byte[] call(String function_name, byte[] inputData) {
        int _result = LibExtism.INSTANCE.extism_plugin_call(this.context.getPointer(), this.index, function_name, inputData, inputData.length);
        int length = LibExtism.INSTANCE.extism_plugin_output_length(this.context.getPointer(), this.index);
        Pointer output = LibExtism.INSTANCE.extism_plugin_output_data(this.context.getPointer(), this.index);
        return output.getByteArray(0, length);
    }

    /**
     * Update the plugin code given manifest changes
     * 
     * @param manifest The manifest for the plugin
     * @param withWASI Set to true to enable WASI
     * @return Returns true if update was successful
     */
    public boolean update(Manifest manifest, boolean withWASI) {
        byte[] manifestJson = manifest.toJson().getBytes();
        return LibExtism.INSTANCE.extism_plugin_update(this.context.getPointer(), this.index, manifestJson, manifestJson.length,
                withWASI);
    }

    /**
     * Frees a plugin from memory. Plugins will be automatically cleaned up
     * if you free their parent Context using {@link org.extism.sdk.Context#free() free()} or {@link org.extism.sdk.Context#reset() reset()} 
     */
    public void free() {
        LibExtism.INSTANCE.extism_plugin_free(this.context.getPointer(), this.index);
    }

    /**
     * Update the config for the plugin
     * 
     * @param json
     * @return
     */
    public boolean config(byte[] json) {
        return LibExtism.INSTANCE.extism_plugin_config(this.context.getPointer(), this.index, json, json.length);
    }
}