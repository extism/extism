package org.extism.sdk;

import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import org.extism.sdk.manifest.Manifest;
import org.extism.sdk.support.JsonSerializer;

import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * Represents a Extism plugin.
 */
public class Plugin implements AutoCloseable {

    /**
     * Holds the The Extism {@link Context} that the plugin belongs to.
     */
    private final Context context;

    /**
     * Holds the index of the plugin
     */
    private final int index;

    /**
     * Constructor for a Plugin. Only expose internally. Plugins should be created and
     * managed from {@link org.extism.sdk.Context}.
     *
     * @param context The context to manage the plugin
     * @param manifestBytes The manifest for the plugin
     * @param withWASI Set to true to enable WASI
     */
    public Plugin(Context context, byte[] manifestBytes, boolean withWASI) {

        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(manifestBytes, "manifestBytes");

        IntByReference pluginIndex = new IntByReference();
        LibExtism.INSTANCE.extism_plugin_new(context.getPointer(), manifestBytes, manifestBytes.length, withWASI, pluginIndex);
        this.index = pluginIndex.getValue();
        this.context = context;
    }

    public Plugin(Context context, Manifest manifest, boolean withWASI) {
        this(context, JsonSerializer.toJson(manifest).getBytes(), withWASI);
    }

    /**
     * Getter for the internal index pointer to this plugin.
     * @return
     */
    public int getIndex() {
        return this.index;
    }

    /**
     * Invoke a function with the given name and input.
     *
     * @param functionName The name of the exported function to invoke
     * @param inputData The raw bytes representing any input data
     * @return A byte array representing the raw output data
     */
    public byte[] call(String functionName, byte[] inputData) {
        int result = LibExtism.INSTANCE.extism_plugin_call(this.context.getPointer(), this.index, functionName, inputData, inputData.length);
        if (result == -1) {
            throw new ExtismException("Call returned -1");
        }
        int length = LibExtism.INSTANCE.extism_plugin_output_length(this.context.getPointer(), this.index);
        Pointer output = LibExtism.INSTANCE.extism_plugin_output_data(this.context.getPointer(), this.index);
        return output.getByteArray(0, length);
    }

    /**
     * Invoke a function with the given name and input.
     *
     * @param functionName The name of the exported function to invoke
     * @param input The string representing the input data
     * @return A string representing the output data
     */
    public String call(String functionName, String input) {
        var inputBytes = input.getBytes(StandardCharsets.UTF_8);
        var outputBytes = call(functionName, inputBytes);
        return new String(outputBytes, StandardCharsets.UTF_8);
    }

    /**
     * Update the plugin code given manifest changes
     *
     * @param manifest The manifest for the plugin
     * @param withWASI Set to true to enable WASI
     * @return Returns true if update was successful
     */
    public boolean update(Manifest manifest, boolean withWASI) {
        byte[] manifestBytes = JsonSerializer.toJson(manifest).getBytes(StandardCharsets.UTF_8);
        return update(manifestBytes,withWASI);
    }

    /**
     * Update the plugin code given manifest changes
     *
     * @param manifestBytes The manifest for the plugin
     * @param withWASI Set to true to enable WASI
     * @return Returns true if update was successful
     */
    public boolean update(byte[] manifestBytes, boolean withWASI) {
        return LibExtism.INSTANCE.extism_plugin_update(this.context.getPointer(), this.index, manifestBytes, manifestBytes.length, withWASI);
    }

    /**
     * Frees a plugin from memory. Plugins will be automatically cleaned up
     * if you free their parent Context using {@link org.extism.sdk.Context#free() free()} or {@link org.extism.sdk.Context#reset() reset()}
     */
    public void free() {
        LibExtism.INSTANCE.extism_plugin_free(this.context.getPointer(), this.index);
    }

    /**
     * Update plugin config values, this will merge with the existing values.
     *
     * @param json
     * @return
     */
    public boolean updateConfig(String json) {
        return updateConfig(json.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Update plugin config values, this will merge with the existing values.
     *
     * @param jsonBytes
     * @return
     */
    public boolean updateConfig(byte[] jsonBytes) {
        return LibExtism.INSTANCE.extism_plugin_config(this.context.getPointer(), this.index, jsonBytes, jsonBytes.length);
    }

    /**
     * Calls {@link #free()} if used in the context of a TWR block.
     */
    @Override
    public void close() {
        free();
    }
}
