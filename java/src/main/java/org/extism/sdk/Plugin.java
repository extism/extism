package org.extism.sdk;

import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import org.extism.sdk.manifest.Manifest;
import org.extism.sdk.support.JsonSerde;

import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * Represents a Extism plugin.
 */
public class Plugin implements AutoCloseable {

    /**
     * Holds the Extism {@link Context} that the plugin belongs to.
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
     * @param context       The context to manage the plugin
     * @param manifestBytes The manifest for the plugin
     * @param withWASI      Set to true to enable WASI
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
        this(context, serialize(manifest), withWASI);
    }

    private static byte[] serialize(Manifest manifest) {
        Objects.requireNonNull(manifest, "manifest");
        return JsonSerde.toJson(manifest).getBytes(StandardCharsets.UTF_8);
    }

    /**
     * Getter for the internal index pointer to this plugin.
     *
     * @return the plugin index
     */
    public int getIndex() {
        return index;
    }

    /**
     * Invoke a function with the given name and input.
     *
     * @param functionName The name of the exported function to invoke
     * @param inputData    The raw bytes representing any input data
     * @return A byte array representing the raw output data
     * @throws ExtismException if the call fails
     */
    public byte[] call(String functionName, byte[] inputData) {

        Objects.requireNonNull(functionName, "functionName");

        Pointer contextPointer = context.getPointer();
        int inputDataLength = inputData == null ? 0 : inputData.length;
        int callExitCode = LibExtism.INSTANCE.extism_plugin_call(contextPointer, index, functionName, inputData, inputDataLength);
        if (callExitCode == -1) {
            String error = context.error(this);
            throw new ExtismException(error);
        }

        int length = LibExtism.INSTANCE.extism_plugin_output_length(contextPointer, index);
        Pointer output = LibExtism.INSTANCE.extism_plugin_output_data(contextPointer, index);
        return output.getByteArray(0, length);
    }

    /**
     * Invoke a function with the given name and input.
     *
     * @param functionName The name of the exported function to invoke
     * @param input        The string representing the input data
     * @return A string representing the output data
     */
    public String call(String functionName, String input) {

        Objects.requireNonNull(functionName, "functionName");

        var inputBytes = input == null ? null : input.getBytes(StandardCharsets.UTF_8);
        var outputBytes = call(functionName, inputBytes);
        return new String(outputBytes, StandardCharsets.UTF_8);
    }

    /**
     * Update the plugin code given manifest changes
     *
     * @param manifest The manifest for the plugin
     * @param withWASI Set to true to enable WASI
     * @return {@literal true} if update was successful
     */
    public boolean update(Manifest manifest, boolean withWASI) {
        Objects.requireNonNull(manifest, "manifest");
        return update(serialize(manifest), withWASI);
    }

    /**
     * Update the plugin code given manifest changes
     *
     * @param manifestBytes The manifest for the plugin
     * @param withWASI      Set to true to enable WASI
     * @return {@literal true} if update was successful
     */
    public boolean update(byte[] manifestBytes, boolean withWASI) {
        Objects.requireNonNull(manifestBytes, "manifestBytes");
        return LibExtism.INSTANCE.extism_plugin_update(context.getPointer(), index, manifestBytes, manifestBytes.length, withWASI);
    }

    /**
     * Frees a plugin from memory. Plugins will be automatically cleaned up
     * if you free their parent Context using {@link org.extism.sdk.Context#free() free()} or {@link org.extism.sdk.Context#reset() reset()}
     */
    public void free() {
        LibExtism.INSTANCE.extism_plugin_free(context.getPointer(), index);
    }

    /**
     * Update plugin config values, this will merge with the existing values.
     *
     * @param json
     * @return
     */
    public boolean updateConfig(String json) {
        Objects.requireNonNull(json, "json");
        return updateConfig(json.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Update plugin config values, this will merge with the existing values.
     *
     * @param jsonBytes
     * @return {@literal true} if update was successful
     */
    public boolean updateConfig(byte[] jsonBytes) {
        Objects.requireNonNull(jsonBytes, "jsonBytes");
        return LibExtism.INSTANCE.extism_plugin_config(context.getPointer(), index, jsonBytes, jsonBytes.length);
    }

    /**
     * Calls {@link #free()} if used in the context of a TWR block.
     */
    @Override
    public void close() {
        free();
    }
}
