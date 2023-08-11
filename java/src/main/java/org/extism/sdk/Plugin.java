package org.extism.sdk;

import com.sun.jna.Pointer;
import org.extism.sdk.manifest.Manifest;
import org.extism.sdk.support.JsonSerde;

import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * Represents a Extism plugin.
 */
public class Plugin implements AutoCloseable {

    /**
     * Holds the Extism plugin pointer
     */
    private final Pointer pluginPointer;

    /**
     * @param manifestBytes The manifest for the plugin
     * @param functions     The Host functions for th eplugin
     * @param withWASI      Set to true to enable WASI
     */
    public Plugin(byte[] manifestBytes, boolean withWASI, HostFunction[] functions) {

        Objects.requireNonNull(manifestBytes, "manifestBytes");

        Pointer[] ptrArr = new Pointer[functions == null ? 0 : functions.length];

        if (functions != null)
            for (int i = 0; i < functions.length; i++) {
               ptrArr[i] = functions[i].pointer;
            }

        Pointer[] errormsg = new Pointer[1];
        Pointer p = LibExtism.INSTANCE.extism_plugin_new(manifestBytes, manifestBytes.length,
                ptrArr,
                functions == null ? 0 : functions.length,
                withWASI,
                errormsg);
        if (p == null) {
            int errlen = LibExtism.INSTANCE.strlen(errormsg[0]);
            byte[] msg = new byte[errlen];
            errormsg[0].read(0, msg, 0, errlen);
            LibExtism.INSTANCE.extism_plugin_error_free(errormsg[0]);
            throw new ExtismException(new String(msg));
        }

        this.pluginPointer = p;
    }

    public Plugin(Manifest manifest, boolean withWASI, HostFunction[] functions) {
        this(serialize(manifest), withWASI, functions);
    }


    private static byte[] serialize(Manifest manifest) {
        Objects.requireNonNull(manifest, "manifest");
        return JsonSerde.toJson(manifest).getBytes(StandardCharsets.UTF_8);
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

        int inputDataLength = inputData == null ? 0 : inputData.length;
        int exitCode = LibExtism.INSTANCE.extism_plugin_call(this.pluginPointer, functionName, inputData, inputDataLength);
        if (exitCode == -1) {
            String error = this.error();
            throw new ExtismException(error);
        }

        int length = LibExtism.INSTANCE.extism_plugin_output_length(this.pluginPointer);
        Pointer output = LibExtism.INSTANCE.extism_plugin_output_data(this.pluginPointer);
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
     * Get the error associated with a plugin
     *
     * @return the error message
     */
    protected String error() {
        return LibExtism.INSTANCE.extism_error(this.pluginPointer);
    }

    /**
     * Frees a plugin from memory
     */
    public void free() {
        LibExtism.INSTANCE.extism_plugin_free(this.pluginPointer);
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
        return LibExtism.INSTANCE.extism_plugin_config(this.pluginPointer, jsonBytes, jsonBytes.length);
    }

    /**
     * Calls {@link #free()} if used in the context of a TWR block.
     */
    @Override
    public void close() {
        free();
    }

    /**
     * Return a new `CancelHandle`, which can be used to cancel a running Plugin
     */
    public CancelHandle cancelHandle() {
        Pointer handle = LibExtism.INSTANCE.extism_plugin_cancel_handle(this.pluginPointer);
        return new CancelHandle(handle);
    }
}
