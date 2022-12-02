package org.extism.sdk;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
//import com.sun.jna.Platform;

public interface LibExtism extends Library {
    LibExtism INSTANCE = (LibExtism) Native.loadLibrary("/usr/local/lib/libextism.dylib", LibExtism.class);

    Pointer extism_context_new();

    void extism_context_free(Pointer contextPointer);

    void extism_context_reset(Pointer contextPointer);

    String extism_error(Pointer contextPointer, int pluginId);

    int extism_plugin_new(long contextPointer, byte[] wasm, long wasmSize, boolean withWASI);

    String extism_version();

    void extism_plugin_new(Pointer contextPointer, byte[] wasm, int length, boolean withWASI,
            IntByReference pluginIndex);

    boolean extism_plugin_update(Pointer contextPointer, int pluginIndex, byte[] wasm, int length, boolean withWASI);

    void extism_plugin_free(Pointer contextPointer, int pluginIndex);

    boolean extism_plugin_config(Pointer contextPointer, int pluginIndex, byte[] json, int jsonLength);
}
