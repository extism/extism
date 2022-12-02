package org.extism.sdk;

import org.extism.sdk.LibExtism;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;

// ExtismContext is used to store and manage plugins
public class ExtismContext {

    // A pointer to the ExtismContext struct
    private Pointer contextPointer;

    // Create a new context
    public ExtismContext() {
        this.contextPointer = LibExtism.INSTANCE.extism_context_new();
    }

    public Pointer getPointer() {
        return this.contextPointer;
    }

    // Free a context
    public void free() {
        LibExtism.INSTANCE.extism_context_free(this.contextPointer);
    }

    // Create a new plugin
    public ExtismPlugin newPlugin(byte[] wasm, boolean withWASI) {
        return new ExtismPlugin(this, wasm, withWASI);
    }

    // Remove all plugins from the registry
    public void reset() {
        LibExtism.INSTANCE.extism_context_reset(this.contextPointer);
    }

    // Get the error associated with a context, if plugin is null then the context
    // error will be returned
    public String error(ExtismPlugin plugin) {
        return LibExtism.INSTANCE.extism_error(this.contextPointer, plugin == null ? -1 : plugin.getIndex());
    }

    // Get the Extism version string
    public String version() {
        return LibExtism.INSTANCE.extism_version();
    }
}
