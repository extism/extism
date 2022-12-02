package org.extism.sdk;

import org.extism.sdk.manifest.ManifestWasmData;
import org.extism.sdk.manifest.ManifestWasm;
import org.extism.sdk.manifest.Manifest;

import com.sun.jna.Pointer;

// ExtismContext is used to store and manage plugins
public class Context {

    // A pointer to the ExtismContext struct
    private Pointer contextPointer;

    // Create a new context
    public Context() {
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
    public Plugin newPlugin(Manifest manifest, boolean withWASI) {
        ManifestWasmData wasm = (ManifestWasmData) manifest.wasm.get(0);
        return new Plugin(this, wasm.data, withWASI);
    }

    // Remove all plugins from the registry
    public void reset() {
        LibExtism.INSTANCE.extism_context_reset(this.contextPointer);
    }

    // Get the error associated with a context, if plugin is null then the context
    // error will be returned
    public String error(Plugin plugin) {
        return LibExtism.INSTANCE.extism_error(this.contextPointer, plugin == null ? -1 : plugin.getIndex());
    }

    // Get the Extism version string
    public String version() {
        return LibExtism.INSTANCE.extism_version();
    }
}
