package org.extism.sdk;

import com.sun.jna.Pointer;
import org.extism.sdk.manifest.Manifest;

/**
 * Extism Context is used to store and manage plugins.
 */
public class Context implements AutoCloseable {

    /**
     * Holds a pointer to the native ExtismContext struct.
     */
    private final Pointer contextPointer;

    /**
     * Creates a new context.
     * <p>
     * A Context is used to manage Plugins
     * and make sure they are cleaned up when you are done with them.
     */
    public Context() {
        this.contextPointer = LibExtism.INSTANCE.extism_context_new();
    }

    /**
     * Create a new plugin managed by this context.
     *
     * @param manifest The manifest for the plugin
     * @param withWASI Set to true to enable WASI
     * @param functions List of Host functions
     * @return the plugin instance
     */
    public Plugin newPlugin(Manifest manifest, boolean withWASI, HostFunction[] functions) {
        return new Plugin(this, manifest, withWASI, functions);
    }

    /**
     * Frees the context *and* frees all its Plugins. Use {@link #reset()}, if you just want to
     * free the plugins but keep the context. You should ensure this is called when you are done
     * with the context.
     */
    public void free() {
        LibExtism.INSTANCE.extism_context_free(this.contextPointer);
    }

    /**
     * Resets the context by freeing all its Plugins. Unlike {@link #free()}, it does not
     * free the context itself.
     */
    public void reset() {
        LibExtism.INSTANCE.extism_context_reset(this.contextPointer);
    }

    /**
     * Get the version string of the linked Extism Runtime.
     *
     * @return the version
     */
    public String getVersion() {
        return LibExtism.INSTANCE.extism_version();
    }

    /**
     * Get the error associated with a context, if plugin is {@literal null} then the context error will be returned.
     *
     * @param plugin
     * @return the error message
     */
    protected String error(Plugin plugin) {
        return LibExtism.INSTANCE.extism_error(this.contextPointer, plugin == null ? -1 : plugin.getIndex());
    }

    /**
     * Return the raw pointer to this context.
     *
     * @return the pointer
     */
    public Pointer getPointer() {
        return this.contextPointer;
    }

    /**
     * Calls {@link #free()} if used in the context of a TWR block.
     */
    @Override
    public void close() {
        this.free();
    }
}
