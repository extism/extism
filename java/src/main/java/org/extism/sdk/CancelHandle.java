package org.extism.sdk;

import com.sun.jna.Pointer;

/**
 * CancelHandle is used to cancel a running Plugin
 */
public class CancelHandle {
    private Pointer handle;

    public CancelHandle(Pointer handle) {
        this.handle = handle;
    }

    /**
     * Cancel execution of the Plugin associated with the CancelHandle
     */
    boolean cancel() {
        return LibExtism.INSTANCE.extism_plugin_cancel(this.handle);
    }
}
