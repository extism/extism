package org.extism.sdk.manifest;

import com.google.gson.annotations.SerializedName;

/**
 * Configures memory for the Wasm runtime.
 * Memory is described in units of pages (64KB) and represent contiguous chunks of addressable memory.
 *
 * @param max Max number of pages.
 */
public class MemoryOptions {

    private @SerializedName("max") Integer max;

    public MemoryOptions(Integer max) {
        this.max = max;
    }
}
