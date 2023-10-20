package org.extism.sdk.manifest;

import com.google.gson.annotations.SerializedName;

/**
 * Configures memory for the Wasm runtime.
 * Memory is described in units of pages (64KB) and represent contiguous chunks of addressable memory.
 */
public class MemoryOptions {

    /**
     * The max number of WebAssembly pages that should be allocated for the module.
     */
    @SerializedName("max")
    private final Integer max;

    public MemoryOptions(Integer max) {
        this.max = max;
    }
}
