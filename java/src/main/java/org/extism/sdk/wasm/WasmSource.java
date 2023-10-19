package org.extism.sdk.wasm;

/**
 * A named WASM source.
 */
public interface WasmSource {

    /**
     * Logical name of the WASM source
     * @return
     */
    String name();

    /**
     * Hash of the WASM source
     * @return
     */
    String hash();
}
