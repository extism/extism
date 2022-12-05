package org.extism.sdk.wasm;

/**
 * WASM Source represented by raw bytes.
 *
 * @param name
 * @param data the byte array representing the WASM code
 * @param hash
 */
public record ByteArrayWasmSource(String name, byte[] data, String hash) implements WasmSource {
}
