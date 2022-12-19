package org.extism.sdk.wasm;

/**
 * WASM Source represented by a file referenced by a path.
 *
 * @param name
 * @param path
 * @param hash
 */
public record PathWasmSource(String name, String path, String hash) implements WasmSource {
}

