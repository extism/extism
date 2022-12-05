package org.extism.sdk.wasm;

/**
 * A file based Wasm Source.
 *
 * @param name
 * @param path
 * @param hash
 */
public record FileBasedWasmSource(String name, String path, String hash) implements WasmSource {
}

