package org.extism.sdk.wasm;

/**
 * WASM Source represented by a file referenced by a path.
 */
public class PathWasmSource implements WasmSource {

    private final String name;

    private final String path;

    private final String hash;

    /**
     * Constructor
     * @param name
     * @param path
     * @param hash
     */
    public PathWasmSource(String name, String path, String hash) {
        this.name = name;
        this.path = path;
        this.hash = hash;
    }

    @Override
    public String name() {
        return name;
    }

    @Override
    public String hash() {
        return hash;
    }

    public String path() {
        return path;
    }
}

