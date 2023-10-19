package org.extism.sdk.wasm;

/**
 * WASM Source represented by raw bytes.
 */
public class ByteArrayWasmSource implements WasmSource {

    private final String name;
    private final byte[] data;
    private final String hash;


    /**
     * Constructor
     * @param name
     * @param data the byte array representing the WASM code
     * @param hash
     */
    public ByteArrayWasmSource(String name, byte[] data, String hash) {
        this.name = name;
        this.data = data;
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

    public byte[] data() {
        return data;
    }
}
