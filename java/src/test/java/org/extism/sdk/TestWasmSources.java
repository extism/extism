package org.extism.sdk;

import org.extism.sdk.wasm.ByteArrayWasmSource;
import org.extism.sdk.wasm.PathWasmSource;
import org.extism.sdk.wasm.WasmSourceResolver;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

public enum TestWasmSources {

    CODE {
        public Path getWasmFilePath() {
            return Paths.get(WASM_LOCATION, "code.wasm");
        }
        public Path getWasmFunctionsFilePath() {
            return Paths.get(WASM_LOCATION, "code-functions.wasm");
        }
    };

    public static final String WASM_LOCATION = "src/test/resources";

    public abstract Path getWasmFilePath();

    public abstract Path getWasmFunctionsFilePath();

    public PathWasmSource pathWasmSource() {
        return resolvePathWasmSource(getWasmFilePath());
    }

    public PathWasmSource pathWasmFunctionsSource() {
        return resolvePathWasmSource(getWasmFunctionsFilePath());
    }

    public ByteArrayWasmSource byteArrayWasmSource() {
        try {
            byte[] wasmBytes = Files.readAllBytes(getWasmFilePath());
            return new WasmSourceResolver().resolve("wasm@" + Arrays.hashCode(wasmBytes), wasmBytes);
        } catch (IOException ioe) {
            throw new RuntimeException(ioe);
        }
    }

    public static PathWasmSource resolvePathWasmSource(Path path) {
        return new WasmSourceResolver().resolve(path);
    }

}
