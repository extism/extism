package org.extism.sdk;

import org.extism.sdk.manifest.Manifest;
import org.extism.sdk.wasm.FileBasedWasmSource;
import org.junit.jupiter.api.Test;

import java.nio.file.Paths;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

public class ExtismTest {

    @Test
    public void shouldInvokeNamedFunction() {

        var wasmFile = Paths.get("src", "test", "resources", "code.wasm").toFile();
        try (var ctx = new Context()) {
            var wasmSource = new FileBasedWasmSource(wasmFile.getName(), wasmFile.getAbsolutePath(), null);
            var manifest = new Manifest(List.of(wasmSource));

            try (var plugin = ctx.newPlugin(manifest, false)) {
                var input = "Hello World";
                var functionName = "count_vowels";
                var output = plugin.call(functionName, input);
                assertThat(output).isEqualTo("{\"count\": 3}");
            }
        }
    }

    @Test
    public void shouldReturnVersionString() {
        var version = LibExtism.INSTANCE.extism_version();
        assertThat(version).isNotNull();
    }
}
