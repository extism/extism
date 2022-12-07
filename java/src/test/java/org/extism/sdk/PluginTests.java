package org.extism.sdk;

import org.extism.sdk.manifest.Manifest;
import org.extism.sdk.manifest.MemoryOptions;
import org.extism.sdk.wasm.ByteArrayWasmSource;
import org.extism.sdk.wasm.PathWasmSource;
import org.extism.sdk.wasm.WasmSourceResolver;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class PluginTests {

    @Test
    public void shouldInvokeFunctionWithMemoryOptions() {
        //FIXME check whether memory options are effective
        var manifest = new Manifest(List.of(exampleFileWasmSource()), new MemoryOptions(0));
        var output = invokeFunction(manifest, "count_vowels", "Hello World");
        assertThat(output).isEqualTo("{\"count\": 3}");
    }

    @Test
    public void shouldInvokeFunctionWithConfig() {
        //FIXME check if config options are available in wasm call
        var config = Map.of("key1", "value1");
        var manifest = new Manifest(List.of(exampleFileWasmSource()), null, config);
        var output = invokeFunction(manifest, "count_vowels", "Hello World");
        assertThat(output).isEqualTo("{\"count\": 3}");
    }

    @Test
    public void shouldInvokeFunctionFromFileWasmSource() {
        var manifest = new Manifest(exampleFileWasmSource());
        var output = invokeFunction(manifest, "count_vowels", "Hello World");
        assertThat(output).isEqualTo("{\"count\": 3}");
    }

    @Test
    public void shouldInvokeFunctionFromByteArrayWasmSource() {
        var manifest = new Manifest(exampleByteArrayWasmSource());
        var output = invokeFunction(manifest, "count_vowels", "Hello World");
        assertThat(output).isEqualTo("{\"count\": 3}");
    }

    @Test
    public void shouldFailToInvokeUnknownFunction() {
        assertThrows(ExtismException.class, () -> {
            var manifest = new Manifest(exampleFileWasmSource());
            invokeFunction(manifest, "unknown", "dummy");
        }, "Function not found: unknown");
    }

    @Test
    public void shouldAllowInvokeFunctionFromFileWasmSourceMultipleTimes() {
        var wasmSource = exampleFileWasmSource();
        var manifest = new Manifest(wasmSource);
        var output = invokeFunction(manifest, "count_vowels", "Hello World");
        assertThat(output).isEqualTo("{\"count\": 3}");

        output = invokeFunction(manifest, "count_vowels", "Hello World");
        assertThat(output).isEqualTo("{\"count\": 3}");
    }

    @Test
    public void shouldAllowInvokeFunctionFromFileWasmSourceApiUsageExample() {

        var wasmSourceResolver = new WasmSourceResolver();
        var manifest = new Manifest(wasmSourceResolver.resolve(getExampleWasmFilePath()));

        var functionName = "count_vowels";
        var input = "Hello World";

        try (var ctx = new Context()) {
            try (var plugin = ctx.newPlugin(manifest, false)) {
                var output = plugin.call(functionName, input);
                assertThat(output).isEqualTo("{\"count\": 3}");
            }
        }
    }

    @Test
    public void shouldAllowInvokeFunctionFromFileWasmSourceMultipleTimesByReusingContext() {

        var manifest = new Manifest(exampleFileWasmSource());
        var functionName = "count_vowels";
        var input = "Hello World";

        try (var ctx = new Context()) {
            try (var plugin = ctx.newPlugin(manifest, false)) {
                var output = plugin.call(functionName, input);
                assertThat(output).isEqualTo("{\"count\": 3}");

                output = plugin.call(functionName, input);
                assertThat(output).isEqualTo("{\"count\": 3}");
            }
        }
    }

    private String invokeFunction(Manifest manifest, String functionName, String input) {
        try (var ctx = new Context()) {
            try (var plugin = ctx.newPlugin(manifest, false)) {
                return plugin.call(functionName, input);
            }
        }
    }

    private Path getExampleWasmFilePath() {
        return Paths.get("src", "test", "resources", "code.wasm");
    }

    private PathWasmSource exampleFileWasmSource() {
        return resolveFileWasmSource(getExampleWasmFilePath());
    }

    private ByteArrayWasmSource exampleByteArrayWasmSource() {
        try {
            var wasmBytes = Files.readAllBytes(getExampleWasmFilePath());
            return new WasmSourceResolver().resolve("wasm@" + Arrays.hashCode(wasmBytes), wasmBytes);
        } catch (IOException ioe) {
            throw new RuntimeException(ioe);
        }
    }

    public static PathWasmSource resolveFileWasmSource(Path path) {
        return new WasmSourceResolver().resolve(path);
    }

}
