package org.extism.sdk;

import org.extism.sdk.manifest.Manifest;
import org.extism.sdk.manifest.MemoryOptions;
import org.extism.sdk.support.JsonSerde;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.extism.sdk.TestWasmSources.CODE;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static uk.org.webcompere.modelassert.json.JsonAssertions.assertJson;

public class ManifestTests {

    @Test
    public void shouldSerializeManifestWithWasmSourceToJson() {

        var manifest = new Manifest(CODE.pathWasmSource());
        var json = JsonSerde.toJson(manifest);
        assertNotNull(json);

        assertJson(json).at("/wasm").isArray();
        assertJson(json).at("/wasm").hasSize(1);
    }

    @Test
    public void shouldSerializeManifestWithWasmSourceAndMemoryOptionsToJson() {

        var manifest = new Manifest(List.of(CODE.pathWasmSource()), new MemoryOptions(4));
        var json = JsonSerde.toJson(manifest);
        assertNotNull(json);

        assertJson(json).at("/wasm").isArray();
        assertJson(json).at("/wasm").hasSize(1);
        assertJson(json).at("/memory/max").isEqualTo(4);
    }
}