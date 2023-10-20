package org.extism.sdk;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.extism.sdk.manifest.Manifest;
import org.extism.sdk.manifest.MemoryOptions;
import org.extism.sdk.support.JsonSerde;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.extism.sdk.TestWasmSources.CODE;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class ManifestTests {

    @Test
    public void shouldSerializeManifestWithWasmSourceToJson() {
        var externalPath = "/tmp/foo";
        var internalPath = "/tmp/extism-plugins/foo";
        var paths = Map.of(externalPath, internalPath);
        var manifest = new Manifest(List.of(CODE.pathWasmSource()), null, null, null, paths);
        var jsonString = JsonSerde.toJson(manifest);
        assertNotNull(jsonString);

        var json = parseAsJsonObject(jsonString);
        assertThat(json.get("wasm").isJsonArray()).isTrue();
        assertThat(json.get("wasm").getAsJsonArray()).hasSize(1);
        assertThat(json.get("allowed_paths").isJsonObject()).isTrue();
        assertThat(json.get("allowed_paths").getAsJsonObject().get(externalPath).getAsString()).isEqualTo(internalPath);
    }

    @Test
    public void shouldSerializeManifestWithWasmSourceAndMemoryOptionsToJson() {

        var manifest = new Manifest(List.of(CODE.pathWasmSource()), new MemoryOptions(4));
        var jsonString = JsonSerde.toJson(manifest);
        assertNotNull(jsonString);

        var json = parseAsJsonObject(jsonString);
        assertThat(json.get("wasm").isJsonArray()).isTrue();
        assertThat(json.get("wasm").getAsJsonArray()).hasSize(1);
        assertThat(json.get("memory").getAsJsonObject().get("max").getAsInt()).isEqualTo(4);
    }

    @Test
    public void codeWasmFromFileAndBytesShouldProduceTheSameHash() {

        var byteHash = CODE.byteArrayWasmSource().hash();
        var fileHash = CODE.pathWasmSource().hash();

        assertThat(byteHash).isEqualTo(fileHash);
    }

    private static JsonObject parseAsJsonObject(String json) {
        return JsonParser.parseString(json).getAsJsonObject();
    }
}
