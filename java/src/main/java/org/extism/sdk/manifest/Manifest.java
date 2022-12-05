package org.extism.sdk.manifest;

import org.extism.sdk.wasm.WasmSource;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class Manifest {

    private final List<WasmSource> wasm;

    private final ManifestMemory memory;

    private final List<String> allowedHosts;

    private final Map<String, String> config;

    public Manifest() {
        this(new ArrayList<>(), null, null, null);
    }

    public Manifest(List<WasmSource> sources) {
        this(sources, null, null, null);
    }

    public Manifest(List<WasmSource> sources, ManifestMemory memory) {
        this(sources, memory, null, null);
    }

    public Manifest(List<WasmSource> sources, ManifestMemory memory, Map<String, String> config, List<String> allowedHosts) {
        this.wasm = sources;
        this.memory = memory;
        this.config = config;
        this.allowedHosts = allowedHosts;
    }

    public void addSource(WasmSource source) {
        this.wasm.add(source);
    }

    public List<WasmSource> getSources() {
        return Collections.unmodifiableList(wasm);
    }

    public ManifestMemory getMemory() {
        return memory;
    }

    public Map<String, String> getConfig() {
        if (config == null || config.isEmpty()) {
            return Collections.emptyMap();
        }
        return Collections.unmodifiableMap(config);
    }

    public List<String> getAllowedHosts() {
        if (allowedHosts == null || allowedHosts.isEmpty()) {
            return Collections.emptyList();
        }
        return Collections.unmodifiableList(allowedHosts);
    }
}
