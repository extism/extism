package org.extism.sdk.manifest;

import com.google.gson.annotations.SerializedName;
import org.extism.sdk.wasm.WasmSource;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class Manifest {

    @SerializedName("wasm")
    private final List<WasmSource> sources;

    @SerializedName("memory")
    private final MemoryOptions memoryOptions;

    // FIXME remove this and related stuff if not supported in java-sdk
    @SerializedName("allowed_hosts")
    private final List<String> allowedHosts;

    @SerializedName("config")
    private final Map<String, String> config;

    public Manifest() {
        this(new ArrayList<>(), null, null, null);
    }

    public Manifest(WasmSource source) {
        this(List.of(source));
    }

    public Manifest(List<WasmSource> sources) {
        this(sources, null, null, null);
    }

    public Manifest(List<WasmSource> sources, MemoryOptions memoryOptions) {
        this(sources, memoryOptions, null, null);
    }

    public Manifest(List<WasmSource> sources, MemoryOptions memoryOptions, Map<String, String> config) {
        this(sources, memoryOptions, config, null);
    }

    public Manifest(List<WasmSource> sources, MemoryOptions memoryOptions, Map<String, String> config, List<String> allowedHosts) {
        this.sources = sources;
        this.memoryOptions = memoryOptions;
        this.config = config;
        this.allowedHosts = allowedHosts;
    }

    public void addSource(WasmSource source) {
        this.sources.add(source);
    }

    public List<WasmSource> getSources() {
        return Collections.unmodifiableList(sources);
    }

    public MemoryOptions getMemoryOptions() {
        return memoryOptions;
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
