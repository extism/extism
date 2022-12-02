package org.extism.sdk.manifest;

import java.util.List;
import java.util.Map;

public class Manifest {
    public List<ManifestWasm> wasm;
    public ManifestMemory memory;
    public List<String> allowedHosts;
    public Map<String, String> config;
}
