package org.extism.sdk.manifest;

import java.util.List;
import java.util.Map;
import com.google.gson.GsonBuilder;
import com.google.gson.Gson;
import com.google.gson.FieldNamingPolicy;

public class Manifest {
    public List<ManifestWasm> wasm;
    public ManifestMemory memory;
    public List<String> allowedHosts;
    public Map<String, String> config;

    private static Gson _gson;
    static {
        Manifest._gson = new GsonBuilder()
            .disableHtmlEscaping()
            .setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES)
            .setPrettyPrinting()
            .create();
    }

    public String toJson() {
        return _gson.toJson(this);
    }
}
