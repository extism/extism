package org.extism.sdk.support;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.extism.sdk.manifest.Manifest;

public class JsonSerializer {

    private static final Gson GSON;

    static {
        GSON = new GsonBuilder().disableHtmlEscaping().setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES).setPrettyPrinting().create();
    }

    public static String toJson(Manifest manifest) {
        return GSON.toJson(manifest);
    }
}
