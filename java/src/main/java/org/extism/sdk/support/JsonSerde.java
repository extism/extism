package org.extism.sdk.support;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import org.extism.sdk.manifest.Manifest;

import java.lang.reflect.Type;
import java.util.Base64;

public class JsonSerde {

    private static final Gson GSON;

    static {
        GSON = new GsonBuilder() //
                .disableHtmlEscaping() //
                // needed to convert the byte[] to a base64 encoded String
                .registerTypeHierarchyAdapter(byte[].class, new ByteArrayToBase64TypeAdapter()) //
                .setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES) //
                .setPrettyPrinting() //
                .create();
    }

    public static String toJson(Manifest manifest) {
        return GSON.toJson(manifest);
    }

    private static class ByteArrayToBase64TypeAdapter implements JsonSerializer<byte[]>, JsonDeserializer<byte[]> {

        public byte[] deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
            return Base64.getDecoder().decode(json.getAsString());
        }

        public JsonElement serialize(byte[] src, Type typeOfSrc, JsonSerializationContext context) {
            return new JsonPrimitive(Base64.getEncoder().withoutPadding().encodeToString(src));
        }
    }
}
