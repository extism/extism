package org.extism.sdk.support;

import com.google.gson.*;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;
import org.extism.sdk.manifest.Manifest;

import java.io.IOException;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class JsonSerde {

    private static final Gson GSON;

    static {
        GSON = new GsonBuilder() //
                .disableHtmlEscaping() //
                // needed to convert the byte[] to a base64 encoded String
                .registerTypeHierarchyAdapter(byte[].class, new ByteArrayAdapter()) //
                .setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES) //
                .setPrettyPrinting() //
                .create();
    }

    public static String toJson(Manifest manifest) {
        return GSON.toJson(manifest);
    }

    private static class ByteArrayAdapter extends TypeAdapter<byte[]> {

        @Override
        public void write(JsonWriter out, byte[] byteValue) throws IOException {
            out.value(new String(Base64.getEncoder().encode(byteValue)));
        }

        @Override
        public byte[] read(JsonReader in) {
            try {
                if (in.peek() == JsonToken.NULL) {
                    in.nextNull();
                    return new byte[]{};
                }
                String byteValue = in.nextString();
                if (byteValue != null) {
                    return Base64.getDecoder().decode(byteValue);
                }
                return new byte[]{};
            } catch (Exception e) {
                throw new JsonParseException(e);
            }
        }
    }
}
