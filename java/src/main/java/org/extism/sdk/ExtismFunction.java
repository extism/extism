package org.extism.sdk;

import com.google.gson.JsonElement;

import java.util.Optional;

public interface ExtismFunction {
    void invoke(
            ExtismCurrentPlugin plugin,
            LibExtism.ExtismVal[] params,
            LibExtism.ExtismVal[] returns,
            Optional<JsonElement> data
    );
}
