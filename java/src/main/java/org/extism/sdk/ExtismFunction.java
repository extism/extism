package org.extism.sdk;

import com.google.gson.JsonElement;

public interface ExtismFunction{
    void invoke(
            ExtismCurrentPlugin currentPlugin,
            LibExtism.ExtismVal[] params,
            LibExtism.ExtismVal[] returns,
            JsonElement data
    );
}
