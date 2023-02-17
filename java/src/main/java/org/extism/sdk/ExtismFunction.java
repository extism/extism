package org.extism.sdk;

import com.google.gson.JsonElement;

public interface ExtismFunction{
    void invoke(
            ExtismCurrentPlugin currentPlugin,
            LibExtism.ExtismVal.ByReference params,
            LibExtism.ExtismVal.ByReference returns,
            JsonElement data
    );
}
