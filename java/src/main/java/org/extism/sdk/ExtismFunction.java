package org.extism.sdk;

public interface ExtismFunction<T extends HostUserData> {
    void invoke(
            ExtismCurrentPlugin plugin,
            LibExtism.ExtismVal[] params,
            LibExtism.ExtismVal[] returns,
            T data
    );
}
