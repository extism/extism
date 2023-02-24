package org.extism.sdk;

import java.util.Optional;

public interface ExtismFunction<T extends HostUserData> {
    void invoke(
            ExtismCurrentPlugin plugin,
            LibExtism.ExtismVal[] params,
            LibExtism.ExtismVal[] returns,
            Optional<T> data
    );
}
