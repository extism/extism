package org.extism.sdk;

import java.nio.file.Path;

public class Extism {
    public static void setLogger(Path path, LogLevel level) {
        var result = LibExtism.INSTANCE.extism_log_file(path.toString(), level.getLevel());
        if (!result) {
            throw new ExtismException(
                String.format("Could not set extism logger to %s with level %s", path, level)
            );
        }
    }
}
