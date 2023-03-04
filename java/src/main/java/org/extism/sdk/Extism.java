package org.extism.sdk;

import org.extism.sdk.manifest.Manifest;

import java.nio.file.Path;
import java.util.Objects;

/**
 * Extism convenience functions.
 */
public class Extism {

    /**
     * Configure a log file with the given {@link Path} and configure the given {@link LogLevel}.
     *
     * @param path
     * @param level
     *
     * @deprecated will be replaced with better logging API.
     */
    @Deprecated(forRemoval = true)
    public static void setLogFile(Path path, LogLevel level) {

        Objects.requireNonNull(path, "path");
        Objects.requireNonNull(level, "level");

        var result = LibExtism.INSTANCE.extism_log_file(path.toString(), level.getLevel());
        if (!result) {
            var error = String.format("Could not set extism logger to %s with level %s", path, level);
            throw new ExtismException(error);
        }
    }

    /**
     * Invokes the named {@code function} from the {@link Manifest} with the given {@code input}.
     *
     * @param manifest the manifest containing the function
     * @param function the name of the function to call
     * @param input    the input as string
     * @return the output as string
     * @throws ExtismException if the call fails
     */
    public static String invokeFunction(Manifest manifest, String function, String input) throws ExtismException {
        try (var ctx = new Context()) {
            try (var plugin = ctx.newPlugin(manifest, false, null)) {
                return plugin.call(function, input);
            }
        }
    }

    /**
     * Error levels for the Extism logging facility.
     *
     * @see Extism#setLogFile(Path, LogLevel)
     */
    public enum LogLevel {

        INFO("info"), //

        DEBUG("debug"), //

        WARN("warn"), //

        TRACE("trace");

        private final String level;

        LogLevel(String level) {
            this.level = level;
        }

        public String getLevel() {
            return level;
        }
    }
}
