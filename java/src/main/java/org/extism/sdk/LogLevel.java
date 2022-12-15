package org.extism.sdk;

public enum LogLevel {
    INFO("info"),
    DEBUG("debug"),
    WARN("warn"),
    TRACE("trace");

    private String level;

    LogLevel(String level) {
        this.level = level;
    }

    public String getLevel() {
        return level;
    }
}