package org.extism.sdk;

public class ExtismException extends RuntimeException {

    public ExtismException() {
    }

    public ExtismException(String message) {
        super(message);
    }

    public ExtismException(String message, Throwable cause) {
        super(message, cause);
    }
}
