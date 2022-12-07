package org.extism.sdk;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class ContextTests {

    @Test
    public void shouldReturnVersionString() {
        try (var ctx = new Context()) {
            var version = ctx.getVersion();
            assertThat(version).isNotNull();
        }
    }

    @Test
    public void shouldAllowResetOnEmptyContext() {
        try (var ctx = new Context()) {
            ctx.reset();
        }
    }
}
