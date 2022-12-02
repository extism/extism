package org.extism.sdk;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

import org.extism.sdk.manifest.Manifest;
import org.extism.sdk.manifest.ManifestWasm;
import org.extism.sdk.manifest.ManifestWasmData;
import org.extism.sdk.manifest.ManifestWasmUrl;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


public class ExtismTest extends TestCase {
    public ExtismTest(String testName) {
        super(testName);
    }

    public static Test suite() {
        return new TestSuite( ExtismTest.class );
    }

    public void testApp() throws IOException {
        Path resourceDirectory = Paths.get("src", "test", "resources", "code.wasm");
        String absolutePath = resourceDirectory.toFile().getAbsolutePath();
        Context ctx = new Context();
        Manifest manifest = new Manifest();
        manifest.wasm = new ArrayList<ManifestWasm>();
        ManifestWasmUrl wasmData = new ManifestWasmUrl();
        System.out.println(absolutePath);
        wasmData.path = absolutePath;
        manifest.wasm.add(wasmData);
        Plugin plugin = ctx.newPlugin(manifest, false);
        System.out.print(plugin.getIndex());

        byte[] b = plugin.call("count_vowels", "Hello World".getBytes());

        System.out.println(new String(b, StandardCharsets.UTF_8));
    }
}
