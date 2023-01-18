package org.extism.sdk.manifest;

import java.util.Map;

// FIXME remove this and related stuff if not supported in java-sdk
public class ManifestHttpRequest {

    private final String url;
    private final Map<String, String> header;
    private final String method;

    public ManifestHttpRequest(String url, Map<String, String> header, String method) {
        this.url = url;
        this.header = header;
        this.method = method;
    }

    public String url() {
        return url;
    }

    public Map<String, String> header() {
        return header;
    }

    public String method() {
        return method;
    }
}