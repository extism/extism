package org.extism.sdk.manifest;

import java.util.Map;

// FIXME remove this and related stuff if not supported in java-sdk
public record ManifestHttpRequest(String url, Map<String, String> header, String method) {
}

