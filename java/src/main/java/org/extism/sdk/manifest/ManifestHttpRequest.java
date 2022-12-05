package org.extism.sdk.manifest;

import java.util.Map;

public record ManifestHttpRequest(String url, Map<String, String> header, String method) {
}

