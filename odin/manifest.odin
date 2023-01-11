package extism

import "core:encoding/json.odin"

WasmData :: struct {
    data []u8   `json:"data"`
    hash string `json:"hash,omitempty"`
    name string `json:"name,omitempty"`
}

WasmFile :: struct {
    path string `json:"path"`
    hast string `json:"hash,omitempty"`
    name string `json:"name,omitempty"`
}

WasmUrl :: struct {
    url     string            `json:"url"`
    hash    string            `json:"hash,omitempty"`
    headers map[string]string `json:"headers,omitempty"`
    name    string            `json:"name,omitempty"`
    method  string            `json:"method,omitempty"`
}

Wasm :: union {
    WasmData,
    WasmFile,
    WasmUrl
}

Memory :: struct {
    maxPages u32 `json:"max_pages,omitempty"`
}

Manifest :: struct {
    wasm         []Wasm            `json:"wasm"`
    memory       Memory            `json:"memory,omitempty"`
    config       map[string]string `json:"config,omitempty"`
    allowedHosts []string          `json:"allowed_hosts,omitempty"`
	allowedPaths map[string]string `json:"allowed_paths,omitempty"`
	timeout      uint              `json:"timeout_ms,omitempty"`
}

newPluginFromManifest :: proc(ctx: Ctx, manifest: Manifest, wasi: bool) -> (Plugin, Error) {
    data, err := json.marshal(manifest)
	if err != .Empty {
		return Plugin{id: -1}, .MarshalManifest
	}

    plg, err := register(ctx.ptr, data, wasi)
    if err != .Empty {
        Plugin{id: -1}, .RegisterManifest
    }

	return plg, .Empty
}

updateManifest :: proc(plg: Plugin, manifest: Manifest, wasi: bool) Error {
	data, err := json.marshal(manifest)
	if err != nil {
		return .MarshalManifest
	}

    err := update(plg.ctx, plg.id, data, wasi)
    if err != .Empty {
        Plugin{id: -1}, .UpdateManifest
    }

	return .Empty
}
