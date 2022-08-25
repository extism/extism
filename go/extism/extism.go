package extism

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"unsafe"
)

/*
#cgo pkg-config: libextism.pc
#include <extism.h>
*/
import "C"

type Plugin struct {
	id int32
}

type WasmData struct {
	Data []byte `json:"data"`
	Hash string `json:"hash,omitempty"`
	Name string `json:"name,omitempty"`
}

type WasmFile struct {
	Path string `json:"path"`
	Hash string `json:"hash,omitempty"`
	Name string `json:"name,omitempty"`
}

type WasmUrl struct {
	Url    string            `json:"url"`
	Hash   string            `json:"hash,omitempty"`
	Header map[string]string `json:"header,omitempty"`
	Name   string            `json:"name,omitempty"`
	Method string            `json:"method,omitempty"`
}

type Wasm interface{}

type Manifest struct {
	Wasm   []Wasm `json:"wasm"`
	Memory struct {
		Max uint32 `json:"max,omitempty"`
	} `json:"memory,omitempty"`
	Config map[string]string `json:"config,omitempty"`
}

func register(data []byte, wasi bool) (Plugin, error) {
	plugin := C.extism_plugin_register(
		(*C.uchar)(unsafe.Pointer(&data[0])),
		C.uint64_t(len(data)),
		C._Bool(wasi),
	)

	if plugin < 0 {
		return Plugin{id: -1}, errors.New("Unable to load plugin")
	}

	return Plugin{id: int32(plugin)}, nil
}

func LoadManifest(manifest Manifest, wasi bool) (Plugin, error) {
	data, err := json.Marshal(manifest)
	if err != nil {
		return Plugin{id: -1}, err
	}

	return register(data, wasi)
}

func LoadPlugin(module io.Reader, wasi bool) (Plugin, error) {
	wasm, err := io.ReadAll(module)
	if err != nil {
		return Plugin{id: -1}, err
	}

	return register(wasm, wasi)
}

func (plugin Plugin) Call(functionName string, input []byte) ([]byte, error) {
	rc := C.extism_call(
		C.int32_t(plugin.id),
		C.CString(functionName),
		(*C.uchar)(unsafe.Pointer(&input[0])),
		C.uint64_t(len(input)),
	)
	if rc != 0 {
		error := C.extism_error(C.int32_t(plugin.id))
		if error != nil {
			return nil, errors.New(
				fmt.Sprintf("ERROR (extism plugin code: %d): %s", rc, C.GoString(error)),
			)
		}
	}

	length := C.extism_output_length(C.int32_t(plugin.id))
	buf := make([]byte, length)
	C.extism_output_get(C.int32_t(plugin.id), (*C.uchar)(unsafe.Pointer(&buf[0])), length)

	return buf, nil
}
