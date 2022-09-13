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
#include <stdlib.h>
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

func makePointer(data []byte) unsafe.Pointer {
	var ptr unsafe.Pointer = nil
	if len(data) > 0 {
		ptr = unsafe.Pointer(&data[0])
	}
	return ptr
}

func SetLogFile(filename string, level string) bool {
	name := C.CString(filename)
	l := C.CString(level)
	r := C.extism_log_file(name, l)
	C.free(unsafe.Pointer(name))
	C.free(unsafe.Pointer(l))
	return bool(r)
}

func register(data []byte, wasi bool) (Plugin, error) {
	ptr := makePointer(data)
	plugin := C.extism_plugin_register(
		(*C.uchar)(ptr),
		C.uint64_t(len(data)),
		C._Bool(wasi),
	)

	if plugin < 0 {
		return Plugin{id: -1}, errors.New("Unable to load plugin")
	}

	return Plugin{id: int32(plugin)}, nil
}

func update(plugin int32, data []byte, wasi bool) bool {
	ptr := makePointer(data)
	return bool(C.extism_plugin_update(
		C.int32_t(plugin),
		(*C.uchar)(ptr),
		C.uint64_t(len(data)),
		C._Bool(wasi),
	))
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

func (p *Plugin) Update(module io.Reader, wasi bool) (bool, error) {
	wasm, err := io.ReadAll(module)
	if err != nil {
		return false, err
	}

	return update(p.id, wasm, wasi), nil
}

func (p *Plugin) UpdateManifest(manifest Manifest, wasi bool) (bool, error) {
	data, err := json.Marshal(manifest)
	if err != nil {
		return false, err
	}

	return update(p.id, data, wasi), nil
}

func (plugin Plugin) SetConfig(data map[string][]byte) error {
	s, err := json.Marshal(data)
	if err != nil {
		return err
	}
	ptr := makePointer(s)
	C.extism_plugin_config(C.int(plugin.id), (*C.uchar)(ptr), C.uint64_t(len(s)))
	return nil
}

func (plugin Plugin) Call(functionName string, input []byte) ([]byte, error) {
	ptr := makePointer(input)
	name := C.CString(functionName)
	rc := C.extism_call(
		C.int32_t(plugin.id),
		name,
		(*C.uchar)(ptr),
		C.uint64_t(len(input)),
	)
	C.free(unsafe.Pointer(name))

	if rc != 0 {
		err := C.extism_error(C.int32_t(plugin.id))
		msg := "<unset by plugin>"
		if err != nil {
			msg = C.GoString(err)
		}

		return nil, errors.New(
			fmt.Sprintf("Plugin error: %s, code: %d", msg, rc),
		)
	}

	length := C.extism_output_length(C.int32_t(plugin.id))

	if length > 0 {
		x := C.extism_output_get(C.int32_t(plugin.id))
		y := (*[]byte)(unsafe.Pointer(&x))
		return []byte((*y)[0:length]), nil

	}

	return []byte{}, nil
}
