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

// Context is used to manage Plugins
type Context struct {
	pointer *C.ExtismContext
}

// NewContext creates a new context, it should be freed using the `Free` method
func NewContext() Context {
	p := C.extism_context_new()
	return Context{
		pointer: p,
	}
}

// Free a context
func (ctx *Context) Free() {
	C.extism_context_free(ctx.pointer)
	ctx.pointer = nil
}

// Plugin is used to call WASM functions
type Plugin struct {
	ctx *Context
	id  int32
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
	Config       map[string]string `json:"config,omitempty"`
	AllowedHosts []string          `json:"allowed_hosts,omitempty"`
}

func makePointer(data []byte) unsafe.Pointer {
	var ptr unsafe.Pointer = nil
	if len(data) > 0 {
		ptr = unsafe.Pointer(&data[0])
	}
	return ptr
}

// SetLogFile sets the log file and level, this is a global setting
func SetLogFile(filename string, level string) bool {
	name := C.CString(filename)
	l := C.CString(level)
	r := C.extism_log_file(name, l)
	C.free(unsafe.Pointer(name))
	C.free(unsafe.Pointer(l))
	return bool(r)
}

// ExtismVersion gets the Extism version string
func ExtismVersion() string {
	return C.GoString(C.extism_version())
}

func register(ctx *Context, data []byte, wasi bool) (Plugin, error) {
	ptr := makePointer(data)
	plugin := C.extism_plugin_new(
		ctx.pointer,
		(*C.uchar)(ptr),
		C.uint64_t(len(data)),
		C._Bool(wasi),
	)

	if plugin < 0 {
		err := C.extism_error(ctx.pointer, C.int32_t(-1))
		msg := "Unknown"
		if err != nil {
			msg = C.GoString(err)
		}

		return Plugin{id: -1}, errors.New(
			fmt.Sprintf("Unable to load plugin: %s", msg),
		)
	}

	return Plugin{id: int32(plugin), ctx: ctx}, nil
}

func update(ctx *Context, plugin int32, data []byte, wasi bool) error {
	ptr := makePointer(data)
	b := bool(C.extism_plugin_update(
		ctx.pointer,
		C.int32_t(plugin),
		(*C.uchar)(ptr),
		C.uint64_t(len(data)),
		C._Bool(wasi),
	))

	if b {
		return nil
	}

	err := C.extism_error(ctx.pointer, C.int32_t(-1))
	msg := "Unknown"
	if err != nil {
		msg = C.GoString(err)
	}

	return errors.New(
		fmt.Sprintf("Unable to load plugin: %s", msg),
	)
}

// PluginFromManifest creates a plugin from a `Manifest`
func (ctx *Context) PluginFromManifest(manifest Manifest, wasi bool) (Plugin, error) {
	data, err := json.Marshal(manifest)
	if err != nil {
		return Plugin{id: -1}, err
	}

	return register(ctx, data, wasi)
}

// Plugin creates a plugin from a WASM module
func (ctx *Context) Plugin(module io.Reader, wasi bool) (Plugin, error) {
	wasm, err := io.ReadAll(module)
	if err != nil {
		return Plugin{id: -1}, err
	}

	return register(ctx, wasm, wasi)
}

// Update a plugin with a new WASM module
func (p *Plugin) Update(module io.Reader, wasi bool) error {
	wasm, err := io.ReadAll(module)
	if err != nil {
		return err
	}

	return update(p.ctx, p.id, wasm, wasi)
}

// Update a plugin with a new Manifest
func (p *Plugin) UpdateManifest(manifest Manifest, wasi bool) error {
	data, err := json.Marshal(manifest)
	if err != nil {
		return err
	}

	return update(p.ctx, p.id, data, wasi)
}

// Set configuration values
func (plugin Plugin) SetConfig(data map[string][]byte) error {
	s, err := json.Marshal(data)
	if err != nil {
		return err
	}
	ptr := makePointer(s)
	C.extism_plugin_config(plugin.ctx.pointer, C.int(plugin.id), (*C.uchar)(ptr), C.uint64_t(len(s)))
	return nil
}

/// FunctionExists returns true when the name function is present in the plugin
func (plugin Plugin) FunctionExists(functionName string) bool {
	name := C.CString(functionName)
	b := C.extism_plugin_function_exists(plugin.ctx.pointer, C.int(plugin.id), name)
	C.free(unsafe.Pointer(name))
	return bool(b)
}

/// Call a function by name with the given input, returning the output
func (plugin Plugin) Call(functionName string, input []byte) ([]byte, error) {
	ptr := makePointer(input)
	name := C.CString(functionName)
	rc := C.extism_plugin_call(
		plugin.ctx.pointer,
		C.int32_t(plugin.id),
		name,
		(*C.uchar)(ptr),
		C.uint64_t(len(input)),
	)
	C.free(unsafe.Pointer(name))

	if rc != 0 {
		err := C.extism_error(plugin.ctx.pointer, C.int32_t(plugin.id))
		msg := "<unset by plugin>"
		if err != nil {
			msg = C.GoString(err)
		}

		return nil, errors.New(
			fmt.Sprintf("Plugin error: %s, code: %d", msg, rc),
		)
	}

	length := C.extism_plugin_output_length(plugin.ctx.pointer, C.int32_t(plugin.id))

	if length > 0 {
		x := C.extism_plugin_output_data(plugin.ctx.pointer, C.int32_t(plugin.id))
		y := (*[]byte)(unsafe.Pointer(&x))
		return []byte((*y)[0:length]), nil
	}

	return []byte{}, nil
}

// Free a plugin
func (plugin *Plugin) Free() {
	if plugin.ctx.pointer == nil {
		return
	}
	C.extism_plugin_free(plugin.ctx.pointer, C.int32_t(plugin.id))
	plugin.id = -1
}

// Reset removes all registered plugins in a Context
func (ctx Context) Reset() {
	C.extism_context_reset(ctx.pointer)
}
