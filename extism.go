package extism

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"unsafe"
)

/*
#cgo CFLAGS: -I/usr/local/include
#cgo LDFLAGS: -L/usr/local/lib -lextism
#include <extism.h>
#include <stdlib.h>
*/
import "C"

// Context is used to manage Plugins
type Context struct {
	pointer *C.ExtismContext
}

type ValType = C.ExtismValType

type Val = C.ExtismVal

type Size = C.ExtismSize

var (
	I32       ValType = C.I32
	I64       ValType = C.I64
	F32       ValType = C.F32
	F64       ValType = C.F64
	FuncRef   ValType = C.FuncRef
	ExternRef ValType = C.ExternRef
)

type Function struct {
	pointer  *C.ExtismFunction
	userData interface{}
}

// Free a function
func (f *Function) Free() {
	C.extism_function_free(f.pointer)
	f.pointer = nil
}

func NewFunction(name string, inputs []ValType, outputs []ValType, f unsafe.Pointer, userData interface{}) Function {
	var function Function
	function.userData = userData
	cname := C.CString(name)
	function.pointer = C.extism_function_new(
		cname,
		(*uint32)(&inputs[0]),
		C.uint64_t(len(inputs)),
		(*uint32)(&outputs[0]),
		C.uint64_t(len(outputs)),
		(*[0]byte)(f),
		unsafe.Pointer(&function.userData),
		nil,
	)
	C.free(unsafe.Pointer(cname))
	return function
}

type CurrentPlugin struct {
	pointer *C.ExtismCurrentPlugin
}

func GetCurrentPlugin(ptr *C.ExtismCurrentPlugin) CurrentPlugin {
	return CurrentPlugin{
		pointer: ptr,
	}
}

func (p *CurrentPlugin) Memory(offs uint) []byte {
	length := C.extism_current_plugin_memory_length(p.pointer, C.uint64_t(offs))
	data := unsafe.Pointer(C.extism_current_plugin_memory(p.pointer))
	return unsafe.Slice((*byte)(unsafe.Add(data, offs)), C.int(length))
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
	Url     string            `json:"url"`
	Hash    string            `json:"hash,omitempty"`
	Headers map[string]string `json:"headers,omitempty"`
	Name    string            `json:"name,omitempty"`
	Method  string            `json:"method,omitempty"`
}

type Wasm interface{}

type Manifest struct {
	Wasm   []Wasm `json:"wasm"`
	Memory struct {
		MaxPages uint32 `json:"max_pages,omitempty"`
	} `json:"memory,omitempty"`
	Config       map[string]string `json:"config,omitempty"`
	AllowedHosts []string          `json:"allowed_hosts,omitempty"`
	AllowedPaths map[string]string `json:"allowed_paths,omitempty"`
	Timeout      uint              `json:"timeout_ms,omitempty"`
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

func register(ctx *Context, data []byte, functions []Function, wasi bool) (Plugin, error) {
	ptr := makePointer(data)
	functionPointers := []*C.ExtismFunction{}
	for _, f := range functions {
		functionPointers = append(functionPointers, f.pointer)
	}
	plugin := C.int32_t(-1)

	if len(functions) == 0 {
		plugin = C.extism_plugin_new(
			ctx.pointer,
			(*C.uchar)(ptr),
			C.uint64_t(len(data)),
			C._Bool(wasi))
	} else {
		plugin = C.extism_plugin_new_with_functions(
			ctx.pointer,
			(*C.uchar)(ptr),
			C.uint64_t(len(data)),
			&functionPointers[0],
			C.uint64_t(len(functions)),
			C._Bool(wasi),
		)
	}

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

func update(ctx *Context, plugin int32, data []byte, functions []Function, wasi bool) error {
	ptr := makePointer(data)
	functionPointers := []*C.ExtismFunction{}
	for _, f := range functions {
		functionPointers = append(functionPointers, f.pointer)
	}

	if len(functions) == 0 {
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
	} else {
		b := bool(C.extism_plugin_update_with_functions(
			ctx.pointer,
			C.int32_t(plugin),
			(*C.uchar)(ptr),
			C.uint64_t(len(data)),
			&functionPointers[0],
			C.uint64_t(len(functions)),
			C._Bool(wasi),
		))

		if b {
			return nil
		}
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
func (ctx *Context) PluginFromManifest(manifest Manifest, functions []Function, wasi bool) (Plugin, error) {
	data, err := json.Marshal(manifest)
	if err != nil {
		return Plugin{id: -1}, err
	}

	return register(ctx, data, functions, wasi)
}

// Plugin creates a plugin from a WASM module
func (ctx *Context) Plugin(module io.Reader, functions []Function, wasi bool) (Plugin, error) {
	wasm, err := io.ReadAll(module)
	if err != nil {
		return Plugin{id: -1}, err
	}

	return register(ctx, wasm, functions, wasi)
}

// Update a plugin with a new WASM module
func (p *Plugin) Update(module io.Reader, functions []Function, wasi bool) error {
	wasm, err := io.ReadAll(module)
	if err != nil {
		return err
	}

	return update(p.ctx, p.id, wasm, functions, wasi)
}

// Update a plugin with a new Manifest
func (p *Plugin) UpdateManifest(manifest Manifest, functions []Function, wasi bool) error {
	data, err := json.Marshal(manifest)
	if err != nil {
		return err
	}

	return update(p.ctx, p.id, data, functions, wasi)
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

// FunctionExists returns true when the named function is present in the plugin
func (plugin Plugin) FunctionExists(functionName string) bool {
	name := C.CString(functionName)
	b := C.extism_plugin_function_exists(plugin.ctx.pointer, C.int(plugin.id), name)
	C.free(unsafe.Pointer(name))
	return bool(b)
}

// Call a function by name with the given input, returning the output
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
		return unsafe.Slice((*byte)(x), C.int(length)), nil
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
