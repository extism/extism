package extism

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"runtime/cgo"
	"unsafe"
)

/*
#cgo CFLAGS: -I/usr/local/include
#cgo LDFLAGS: -L/usr/local/lib -lextism
#include <extism.h>
#include <stdlib.h>

int64_t extism_val_i64(ExtismValUnion* x){
	return x->i64;
}

int32_t extism_val_i32(ExtismValUnion* x){
	return x->i32;
}

float extism_val_f32(ExtismValUnion* x){
	return x->f32;
}

double extism_val_f64(ExtismValUnion* x){
	return x->f64;
}


void extism_val_set_i64(ExtismValUnion* x, int64_t i){
	x->i64 = i;
}


void extism_val_set_i32(ExtismValUnion* x, int32_t i){
	x->i32 = i;
}

void extism_val_set_f32(ExtismValUnion* x, float f){
	x->f32 = f;
}

void extism_val_set_f64(ExtismValUnion* x, double f){
	x->f64 = f;
}

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
	V128      ValType = C.V128
	FuncRef   ValType = C.FuncRef
	ExternRef ValType = C.ExternRef
)

// Function is used to define host functions
type Function struct {
	pointer  *C.ExtismFunction
	userData cgo.Handle
}

// Free a function
func (f *Function) Free() {
	C.extism_function_free(f.pointer)
	f.pointer = nil
	f.userData.Delete()
}

// NewFunction creates a new host function with the given name, input/outputs and optional user data, which can be an
// arbitrary `interface{}`
func NewFunction(name string, inputs []ValType, outputs []ValType, f unsafe.Pointer, userData interface{}) Function {
	var function Function
	function.userData = cgo.NewHandle(userData)
	cname := C.CString(name)
	ptr := unsafe.Pointer(function.userData)
	var inputsPtr *C.ExtismValType = nil
	if len(inputs) > 0 {
		inputsPtr = (*C.ExtismValType)(&inputs[0])
	}
	var outputsPtr *C.ExtismValType = nil
	if len(outputs) > 0 {
		outputsPtr = (*C.ExtismValType)(&outputs[0])
	}
	function.pointer = C.extism_function_new(
		cname,
		inputsPtr,
		C.uint64_t(len(inputs)),
		outputsPtr,
		C.uint64_t(len(outputs)),
		(*[0]byte)(f),
		ptr,
		nil,
	)
	C.free(unsafe.Pointer(cname))
	return function
}

func (f *Function) SetNamespace(s string) {
	cstr := C.CString(s)
	defer C.free(unsafe.Pointer(cstr))
	C.extism_function_set_namespace(f.pointer, cstr)
}

func (f Function) WithNamespace(s string) Function {
	f.SetNamespace(s)
	return f
}

type CurrentPlugin struct {
	pointer *C.ExtismCurrentPlugin
}

func GetCurrentPlugin(ptr unsafe.Pointer) CurrentPlugin {
	return CurrentPlugin{
		pointer: (*C.ExtismCurrentPlugin)(ptr),
	}
}

func (p *CurrentPlugin) Memory(offs uint) []byte {
	length := C.extism_current_plugin_memory_length(p.pointer, C.uint64_t(offs))
	data := unsafe.Pointer(C.extism_current_plugin_memory(p.pointer))
	return unsafe.Slice((*byte)(unsafe.Add(data, offs)), C.int(length))
}

// Alloc a new memory block of the given length, returning its offset
func (p *CurrentPlugin) Alloc(n uint) uint {
	return uint(C.extism_current_plugin_memory_alloc(p.pointer, C.uint64_t(n)))
}

// Free the memory block specified by the given offset
func (p *CurrentPlugin) Free(offs uint) {
	C.extism_current_plugin_memory_free(p.pointer, C.uint64_t(offs))
}

// Length returns the number of bytes allocated at the specified offset
func (p *CurrentPlugin) Length(offs uint) uint {
	return uint(C.extism_current_plugin_memory_length(p.pointer, C.uint64_t(offs)))
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
			nil,
			0,
			C._Bool(wasi))
	} else {
		plugin = C.extism_plugin_new(
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
			nil,
			0,
			C._Bool(wasi),
		))

		if b {
			return nil
		}
	} else {
		b := bool(C.extism_plugin_update(
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

// ValGetI64 returns an I64 from an ExtismVal, it accepts a pointer to a C.ExtismVal
func ValGetI64(v unsafe.Pointer) int64 {
	return int64(C.extism_val_i64(&(*Val)(v).v))
}

// ValGetUInt returns a uint from an ExtismVal, it accepts a pointer to a C.ExtismVal
func ValGetUInt(v unsafe.Pointer) uint {
	return uint(C.extism_val_i64(&(*Val)(v).v))
}

// ValGetI32 returns an int32 from an ExtismVal, it accepts a pointer to a C.ExtismVal
func ValGetI32(v unsafe.Pointer) int32 {
	return int32(C.extism_val_i32(&(*Val)(v).v))
}

// ValGetF32 returns a float32 from an ExtismVal, it accepts a pointer to a C.ExtismVal
func ValGetF32(v unsafe.Pointer) float32 {
	return float32(C.extism_val_f32(&(*Val)(v).v))
}

// ValGetF32 returns a float64 from an ExtismVal, it accepts a pointer to a C.ExtismVal
func ValGetF64(v unsafe.Pointer) float64 {
	return float64(C.extism_val_i64(&(*Val)(v).v))
}

// ValSetI64 stores an int64 in an ExtismVal, it accepts a pointer to a C.ExtismVal and the new value
func ValSetI64(v unsafe.Pointer, i int64) {
	C.extism_val_set_i64(&(*Val)(v).v, C.int64_t(i))
}

// ValSetI32 stores an int32 in an ExtismVal, it accepts a pointer to a C.ExtismVal and the new value
func ValSetI32(v unsafe.Pointer, i int32) {
	C.extism_val_set_i32(&(*Val)(v).v, C.int32_t(i))
}

// ValSetF32 stores a float32 in an ExtismVal, it accepts a pointer to a C.ExtismVal and the new value
func ValSetF32(v unsafe.Pointer, i float32) {
	C.extism_val_set_f32(&(*Val)(v).v, C.float(i))
}

// ValSetF64 stores a float64 in an ExtismVal, it accepts a pointer to a C.ExtismVal and the new value
func ValSetF64(v unsafe.Pointer, f float64) {
	C.extism_val_set_f64(&(*Val)(v).v, C.double(f))
}

func (p *CurrentPlugin) ReturnBytes(v unsafe.Pointer, b []byte) {
	mem := p.Alloc(uint(len(b)))
	ptr := p.Memory(mem)
	copy(ptr, b)
	ValSetI64(v, int64(mem))
}

func (p *CurrentPlugin) ReturnString(v unsafe.Pointer, s string) {
	p.ReturnBytes(v, []byte(s))
}

func (p *CurrentPlugin) InputBytes(v unsafe.Pointer) []byte {
	return p.Memory(ValGetUInt(v))
}

func (p *CurrentPlugin) InputString(v unsafe.Pointer) string {
	return string(p.InputBytes(v))
}

type CancelHandle struct {
	pointer *C.ExtismCancelHandle
}

func (p *Plugin) CancelHandle() CancelHandle {
	pointer := C.extism_plugin_cancel_handle(p.ctx.pointer, C.int(p.id))
	return CancelHandle{pointer}
}

func (c *CancelHandle) Cancel() bool {
	return bool(C.extism_plugin_cancel(c.pointer))
}
