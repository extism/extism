package main

import (
	"encoding/json"
	"fmt"
	"os"
	"unsafe"

	"github.com/extism/extism"
)

/*
#include <extism.h>
EXTISM_GO_FUNCTION(hello_world);
*/
import "C"

//export hello_world
func hello_world(plugin *C.ExtismCurrentPlugin, inputs *C.ExtismVal, nInputs C.ExtismSize, outputs *C.ExtismVal, nOutputs C.ExtismSize, userData unsafe.Pointer) {
	fmt.Println("Hello from Go!")
	s := *(*interface{})(userData)
	fmt.Println(s.(string))
	inputSlice := unsafe.Slice(inputs, nInputs)
	outputSlice := unsafe.Slice(outputs, nOutputs)
	outputSlice[0] = inputSlice[0]
}

func main() {
	version := extism.ExtismVersion()
	fmt.Println("Extism Version: ", version)

	ctx := extism.NewContext()
	defer ctx.Free() // this will free the context and all associated plugins

	// set some input data to provide to the plugin module
	var data []byte
	if len(os.Args) > 1 {
		data = []byte(os.Args[1])
	} else {
		data = []byte("testing from go -> wasm shared memory...")
	}
	manifest := extism.Manifest{Wasm: []extism.Wasm{extism.WasmFile{Path: "../wasm/code-functions.wasm"}}}
	f := extism.NewFunction("hello_world", []extism.ValType{extism.I64}, []extism.ValType{extism.I64}, C.hello_world, "Hello again!")
	defer f.Free()
	plugin, err := ctx.PluginFromManifest(manifest, []extism.Function{f}, true)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// use the extism Go library to provide the input data to the plugin, execute it, and then
	// collect the plugin state and error if present
	out, err := plugin.Call("count_vowels", data)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// "out" is []byte type, and the plugin sends back json, so deserialize it into a map.
	// expect this object: `{"count": n}`
	var dest map[string]int
	json.Unmarshal(out, &dest)

	fmt.Println("Count:", dest["count"])
}
