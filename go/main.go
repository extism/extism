package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/extism/extism"
)

func main() {
	ctx := extism.NewContext()
	defer ctx.Free()

	// set some input data to provide to the plugin module
	var data []byte
	if len(os.Args) > 1 {
		data = []byte(os.Args[1])
	} else {
		data = []byte("testing from go -> wasm shared memory...")
	}

	manifest := extism.Manifest{Wasm: []extism.Wasm{extism.WasmFile{Path: "../wasm/code.wasm"}}}
	plugin, err := ctx.PluginFromManifest(manifest, false)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	defer plugin.Free()

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
