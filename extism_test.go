package extism

import (
	"encoding/json"
	"fmt"
	"testing"
	"time"
)

func manifest(functions bool) Manifest {
	path := "./wasm/code.wasm"
	if functions {
		path = "./wasm/code-functions.wasm"
	}

	return Manifest{
		Wasm: []Wasm{
			WasmFile{
				Path: path,
			},
		},
	}
}

func expectVowelCount(plugin Plugin, input string, count int) error {
	out, err := plugin.Call("count_vowels", []byte(input))
	if err != nil {
		return err
	}
	var result map[string]int
	json.Unmarshal(out, &result)
	if result["count"] != count {
		return fmt.Errorf("Got count %d but expected %d", result["count"], count)
	}
	return nil
}

func TestCallPlugin(t *testing.T) {
	plugin, err := NewPluginFromManifest(manifest(false), []Function{}, false)
	if err != nil {
		t.Error(err)
	}

	if err := expectVowelCount(plugin, "this is a test", 4); err != nil {
		t.Error(err)
	}
	if err := expectVowelCount(plugin, "this is a test again", 7); err != nil {
		t.Error(err)
	}
	if err := expectVowelCount(plugin, "this is a test thrice", 6); err != nil {
		t.Error(err)
	}
}

func TestFreePlugin(t *testing.T) {
	plugin, err := NewPluginFromManifest(manifest(false), []Function{}, false)
	if err != nil {
		t.Error(err)
	}
	if err := expectVowelCount(plugin, "this is a test", 4); err != nil {
		t.Error(err)
	}

	// free this specific plugin
	plugin.Free()

	if err := expectVowelCount(plugin, "this is a test", 4); err == nil {
		t.Fatal("Expected an error after plugin was freed")
	}
}

func TestFunctionExists(t *testing.T) {
	plugin, err := NewPluginFromManifest(manifest(false), []Function{}, false)
	if err != nil {
		t.Error(err)
	}

	if !plugin.FunctionExists("count_vowels") {
		t.Fatal("Was expecting to find the function count_vowels")
	}
	if plugin.FunctionExists("i_dont_exist") {
		t.Fatal("Was not expecting to find the function i_dont_exist")
	}
}

func TestErrorsOnUnknownFunction(t *testing.T) {
	plugin, err := NewPluginFromManifest(manifest(false), []Function{}, false)
	if err != nil {
		t.Error(err)
	}

	_, err = plugin.Call("i_dont_exist", []byte("someinput"))
	if err == nil {
		t.Fatal("Was expecting call to unknown function to fail")
	}
}

func TestCancel(t *testing.T) {
	manifest := Manifest{
		Wasm: []Wasm{
			WasmFile{
				Path: "./wasm/loop.wasm",
			},
		},
	}

	plugin, err := NewPluginFromManifest(manifest, []Function{}, false)
	if err != nil {
		t.Error(err)
	}

	cancelHandle := plugin.CancelHandle()

	go func(handle CancelHandle) {
		time.Sleep(time.Second * 1)
		handle.Cancel()
	}(cancelHandle)

	_, err = plugin.Call("infinite_loop", []byte(""))
	if err == nil {
		t.Fail()
	}
}
