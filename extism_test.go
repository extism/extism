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

func TestCreateAndFreeContext(t *testing.T) {
	ctx := NewContext()
	ctx.Free()
}

func TestCallPlugin(t *testing.T) {
	ctx := NewContext()
	defer ctx.Free()

	plugin, err := ctx.PluginFromManifest(manifest(false), []Function{}, false)
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
	ctx := NewContext()
	defer ctx.Free()

	plugin, err := ctx.PluginFromManifest(manifest(false), []Function{}, false)
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

func TestContextReset(t *testing.T) {
	ctx := NewContext()
	defer ctx.Free()

	plugin, err := ctx.PluginFromManifest(manifest(false), []Function{}, false)
	if err != nil {
		t.Error(err)
	}
	if err := expectVowelCount(plugin, "this is a test", 4); err != nil {
		t.Error(err)
	}

	// reset the context dropping all plugins
	ctx.Reset()

	if err := expectVowelCount(plugin, "this is a test", 4); err == nil {
		t.Fatal("Expected an error after plugin was freed")
	}
}

func TestCanUpdateAManifest(t *testing.T) {
	ctx := NewContext()
	defer ctx.Free()

	plugin, err := ctx.PluginFromManifest(manifest(false), []Function{}, false)
	if err != nil {
		t.Error(err)
	}

	if err := expectVowelCount(plugin, "this is a test", 4); err != nil {
		t.Error(err)
	}

	plugin.UpdateManifest(manifest(false), []Function{}, false)

	// can still call the plugin
	if err := expectVowelCount(plugin, "this is a test", 4); err != nil {
		t.Error(err)
	}
}

func TestFunctionExists(t *testing.T) {
	ctx := NewContext()
	defer ctx.Free()

	plugin, err := ctx.PluginFromManifest(manifest(false), []Function{}, false)
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
	ctx := NewContext()
	defer ctx.Free()

	plugin, err := ctx.PluginFromManifest(manifest(false), []Function{}, false)
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

	ctx := NewContext()
	defer ctx.Free()

	plugin, err := ctx.PluginFromManifest(manifest, []Function{}, false)
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
