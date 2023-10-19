defmodule ExtismTest do
  use ExUnit.Case
  doctest Extism

  defp new_plugin() do
    path = Path.join([__DIR__, "../../wasm/code.wasm"])
    manifest = %{wasm: [%{path: path}]}
    {:ok, plugin} = Extism.Plugin.new(manifest, false)
    plugin
  end

  test "counts vowels" do
    plugin = new_plugin()
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
    assert JSON.decode(output) == {:ok, %{"count" => 4}}
  end

  test "can make multiple calls on a plugin" do
    plugin = new_plugin()
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
    assert JSON.decode(output) == {:ok, %{"count" => 4}}
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test again")
    assert JSON.decode(output) == {:ok, %{"count" => 7}}
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test thrice")
    assert JSON.decode(output) == {:ok, %{"count" => 6}}
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "🌎hello🌎world🌎")
    assert JSON.decode(output) == {:ok, %{"count" => 3}}
  end

  test "can free a plugin" do
    plugin = new_plugin()
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
    assert JSON.decode(output) == {:ok, %{"count" => 4}}
    Extism.Plugin.free(plugin)
    # Expect an error when calling a plugin that was freed
    {:error, _err} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
  end

  test "errors on bad manifest" do
    {:error, _msg} = Extism.Plugin.new(%{"wasm" => 123}, false)
  end

  test "errors on unknown function" do
    plugin = new_plugin()
    {:error, _msg} = Extism.Plugin.call(plugin, "unknown", "this is a test")
  end

  test "set_log_file" do
    Extism.set_log_file("/tmp/logfile.log", "debug")
  end

  test "has_function" do
    plugin = new_plugin()
    assert Extism.Plugin.has_function(plugin, "count_vowels")
    assert !Extism.Plugin.has_function(plugin, "unknown")
  end
end
