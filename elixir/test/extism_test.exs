defmodule ExtismTest do
  use ExUnit.Case
  doctest Extism

  test "context create & reset" do
    ctx = Extism.Context.new()
    path = Path.join([__DIR__, "../../wasm/code.wasm"])
    manifest = %{wasm: [%{path: path}]}
    {:ok, plugin} = Extism.Context.new_plugin(ctx, manifest, false)
    Extism.Context.reset(ctx)
    # we should expect an error after resetting context
    {:error, _err} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
  end

  defp new_plugin() do
    ctx = Extism.Context.new()
    path = Path.join([__DIR__, "../../wasm/code.wasm"])
    manifest = %{wasm: [%{path: path}]}
    {:ok, plugin} = Extism.Context.new_plugin(ctx, manifest, false)
    {ctx, plugin}
  end

  test "counts vowels" do
    {ctx, plugin} = new_plugin()
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
    assert JSON.decode(output) == {:ok, %{"count" => 4}}
    Extism.Context.free(ctx)
  end

  test "can make multiple calls on a plugin" do
    {ctx, plugin} = new_plugin()
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
    assert JSON.decode(output) == {:ok, %{"count" => 4}}
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test again")
    assert JSON.decode(output) == {:ok, %{"count" => 7}}
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test thrice")
    assert JSON.decode(output) == {:ok, %{"count" => 6}}
    Extism.Context.free(ctx)
  end

  test "can free a plugin" do
    {ctx, plugin} = new_plugin()
    {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
    assert JSON.decode(output) == {:ok, %{"count" => 4}}
    Extism.Plugin.free(plugin)
    # Expect an error when calling a plugin that was freed
    {:error, _err} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
    Extism.Context.free(ctx)
  end

  test "can update manifest" do
    {ctx, plugin} = new_plugin()
    path = Path.join([__DIR__, "../../wasm/code.wasm"])
    manifest = %{wasm: [%{path: path}]}
    assert Extism.Plugin.update(plugin, manifest, true) == :ok
    Extism.Context.free(ctx)
  end

  test "errors on bad manifest" do
    ctx = Extism.Context.new()
    {:error, _msg} = Extism.Context.new_plugin(ctx, %{"wasm" => 123}, false)
    Extism.Context.free(ctx)
  end

  test "errors on unknown function" do
    {ctx, plugin} = new_plugin()
    {:error, _msg} = Extism.Plugin.call(plugin, "unknown", "this is a test")
    Extism.Context.free(ctx)
  end

  test "set_log_file" do
    Extism.set_log_file("/tmp/logfile.log", "debug")
  end

  test "has_function" do
    {ctx, plugin} = new_plugin()
    assert Extism.Plugin.has_function(plugin, "count_vowels")
    assert !Extism.Plugin.has_function(plugin, "unknown")
    Extism.Context.free(ctx)
  end
end
