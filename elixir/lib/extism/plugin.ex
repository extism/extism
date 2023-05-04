defmodule Extism.Plugin do
  @moduledoc """
  A Plugin represents an instance of your WASM program from the given manifest.
  """
  defstruct [
    # The actual NIF Resource. PluginIndex and the context
    plugin_id: nil,
    ctx: nil
  ]

  def wrap_resource(ctx, plugin_id) do
    %__MODULE__{
      ctx: ctx,
      plugin_id: plugin_id
    }
  end

  @doc """
  Creates a new plugin
  """
  def new(manifest, wasi \\ false, context \\ nil) do
    ctx = context || Extism.Context.new()
    {:ok, manifest_payload} = JSON.encode(manifest)

    case Extism.Native.plugin_new_with_manifest(ctx.ptr, manifest_payload, wasi) do
      {:error, err} -> {:error, err}
      res -> {:ok, Extism.Plugin.wrap_resource(ctx, res)}
    end
  end

  @doc """
  Call a plugin's function by name

  ## Examples

      iex> {:ok, plugin} = Extism.Plugin.new(manifest, false)
      iex> {:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
         # {:ok, "{\"count\": 4}"}

  ## Parameters

    - plugin: The plugin
    - name: The name of the function as a string
    - input: The input data as a string

  ## Returns

    A string representation of the functions output

  """
  def call(plugin, name, input) do
    case Extism.Native.plugin_call(plugin.ctx.ptr, plugin.plugin_id, name, input) do
      {:error, err} -> {:error, err}
      res -> {:ok, res}
    end
  end

  @doc """
  Updates the manifest of the given plugin

  ## Parameters

    - ctx: The Context to manage this plugin
    - manifest: The String or Map of the WASM module or [manifest](https://extism.org/docs/concepts/manifest)
    - wasi: A bool you set to true if you want WASI support


  """
  def update(plugin, manifest, wasi) when is_map(manifest) do
    {:ok, manifest_payload} = JSON.encode(manifest)

    case Extism.Native.plugin_update_manifest(
           plugin.ctx.ptr,
           plugin.plugin_id,
           manifest_payload,
           wasi
         ) do
      {:error, err} -> {:error, err}
      _ -> :ok
    end
  end

  @doc """
  Frees the plugin
  """
  def free(plugin) do
    Extism.Native.plugin_free(plugin.ctx.ptr, plugin.plugin_id)
  end

  @doc """
  Returns true if the given plugin responds to the given function name
  """
  def has_function(plugin, function_name) do
    Extism.Native.plugin_has_function(plugin.ctx.ptr, plugin.plugin_id, function_name)
  end
end

defimpl Inspect, for: Extim.Plugin do
  import Inspect.Algebra

  def inspect(dict, opts) do
    concat(["#Extism.Plugin<", to_doc(dict.plugin_id, opts), ">"])
  end
end
