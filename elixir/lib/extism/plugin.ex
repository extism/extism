defmodule Extism.Plugin do
  @moduledoc """
  A Plugin represents an instance of your WASM program from the given manifest.
  """
  defstruct [
    # The actual NIF Resource
    plugin: nil,
  ]

  def wrap_resource(plugin) do
    %__MODULE__{
      plugin: plugin
    }
  end

  @doc """
  Creates a new plugin
  """
  def new(manifest, wasi \\ false) do
    {:ok, manifest_payload} = JSON.encode(manifest)

    case Extism.Native.plugin_new_with_manifest(manifest_payload, wasi) do
      {:error, err} -> {:error, err}
      res -> {:ok, Extism.Plugin.wrap_resource(res)}
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
    case Extism.Native.plugin_call(plugin.plugin, name, input) do
      {:error, err} -> {:error, err}
      res -> {:ok, res}
    end
  end

  @doc """
  Frees the plugin
  """
  def free(plugin) do
    Extism.Native.plugin_free(plugin.plugin)
  end

  @doc """
  Returns true if the given plugin responds to the given function name
  """
  def has_function(plugin, function_name) do
    Extism.Native.plugin_has_function(plugin.plugin, function_name)
  end
end

defimpl Inspect, for: Extim.Plugin do
  import Inspect.Algebra

  def inspect(dict, opts) do
    concat(["#Extism.Plugin<", to_doc(dict.plugin, opts), ">"])
  end
end
