defmodule Extism.Context do
  @moduledoc """
  A Context is needed to create plugins. The Context is where your plugins
  live. Freeing the context frees all of the plugins in its scope.
  """

  defstruct [
    # The actual NIF Resource. A pointer in this case
    ptr: nil
  ]

  def wrap_resource(ptr) do
    %__MODULE__{
      ptr: ptr
    }
  end

  @doc """
  Creates a new context.
  """
  def new() do
    ptr = Extism.Native.context_new()
    Extism.Context.wrap_resource(ptr)
  end

  @doc """
  Resets the context. This has the effect of freeing all the plugins created so far.
  """
  def reset(ctx) do
    Extism.Native.context_reset(ctx.ptr)
  end

  @doc """
  Frees the context from memory and all of its plugins.
  """
  def free(ctx) do
    Extism.Native.context_free(ctx.ptr)
  end

  @doc """
  Create a new plugin from a WASM module or manifest

  ## Examples:

      iex> ctx = Extism.Context.new()
      iex> manifest = %{ wasm: [ %{ path: "/Users/ben/code/extism/wasm/code.wasm" } ]}
      iex> {:ok, plugin} =  Extism.Context.new_plugin(ctx, manifest, false)

  ## Parameters

    - ctx: The Context to manage this plugin
    - manifest: The String or Map of the WASM module or manifest
    - wasi: A bool you set to true if you want WASI support

  """
  def new_plugin(ctx, manifest, wasi \\ false) do
    {:ok, manifest_payload} = JSON.encode(manifest)

    case Extism.Native.plugin_new_with_manifest(ctx.ptr, manifest_payload, wasi) do
      {:error, err} -> {:error, err}
      res -> {:ok, Extism.Plugin.wrap_resource(ctx, res)}
    end
  end
end
