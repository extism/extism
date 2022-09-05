defmodule Extism.Context do
  defstruct [
    # The actual NIF Resource. A pointer in this case
    ptr: nil
  ]

  def wrap_resource(ptr) do
    %__MODULE__{
      ptr: ptr
    }
  end

  def new() do
    ptr = Extism.Native.context_new()
    Extism.Context.wrap_resource(ptr)
  end

  def reset(ctx) do
    Extism.Native.context_reset(ctx.ptr)
  end

  def free(ctx) do
    Extism.Native.context_free(ctx.ptr)
  end

  def new_plugin(ctx, manifest, wasi) do
    {:ok, manifest_payload} = JSON.encode(manifest)
    case Extism.Native.plugin_new_with_manifest(ctx.ptr, manifest_payload, wasi) do
      {:error, err} -> {:error, err}
      res -> {:ok, Extism.Plugin.wrap_resource(ctx, res)}
    end
  end
end
