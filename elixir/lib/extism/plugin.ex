defmodule Extism.Plugin do
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

  def call(plugin, name, input) do
    case Extism.Native.plugin_call(plugin.ctx.ptr, plugin.plugin_id, name, input) do
      {:error, err} -> {:error, err}
      res -> {:ok, res}
    end
  end

   def update(plugin, manifest, wasi) when is_map(manifest) do
     {:ok, manifest_payload} = JSON.encode(manifest)
     case Extism.Native.plugin_update_manifest(plugin.ctx.ptr, plugin.plugin_id, manifest_payload, wasi) do
       {:error, err} -> {:error, err}
       res -> :ok
     end
   end

   def free(plugin) do
     Extism.Native.plugin_free(plugin.ctx.ptr, plugin.plugin_id)
   end

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
