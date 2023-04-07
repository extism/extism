defmodule Extism.Native do
  @moduledoc """
  This module represents the Native Extism runtime API and is for internal use.
  Do not use or rely on this this module.
  """
  use Rustler,
    otp_app: :extism,
    crate: :extism_nif

  def context_new(), do: error()
  def context_reset(_ctx), do: error()
  def context_free(_ctx), do: error()
  def plugin_new_with_manifest(_ctx, _manifest, _wasi), do: error()
  def plugin_call(_ctx, _plugin_id, _name, _input), do: error()
  def plugin_update_manifest(_ctx, _plugin_id, _manifest, _wasi), do: error()
  def plugin_has_function(_ctx, _plugin_id, _function_name), do: error()
  def plugin_free(_ctx, _plugin_id), do: error()
  def set_log_file(_filename, _level), do: error()
  def plugin_cancel_handle(_ctx, _plugin_id), do: error()
  def plugin_cancel(_handle), do: error()

  defp error, do: :erlang.nif_error(:nif_not_loaded)
end
