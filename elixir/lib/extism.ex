defmodule Extism do
  def set_log_file(filepath, level) do
    Extism.Native.set_log_file(filepath, level)
  end
end
