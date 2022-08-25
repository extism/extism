require 'ffi'
require 'json'

module C
  extend FFI::Library
  ffi_lib "extism"
  attach_function :extism_plugin_register, [:pointer, :uint64, :bool], :int32
  attach_function :extism_error, [:int32], :string
  attach_function :extism_call, [:int32, :string, :pointer, :uint64], :int32
  attach_function :extism_output_length, [:int32], :uint64
  attach_function :extism_output_get, [:int32, :pointer, :uint64], :void
end

class Error < StandardError
end


class Plugin
  def initialize(wasm, wasi=false)
    if wasm.class == Hash then
      wasm = JSON.generate(wasm)
    end
    code = FFI::MemoryPointer.new(:char, wasm.bytesize)
    code.put_bytes(0, wasm) 
    @plugin = C.extism_plugin_register(code, wasm.bytesize, wasi)
  end
  
  def call(name, data)
    input = FFI::MemoryPointer::from_string(data)
    rc = C.extism_call(@plugin, name, input, data.bytesize)
    if rc != 0 then 
      err = C.extism_error(@plugin)
      if err.empty? then
        raise Error.new "extism_call failed"
      else raise Error.new err
      end
    end
    out_len = C.extism_output_length(@plugin)
    buf = FFI::MemoryPointer.new(:char, out_len)
    C.extism_output_get(@plugin, buf, out_len)
    return buf.read_string()
  end
end
