require 'ffi'
require 'json'

module Extism
  module C
    extend FFI::Library
    ffi_lib "extism"
    attach_function :extism_plugin_register, [:pointer, :uint64, :bool], :int32
    attach_function :extism_plugin_update, [:int32, :pointer, :uint64, :bool], :bool
    attach_function :extism_error, [:int32], :string
    attach_function :extism_call, [:int32, :string, :pointer, :uint64], :int32
    attach_function :extism_output_length, [:int32], :uint64
    attach_function :extism_output_get, [:int32, :pointer, :uint64], :void
    attach_function :extism_log_file, [:string, :pointer], :void
  end


  class Error < StandardError
  end

  def self.set_log_file(name, level=nil)
    if level then
      level = FFI::MemoryPointer::from_string(level)
    end
    C.extism_log_file(name, level)
  end

  class Plugin
    def initialize(wasm, wasi=false, config=nil)
      if wasm.class == Hash then
        wasm = JSON.generate(wasm)
      end
      code = FFI::MemoryPointer.new(:char, wasm.bytesize)
      code.put_bytes(0, wasm)
      @plugin = C.extism_plugin_register(code, wasm.bytesize, wasi)

      if config != nil and @plugin >= 0 then
        s = JSON.generate(config)
        ptr = FFI::MemoryPointer::from_string(s)
        C.extism_plugin_config(@plugin, ptr, s.bytesize)
      end
    end

    def update(wasm, wasi=false, config=nil)
      if wasm.class == Hash then
        wasm = JSON.generate(wasm)
      end
      code = FFI::MemoryPointer.new(:char, wasm.bytesize)
      code.put_bytes(0, wasm)
      ok = C.extism_plugin_update(@plugin, code, wasm.bytesize, wasi)
      if ok then
        if config != nil then
          s = JSON.generate(config)
          ptr = FFI::MemoryPointer::from_string(s)
          C.extism_plugin_config(@plugin, ptr, s.bytesize)
        end
      end
      return ok
    end

    def call(name, data)
      input = FFI::MemoryPointer::from_string(data)
      rc = C.extism_call(@plugin, name, input, data.bytesize)
      if rc != 0 then
        err = C.extism_error(@plugin)
        if err&.empty? then
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
end
