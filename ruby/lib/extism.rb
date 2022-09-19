require 'ffi'
require 'json'

module Extism
  module C
    extend FFI::Library
    ffi_lib "extism"
    attach_function :extism_context_new, [], :pointer
    attach_function :extism_context_free, [:pointer], :void
    attach_function :extism_plugin_new, [:pointer, :pointer, :uint64, :bool], :int32
    attach_function :extism_plugin_update, [:pointer, :int32, :pointer, :uint64, :bool], :bool
    attach_function :extism_error, [:pointer, :int32], :string
    attach_function :extism_plugin_call, [:pointer, :int32, :string, :pointer, :uint64], :int32
    attach_function :extism_plugin_function_exists, [:pointer, :int32, :string], :bool
    attach_function :extism_plugin_output_length, [:pointer, :int32], :uint64
    attach_function :extism_plugin_output_data, [:pointer, :int32], :pointer
    attach_function :extism_log_file, [:string, :pointer], :void
    attach_function :extism_plugin_free, [:pointer, :int32], :void
    attach_function :extism_context_reset, [:pointer], :void
  end


  class Error < StandardError
  end

  def self.set_log_file(name, level=nil)
    if level then
      level = FFI::MemoryPointer::from_string(level)
    end
    C.extism_log_file(name, level)
  end

  $_plugins = {}
  $_free_plugin = proc { |id|
    if $_plugins.has_value?(id) then
      x = $_plugins[id]
      C.extism_plugin_free(x[:context].pointer, x[:plugin])
      $_plugins.delete(id)
    end
  }
  
  $_contexts = {}
  $_free_context = proc { |id|
    if $_contexts.has_value?(id) then
      C.extism_context_free($_contexts[id])
      $_contexts.delete(id)
    end
  }
  
  class Context
    attr_accessor :pointer
    
    def initialize
      @pointer = C.extism_context_new()
      $_contexts[self.object_id] = @pointer
      ObjectSpace.define_finalizer(self,  $_free_context)
    end
  
    def reset
      C.extism_context_reset(@pointer)
    end
    
    def free
      if @pointer.nil? then
        return
      end
      $_contexts.delete(self.object_id)
      C.extism_context_free(@pointer)
      @pointer = nil
    end
    
    def plugin(wasm, wasi=false, config=nil)
      return Plugin.new(self, wasm, wasi, config)
    end
  end

  class Plugin
    def initialize(context, wasm, wasi=false, config=nil)
      if wasm.class == Hash then
        wasm = JSON.generate(wasm)
      end
      code = FFI::MemoryPointer.new(:char, wasm.bytesize)
      code.put_bytes(0, wasm)
      @plugin = C.extism_plugin_new(context.pointer, code, wasm.bytesize, wasi)
      if @plugin < 0 then
        err = C.extism_error(-1)
        if err&.empty? then
          raise Error.new "extism_plugin_new failed"
        else raise Error.new err
        end
      end
      @context = context
      $_plugins[self.object_id] = {:plugin => @plugin, :context => context}
      ObjectSpace.define_finalizer(self,  $_free_plugin)
      if config != nil and @plugin >= 0 then
        s = JSON.generate(config)
        ptr = FFI::MemoryPointer::from_string(s)
        C.extism_plugin_config(@context.pointer, @plugin, ptr, s.bytesize)
      end
    end

    def update(wasm, wasi=false, config=nil)
      if wasm.class == Hash then
        wasm = JSON.generate(wasm)
      end
      code = FFI::MemoryPointer.new(:char, wasm.bytesize)
      code.put_bytes(0, wasm)
      ok = C.extism_plugin_update(@context.pointer, @plugin, code, wasm.bytesize, wasi)
      if !ok then
        err = C.extism_error(-1)
        if err&.empty? then
          raise Error.new "extism_plugin_update failed"
        else raise Error.new err
        end
      end

      if config != nil then
        s = JSON.generate(config)
        ptr = FFI::MemoryPointer::from_string(s)
        C.extism_plugin_config(@context.pointer, @plugin, ptr, s.bytesize)
      end
    end


    def function_exists(name)
      return C.extism_function_exists(@context.pointer, @plugin, name)
    end

    def call(name, data, &block)
      # If no block was passed then use Pointer::read_string
      block ||= ->(buf, len){ buf.read_string(len) }
      input = FFI::MemoryPointer::from_string(data)
      rc = C.extism_plugin_call(@context.pointer, @plugin, name, input, data.bytesize)
      if rc != 0 then
        err = C.extism_error(@context.pointer, @plugin)
        if err&.empty? then
          raise Error.new "extism_call failed"
        else raise Error.new err
        end
      end
      out_len = C.extism_plugin_output_length(@context.pointer, @plugin)
      buf = C.extism_plugin_output_data(@context.pointer, @plugin)
      return block.call(buf, out_len)
    end

    def free
      if @context.pointer.nil? then
        return
      end

      $_plugins.delete(self.object_id)
      C.extism_plugin_free(@context.pointer, @plugin)
      @plugin = -1
    end

  end
end
