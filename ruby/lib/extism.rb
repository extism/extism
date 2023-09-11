require 'ffi'
require 'json'
require_relative './extism/version'

module Extism
  class Error < StandardError
  end

  # Return the version of Extism
  #
  # @return [String] The version string of the Extism runtime
  def self.extism_version
    C.extism_version
  end

  # Set log file and level, this is a global configuration
  # @param name [String] The path to the logfile
  # @param level [String] The log level. One of {"debug", "error", "info", "trace" }
  def self.set_log_file(name, level = nil)
    C.extism_log_file(name, level)
  end

  $PLUGINS = {}
  $FREE_PLUGIN = proc { |ptr|
    x = $PLUGINS[ptr]
    unless x.nil?
      C.extism_plugin_free(x[:plugin])
      $PLUGINS.delete(ptr)
    end
  }

  # A CancelHandle can be used to cancel a running plugin from another thread
  class CancelHandle
    def initialize(handle)
      @handle = handle
    end

    # Cancel the plugin used to generate the handle
    def cancel
      C.extism_plugin_cancel(@handle)
    end
  end

  # A Plugin represents an instance of your WASM program from the given manifest.
  class Plugin
    # Intialize a plugin
    #
    # @param wasm [Hash, String] The manifest or WASM binary. See https://extism.org/docs/concepts/manifest/.
    # @param wasi [Boolean] Enable WASI support
    # @param config [Hash] The plugin config
    def initialize(wasm, functions = [], wasi = false, config = nil)
      wasm = JSON.generate(wasm) if wasm.instance_of?(Hash)
      code = FFI::MemoryPointer.new(:char, wasm.bytesize)
      errmsg = FFI::MemoryPointer.new(:pointer)
      code.put_bytes(0, wasm)
      funcs_ptr = FFI::MemoryPointer.new(C::ExtismFunction)
      funcs_ptr.write_array_of_pointer(functions.map { |f| f.pointer })
      @plugin = C.extism_plugin_new(code, wasm.bytesize, funcs_ptr, functions.length, wasi, errmsg)
      if @plugin.null?
        err = errmsg.read_pointer.read_string
        C.extism_plugin_new_error_free errmsg.read_pointer
        raise Error, err
      end
      $PLUGINS[object_id] = { plugin: @plugin }
      ObjectSpace.define_finalizer(self, $FREE_PLUGIN)
      return unless !config.nil? and @plugin.null?

      s = JSON.generate(config)
      ptr = FFI::MemoryPointer.from_string(s)
      C.extism_plugin_config(@plugin, ptr, s.bytesize)
    end

    # Check if a function exists
    #
    # @param name [String] The name of the function
    # @return [Boolean] Returns true if function exists
    def has_function?(name)
      C.extism_plugin_function_exists(@plugin, name)
    end

    # Call a function by name
    #
    # @param name [String] The function name
    # @param data [String] The input data for the function
    # @return [String] The output from the function in String form
    def call(name, data, &block)
      # If no block was passed then use Pointer::read_string
      block ||= ->(buf, len) { buf.read_string(len) }
      input = FFI::MemoryPointer.from_string(data)
      rc = C.extism_plugin_call(@plugin, name, input, data.bytesize)
      if rc != 0
        err = C.extism_plugin_error(@plugin)
        raise Error, 'extism_call failed' if err&.empty?

        raise Error, err

      end

      out_len = C.extism_plugin_output_length(@plugin)
      buf = C.extism_plugin_output_data(@plugin)
      block.call(buf, out_len)
    end

    # Free a plugin, this should be called when the plugin is no longer needed
    #
    # @return [void]
    def free
      return if @plugin.null?

      $PLUGINS.delete(object_id)
      C.extism_plugin_free(@plugin)
      @plugin = nil
    end

    # Get a CancelHandle for a plugin
    def cancel_handle
      CancelHandle.new(C.extism_plugin_cancel_handle(@plugin))
    end
  end

  Memory = Struct.new(:offset, :len)

  class CurrentPlugin
    def initialize(ptr)
      @ptr = ptr
    end

    def memory_ptr(mem)
      plugin_ptr = C.extism_current_plugin_memory(@ptr)
      FFI::Pointer.new(plugin_ptr.address + mem.offset)
    end

    def alloc(amount)
      offset = C.extism_current_plugin_memory_alloc(@ptr, amount)
      Memory.new(offset, amount)
    end

    def free(memory)
      C.extism_current_plugin_memory_free(@ptr, memory.offset)
    end

    def memory_at_offset(offset)
      len = C.extism_current_plugin_memory_length(@ptr, offset)
      Memory.new(offset, len)
    end

    def input_as_bytes(input)
      # TODO: should assert that this is an int input
      mem = memory_at_offset(input.value)
      # if mem
      #   require 'debug'
      #   binding.break
      # end
      memory_ptr(mem).read_bytes(mem.len)
    end

    def return_bytes(output, bytes)
      mem = alloc(bytes.length)
      memory_ptr(mem).put_bytes(0, bytes)
      output.value = mem.offset
    end

    def return_string(output, string)
      return_bytes(output, string)
    end
  end

  module ValType
    I32 = 0
    I64 = 1
    F32 = 2
    F64 = 3
    V128 = 4
    FUNC_REF = 5
    EXTERN_REF = 6
  end

  class Val
    def initialize(ptr)
      @c_val = C::ExtismVal.new(ptr)
    end

    def type
      case @c_val[:t]
      when :I32
        :i32
      when :I64
        :i64
      when :F32
        :f32
      when :F64
        :f64
      else
        raise "Unsupported wasm value type #{type}"
      end
    end

    def value
      @c_val[:v][type]
    end

    def value=(val)
      @c_val[:v][type] = val
    end
  end

  class Function
    def initialize(name, args, returns, func_proc, user_data)
      @name = name
      @args = args
      @returns = returns
      @func = func_proc
      @user_data = user_data
    end

    def pointer
      return @pointer if @pointer

      free = proc { puts 'freeing ' }
      args = C.from_int_array(@args)
      returns = C.from_int_array(@returns)
      @pointer = C.extism_function_new(@name, args, @args.length, returns, @returns.length, c_func, free, nil)
    end

    private

    def c_func
      @c_func ||= proc do |plugin_ptr, inputs_ptr, inputs_size, outputs_ptr, outputs_size, _data_ptr|
        current_plugin = CurrentPlugin.new(plugin_ptr)
        val_struct_size = C::ExtismVal.size

        inputs = (0...inputs_size).map do |i|
          Val.new(inputs_ptr + i * val_struct_size)
        end
        outputs = (0...outputs_size).map do |i|
          Val.new(outputs_ptr + i * val_struct_size)
        end

        @func.call(current_plugin, inputs, outputs, @user_data)
      end
    end
  end

  # Private module used to interface with the Extism runtime.
  # *Warning*: Do not use or rely on this directly.
  module C
    extend FFI::Library
    ffi_lib 'extism'

    def self.from_int_array(ruby_array)
      ptr = FFI::MemoryPointer.new(:int, ruby_array.length)
      ptr.write_array_of_int(ruby_array)
      ptr
    end

    typedef :uint64, :ExtismMemoryHandle
    typedef :uint64, :ExtismSize

    enum :ExtismValType, %i[I32 I64 F32 F64 V128 FuncRef ExternRef]

    class ExtismValUnion < FFI::Union
      layout :i32, :int32,
             :i64, :int64,
             :f32, :float,
             :f64, :double
    end

    class ExtismVal < FFI::Struct
      layout :t, :ExtismValType,
             :v, ExtismValUnion
    end

    class ExtismFunction < FFI::Struct
      layout :name, :string,
             :inputs, :pointer,
             :n_inputs, :uint64,
             :outputs, :pointer,
             :n_outputs, :uint64,
             :data, :pointer
    end

    callback :ExtismFunctionType, [
      :pointer, # plugin
      :pointer, # inputs
      :ExtismSize, # n_inputs
      :pointer, # outputs
      :ExtismSize, # n_outputs
      :pointer # user_data
    ], :void

    callback :ExtismFreeFunctionType, [], :void

    attach_function :extism_plugin_id, [:pointer], :pointer
    attach_function :extism_current_plugin_memory, [:pointer], :pointer
    attach_function :extism_current_plugin_memory_alloc, %i[pointer ExtismSize], :ExtismMemoryHandle
    attach_function :extism_current_plugin_memory_length, %i[pointer ExtismMemoryHandle], :ExtismSize
    attach_function :extism_current_plugin_memory_free, %i[pointer ExtismMemoryHandle], :void
    attach_function :extism_function_new,
                    %i[string pointer ExtismSize pointer ExtismSize ExtismFunctionType ExtismFreeFunctionType pointer], :pointer
    attach_function :extism_function_free, [:pointer], :void
    attach_function :extism_function_set_namespace, %i[pointer string], :void
    attach_function :extism_plugin_new, %i[pointer ExtismSize pointer ExtismSize bool pointer], :pointer
    attach_function :extism_plugin_new_error_free, [:pointer], :void
    attach_function :extism_plugin_free, [:pointer], :void
    attach_function :extism_plugin_cancel_handle, [:pointer], :pointer
    attach_function :extism_plugin_cancel, [:pointer], :bool
    attach_function :extism_plugin_config, %i[pointer pointer ExtismSize], :bool
    attach_function :extism_plugin_function_exists, %i[pointer string], :bool
    attach_function :extism_plugin_call, %i[pointer string pointer ExtismSize], :int32
    attach_function :extism_error, [:pointer], :string
    attach_function :extism_plugin_error, [:pointer], :string
    attach_function :extism_plugin_output_length, [:pointer], :ExtismSize
    attach_function :extism_plugin_output_data, [:pointer], :pointer
    attach_function :extism_log_file, %i[string string], :bool
    attach_function :extism_version, [], :string
  end
end
