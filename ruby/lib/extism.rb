require "ffi"
require "json"
require_relative "./extism/version"

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
    if level
      level = FFI::MemoryPointer::from_string(level)
    end
    C.extism_log_file(name, level)
  end

  $PLUGINS = {}
  $FREE_PLUGIN = proc { |ptr|
    x = $PLUGINS[ptr]
    if !x.nil?
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
      return C.extism_plugin_cancel(@handle)
    end
  end

  # A Plugin represents an instance of your WASM program from the given manifest.
  class Plugin
    # Intialize a plugin
    #
    # @param wasm [Hash, String] The manifest or WASM binary. See https://extism.org/docs/concepts/manifest/.
    # @param wasi [Boolean] Enable WASI support
    # @param config [Hash] The plugin config
    def initialize(wasm, wasi = false, config = nil)
      if wasm.class == Hash
        wasm = JSON.generate(wasm)
      end
      code = FFI::MemoryPointer.new(:char, wasm.bytesize)
      code.put_bytes(0, wasm)
      @plugin = C.extism_plugin_new(code, wasm.bytesize, nil, 0, wasi, nil)
      if @plugin.null?
        # TODO: errmsg
        # err = C.extism_error(@context.pointer, -1)
        # if err&.empty?
        raise Error.new "extism_plugin_new failed"
        # else
        #   raise Error.new err
        # end
      end
      $PLUGINS[self.object_id] = { :plugin => @plugin }
      ObjectSpace.define_finalizer(self, $FREE_PLUGIN)
      if config != nil and @plugin.null?
        s = JSON.generate(config)
        ptr = FFI::MemoryPointer::from_string(s)
        C.extism_plugin_config(@plugin, ptr, s.bytesize)
      end
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
      input = FFI::MemoryPointer::from_string(data)
      rc = C.extism_plugin_call(@plugin, name, input, data.bytesize)
      if rc != 0
        err = C.extism_error(@plugin)
        if err&.empty?
          raise Error.new "extism_call failed"
        else
          raise Error.new err
        end
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

      $PLUGINS.delete(self.object_id)
      C.extism_plugin_free(@plugin)
      @plugin = nil
    end

    # Get a CancelHandle for a plugin
    def cancel_handle
      return CancelHandle.new(C.extism_plugin_cancel_handle(@plugin))
    end
  end

  private

  # Private module used to interface with the Extism runtime.
  # *Warning*: Do not use or rely on this directly.
  module C
    extend FFI::Library
    ffi_lib "extism"
    attach_function :extism_plugin_error_free, [:pointer], :void
    attach_function :extism_plugin_new, [:pointer, :uint64, :pointer, :uint64, :bool, :pointer], :pointer
    attach_function :extism_error, [:pointer], :string
    attach_function :extism_plugin_call, [:pointer, :string, :pointer, :uint64], :int32
    attach_function :extism_plugin_function_exists, [:pointer, :string], :bool
    attach_function :extism_plugin_output_length, [:pointer], :uint64
    attach_function :extism_plugin_output_data, [:pointer], :pointer
    attach_function :extism_log_file, [:string, :pointer], :void
    attach_function :extism_plugin_free, [:pointer], :void
    attach_function :extism_version, [], :string
    attach_function :extism_plugin_cancel_handle, [:pointer], :pointer
    attach_function :extism_plugin_cancel, [:pointer], :bool
  end
end
