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
  $FREE_PLUGIN = proc { |id|
    x = $PLUGINS[id]
    if !x.nil?
      C.extism_plugin_free(x[:context].pointer, x[:plugin])
      $PLUGINS.delete(id)
    end
  }

  $CONTEXTS = {}
  $FREE_CONTEXT = proc { |id|
    x = $CONTEXTS[id]
    if !x.nil?
      C.extism_context_free($CONTEXTS[id])
      $CONTEXTS.delete(id)
    end
  }

  # A Context is needed to create plugins. The Context
  # is where your plugins live. Freeing the context
  # frees all of the plugins in its scope.
  #
  # @example Create and free a context
  #  ctx = Extism::Context.new
  #  plugin = ctx.plugin(my_manifest)
  #  puts plugin.call("my_func", "my-input")
  #  ctx.free # frees any plugins
  #
  # @example Use with_context to auto-free
  #  Extism.with_context do |ctx|
  #    plugin = ctx.plugin(my_manifest)
  #    puts plugin.call("my_func", "my-input")
  #  end # frees context after exiting this block
  #
  # @attr_reader pointer [FFI::Pointer] Pointer to the Extism context. *Used internally*.
  class Context
    attr_reader :pointer

    # Initialize a new context
    def initialize
      @pointer = C.extism_context_new()
      $CONTEXTS[self.object_id] = @pointer
      ObjectSpace.define_finalizer(self, $FREE_CONTEXT)
    end

    # Remove all registered plugins in this context
    # @return [void]
    def reset
      C.extism_context_reset(@pointer)
    end

    # Free the context, this should be called when it is no longer needed
    # @return [void]
    def free
      return if @pointer.nil?

      $CONTEXTS.delete(self.object_id)
      C.extism_context_free(@pointer)
      @pointer = nil
    end

    # Create a new plugin from a WASM module or JSON encoded manifest
    #
    # @param wasm [Hash, String] The manifest for the plugin. See https://extism.org/docs/concepts/manifest/.
    # @param wasi [Boolean] Enable WASI support
    # @param config [Hash] The plugin config
    # @return [Plugin]
    def plugin(wasm, wasi = false, config = nil)
      Plugin.new(self, wasm, wasi, config)
    end
  end

  # A context manager to create contexts and ensure that they get freed.
  #
  # @example Use with_context to auto-free
  #  Extism.with_context do |ctx|
  #    plugin = ctx.plugin(my_manifest)
  #    puts plugin.call("my_func", "my-input")
  #  end # frees context after exiting this block
  #
  # @yield [ctx] Yields the created Context
  # @return [Object] returns whatever your block returns
  def self.with_context(&block)
    ctx = Context.new
    begin
      x = block.call(ctx)
      return x
    ensure
      ctx.free
    end
  end

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
    # @see Extism::Context#plugin
    # @param context [Context] The context to manager this plugin
    # @param wasm [Hash, String] The manifest or WASM binary. See https://extism.org/docs/concepts/manifest/.
    # @param wasi [Boolean] Enable WASI support
    # @param config [Hash] The plugin config
    def initialize(context, wasm, wasi = false, config = nil)
      @context = context
      if wasm.class == Hash
        wasm = JSON.generate(wasm)
      end
      code = FFI::MemoryPointer.new(:char, wasm.bytesize)
      code.put_bytes(0, wasm)
      @plugin = C.extism_plugin_new(context.pointer, code, wasm.bytesize, nil, 0, wasi)
      if @plugin < 0
        err = C.extism_error(@context.pointer, -1)
        if err&.empty?
          raise Error.new "extism_plugin_new failed"
        else
          raise Error.new err
        end
      end
      $PLUGINS[self.object_id] = { :plugin => @plugin, :context => context }
      ObjectSpace.define_finalizer(self, $FREE_PLUGIN)
      if config != nil and @plugin >= 0
        s = JSON.generate(config)
        ptr = FFI::MemoryPointer::from_string(s)
        C.extism_plugin_config(@context.pointer, @plugin, ptr, s.bytesize)
      end
    end

    # Update a plugin with new WASM module or manifest
    #
    # @param wasm [Hash, String] The manifest or WASM binary. See https://extism.org/docs/concepts/manifest/.
    # @param wasi [Boolean] Enable WASI support
    # @param config [Hash] The plugin config
    # @return [void]
    def update(wasm, wasi = false, config = nil)
      if wasm.class == Hash
        wasm = JSON.generate(wasm)
      end
      code = FFI::MemoryPointer.new(:char, wasm.bytesize)
      code.put_bytes(0, wasm)
      ok = C.extism_plugin_update(@context.pointer, @plugin, code, wasm.bytesize, nil, 0, wasi)
      if !ok
        err = C.extism_error(@context.pointer, @plugin)
        if err&.empty?
          raise Error.new "extism_plugin_update failed"
        else
          raise Error.new err
        end
      end

      if config != nil
        s = JSON.generate(config)
        ptr = FFI::MemoryPointer::from_string(s)
        C.extism_plugin_config(@context.pointer, @plugin, ptr, s.bytesize)
      end
    end

    # Check if a function exists
    #
    # @param name [String] The name of the function
    # @return [Boolean] Returns true if function exists
    def has_function?(name)
      C.extism_plugin_function_exists(@context.pointer, @plugin, name)
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
      rc = C.extism_plugin_call(@context.pointer, @plugin, name, input, data.bytesize)
      if rc != 0
        err = C.extism_error(@context.pointer, @plugin)
        if err&.empty?
          raise Error.new "extism_call failed"
        else
          raise Error.new err
        end
      end
      out_len = C.extism_plugin_output_length(@context.pointer, @plugin)
      buf = C.extism_plugin_output_data(@context.pointer, @plugin)
      block.call(buf, out_len)
    end

    # Free a plugin, this should be called when the plugin is no longer needed
    #
    # @return [void]
    def free
      return if @context.pointer.nil?

      $PLUGINS.delete(self.object_id)
      C.extism_plugin_free(@context.pointer, @plugin)
      @plugin = -1
    end

    # Get a CancelHandle for a plugin
    def cancel_handle
      return CancelHandle.new(C.extism_plugin_cancel_handle(@context.pointer, @plugin))
    end
  end

  private

  # Private module used to interface with the Extism runtime.
  # *Warning*: Do not use or rely on this directly.
  module C
    extend FFI::Library
    ffi_lib "extism"
    attach_function :extism_context_new, [], :pointer
    attach_function :extism_context_free, [:pointer], :void
    attach_function :extism_plugin_new, [:pointer, :pointer, :uint64, :pointer, :uint64, :bool], :int32
    attach_function :extism_plugin_update, [:pointer, :int32, :pointer, :uint64, :pointer, :uint64, :bool], :bool
    attach_function :extism_error, [:pointer, :int32], :string
    attach_function :extism_plugin_call, [:pointer, :int32, :string, :pointer, :uint64], :int32
    attach_function :extism_plugin_function_exists, [:pointer, :int32, :string], :bool
    attach_function :extism_plugin_output_length, [:pointer, :int32], :uint64
    attach_function :extism_plugin_output_data, [:pointer, :int32], :pointer
    attach_function :extism_log_file, [:string, :pointer], :void
    attach_function :extism_plugin_free, [:pointer, :int32], :void
    attach_function :extism_context_reset, [:pointer], :void
    attach_function :extism_version, [], :string
    attach_function :extism_plugin_cancel_handle, [:pointer, :int32], :pointer
    attach_function :extism_plugin_cancel, [:pointer], :bool
  end
end
