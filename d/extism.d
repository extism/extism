module extism;

import std.conv : castFrom, to;
import std.meta : Alias;
import std.string : fromStringz, toStringz;

version(Windows) {
    import runtimeWindows;
} else {
    import runtime;
}

/// A list of all possible value types in WebAssembly.
enum ValType {
    i32 = I32,
    i64 = I64,
    f32 = F32,
    f64 = F64,
    v128 = V128,
    funcRef = FuncRef,
    externRef = ExternRef,
}

// Opaque Pointers
///
alias CancelHandle = Alias!(void*);
///
alias CurrentPlugin = Alias!(void*);

///
alias ExtismSize = ulong;

///
union ValUnion {
    int i32;
    long i64;
    float f32;
    double f64;
}

///
struct Val {
    ValType t;
    ValUnion v;
}

/// Used to register host functions with plugins.
/// Params:
///     plugin: the currently executing plugin from within a host function
///     inputs: argument values
///     outputs: return values
///     data: user data associated with the host function
alias FunctionType = void function(
    CurrentPlugin plugin, const Val[] inputs, Val[] outputs, void *data
);

/// Returns: A slice of an allocated memory block of the currently running plugin.
ubyte[] memory(CurrentPlugin plugin, ulong n) {
    auto length = extism_current_plugin_memory_length(cast(ExtismCurrentPlugin*) plugin, n);
    return extism_current_plugin_memory(cast(ExtismCurrentPlugin*) plugin)[n .. length];
}

/// Allocate a memory block in the currently running plugin.
ulong memoryAlloc(CurrentPlugin plugin, ulong n) {
    return extism_current_plugin_memory_alloc(cast(ExtismCurrentPlugin*) plugin, n);
}

/// Free an allocated memory block.
void memoryFree(CurrentPlugin plugin, ulong ptr) {
    extism_current_plugin_memory_free(cast(ExtismCurrentPlugin*) plugin, ptr);
}

///
struct Function {
    private ExtismFunction* func;
    alias func this;
    private string _namespace = null;

    /// Create a new host function.
    /// Params:
    ///     name: function name, this should be valid UTF-8
    ///     inputs: argument types
    ///     outputs: return types
    ///     func: the function to call
    ///     userData: a pointer that will be passed to the function when it's called. This value should live as long as the function exists.
    ///     freeUserData: a callback to release the `userData`` value when the resulting `Function` is freed.
    this(
        string name, const ValType[] inputs, const ValType[] outputs, FunctionType func,
        void* userData = null, void function(void* userData) freeUserData = null
    ) {
        import std.functional : toDelegate;

        // Bind the given host function with C linkage
        auto funcClosure = ((
            ExtismCurrentPlugin* plugin,
            const ExtismVal* inputs, ulong numInputs, ExtismVal* outputs, ulong numOutputs,
            void* data
        ) {
            func(plugin, (cast(const Val*) inputs)[0 .. numInputs], (cast(Val*) outputs)[0 .. numOutputs], data);
        }).toDelegate.bindDelegate;

        this.func = extism_function_new(
            name.toStringz,
            // See https://dlang.org/spec/importc.html#enums
            // See https://forum.dlang.org/post/qmidcpaxctbuphcyvkdc@forum.dlang.org
            castFrom!(const(ValType)*).to!(typeof(ExtismVal.t)*)(inputs.ptr), inputs.length,
            castFrom!(const(ValType)*).to!(typeof(ExtismVal.t)*)(outputs.ptr), outputs.length,
            funcClosure,
            userData,
            freeUserData == null ? null : ((void* userData) {
                freeUserData(userData);
            }).toDelegate.bindDelegate
        );
    }
    ~this() {
        extism_function_free(func);
    }

    /// Get the namespace of this function.
    string namespace() {
        return this._namespace;
    }
    /// Set the namespace of this function.
    void namespace(string value) {
        this._namespace = value;
        extism_function_set_namespace(func, value.toStringz);
    }
}

///
struct Plugin {
    private ExtismPlugin* plugin;
    alias plugin this;

    /// Create a new plugin.
    /// Params:
    ///     wasm: is a WASM module (wat or wasm) or a JSON encoded manifest
    ///     functions: is an array of ExtismFunction*
    ///     withWasi: enables/disables WASI
    this(const ubyte[] wasm, const Function[] functions, bool withWasi) {
        char* errorMsg;
        extism_plugin_new(
            wasm.ptr, wasm.length, cast(ExtismFunction**) functions.ptr, functions.length, withWasi, &errorMsg
        );
    }
    ~this() {
      extism_plugin_free(plugin);
    }

    /// Get the error associated with this `Plugin`.
    string error() {
        return extism_error(plugin).fromStringz.idup;
    }

    /// Update plugin config values, this will merge with the existing values.
    bool config(ubyte[] json) {
        return extism_plugin_config(plugin, json.ptr, json.length);
    }

    /// See_Also: `cancel`
    const(CancelHandle) cancelHandle() {
        return extism_plugin_cancel_handle(plugin);
    }

    /// Cancel a running plugin.
    /// See_Also: `cancelHandle`
    bool cancel(const CancelHandle handle) {
        return extism_plugin_cancel(cast(ExtismCancelHandle*) handle);
    }

    /// Returns: Whether a function with `name` exists.
    bool functionExists(string name) {
        return extism_plugin_function_exists(plugin, name.toStringz);
    }

    /// Call a function.
    /// Params:
    ///     funcName: is the function to call
    ///     data: is the input data
    int call(string funcName, ubyte[] data) {
        return extism_plugin_call(plugin, funcName.toStringz, data.ptr, data.length);
    }

    /// Get the plugin's output data.
    /// Remarks: Use `outputData.length` to retreive size of plugin output.
    ubyte[] outputData() {
        import std.algorithm : copy;

        auto outputLength = extism_plugin_output_length(plugin);
        auto outputData = extism_plugin_output_data(plugin)[0 .. outputLength];
        auto buffer = new ubyte[outputLength];
        assert(
            outputData.copy(buffer).length == 0,
            "Output data was not completely copied into buffer, i.e. buffer was not filled."
        );
        return buffer;
    }
}

/// Get the error associated with a `Context` or `Plugin`, if plugin is -1 then the context error will be returned.

/// Set log file and level.
bool setLogFile(string filename, string logLevel) {
    return extism_log_file(filename.toStringz, logLevel.toStringz);
}

/// Get the Extism version string.
string version_() {
    return extism_version().fromStringz.idup;
}

import std.traits : isDelegate;

/// Transform the given delegate into a static function pointer with C linkage.
/// See_Also: <a href="https://stackoverflow.com/a/22845722/1363247">stackoverflow.com/a/22845722/1363247</a>
package auto bindDelegate(Func)(Func f) if(isDelegate!Func) {
  import std.traits : ParameterTypeTuple, ReturnType;

  static Func delegate_;
  delegate_ = f;
  extern(C) static ReturnType!Func func(ParameterTypeTuple!Func args) {
    return delegate_(args);
  }

  return &func;
}
