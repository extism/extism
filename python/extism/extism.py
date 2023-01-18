import json
import os
from base64 import b64encode
from cffi import FFI
from typing import Union
from enum import Enum


class Error(Exception):
    """Extism error type"""

    pass


_search_dirs = ["/usr/local", "/usr", os.path.join(os.getenv("HOME"), ".local"), "."]


def _check_for_header_and_lib(p):
    def _exists(a, *b):
        return os.path.exists(os.path.join(a, *b))

    if _exists(p, "extism.h"):
        if _exists(p, "libextism.so"):
            return os.path.join(p, "extism.h"), os.path.join(p, "libextism.so")

        if _exists(p, "libextism.dylib"):
            return os.path.join(p, "extism.h"), os.path.join(p, "libextism.dylib")

    if _exists(p, "include", "extism.h"):
        if _exists(p, "lib", "libextism.so"):
            return os.path.join(p, "include", "extism.h"), os.path.join(
                p, "lib", "libextism.so"
            )

        if _exists(p, "lib", "libextism.dylib"):
            return os.path.join(p, "include", "extism.h"), os.path.join(
                p, "lib", "libextism.dylib"
            )


def _locate():
    """Locate extism library and header"""
    script_dir = os.path.dirname(__file__)
    env = os.getenv("EXTISM_PATH")
    if env is not None:
        r = _check_for_header_and_lib(env)
        if r is not None:
            return r

    r = _check_for_header_and_lib(script_dir)
    if r is not None:
        return r

    r = _check_for_header_and_lib(".")
    if r is not None:
        return r

    for d in _search_dirs:
        r = _check_for_header_and_lib(d)
        if r is not None:
            return r

    raise Error("Unable to locate the extism library and header file")


# Initialize the C library
_ffi = FFI()
_header, _lib = _locate()
with open(_header) as f:
    lines = []
    for line in f.readlines():
        if line[0] != "#":
            lines.append(line)
    _ffi.cdef("".join(lines))
_lib = _ffi.dlopen(_lib)


class _Base64Encoder(json.JSONEncoder):
    # pylint: disable=method-hidden
    def default(self, o):
        if isinstance(o, bytes):
            return b64encode(o).decode()
        return json.JSONEncoder.default(self, o)


def set_log_file(file, level=None):
    """
    Sets the log file and level, this is a global configuration

    Parameters
    ----------
    file : str
        The path to the logfile
    level : str
        The debug level, one of ('debug', 'error', 'trace', 'warn')
    """
    level = level or _ffi.NULL
    if isinstance(level, str):
        level = level.encode()
    _lib.extism_log_file(file.encode(), level)


def extism_version():
    """
    Gets the Extism version string

    Returns
    -------
    str
        The Extism runtime version string
    """
    return _ffi.string(_lib.extism_version()).decode()


def _wasm(plugin):
    if isinstance(plugin, str) and os.path.exists(plugin):
        with open(plugin, "rb") as f:
            wasm = f.read()
    elif isinstance(plugin, str):
        wasm = plugin.encode()
    elif isinstance(plugin, dict):
        wasm = json.dumps(plugin, cls=_Base64Encoder).encode()
    else:
        wasm = plugin
    return wasm


class Memory:
    def __init__(self, offs, length):
        self.offset = offs
        self.length = length

    def __len__(self):
        return self.length


class Context:
    """
    Context is used to store and manage plugins. You need a context to create
    or call plugins.  The best way to interact with the Context is
    as a context manager as it can ensure that resources are cleaned up.

    Example
    -------
        with Context() as ctx:
            plugin = ctx.plugin(manifest)
            print(plugin.call("my_function", "some-input"))

    If you need a long lived context, you can use the constructor and the `del` keyword to free.

    Example
    -------
        ctx = Context()
        del ctx
    """

    def __init__(self):
        self.pointer = _lib.extism_context_new()

    def __del__(self):
        _lib.extism_context_free(self.pointer)
        self.pointer = _ffi.NULL

    def __enter__(self):
        return self

    def __exit__(self, type, exc, traceback):
        self.__del__()

    def reset(self):
        """Remove all registered plugins"""
        _lib.extism_context_reset(self.pointer)

    def plugin(
        self, manifest: Union[str, bytes, dict], wasi=False, config=None, functions=None
    ):
        """
        Register a new plugin from a WASM module or JSON encoded manifest

        Parameters
        ----------
        manifest : Union[str, bytes, dict]
            A manifest dictionary describing the plugin or the raw bytes for a module. See [Extism > Concepts > Manifest](https://extism.org/docs/concepts/manifest/).
        wasi : bool
            Set to `True` to enable WASI support
        config : dict
            The plugin config dictionary
        functions: list
            Additional host functions

        Returns
        -------
        Plugin
            The created plugin
        """
        return Plugin(self, manifest, wasi, config, functions)


class Function:
    def __init__(self, name: str, args, returns, f, *user_data):
        self.pointer = None
        args = [a.value for a in args]
        returns = [r.value for r in returns]
        if len(user_data) > 0:
            self.user_data = _ffi.new_handle(user_data)
        else:
            self.user_data = _ffi.NULL
        self.pointer = _lib.extism_function_new(
            name.encode(),
            args,
            len(args),
            returns,
            len(returns),
            f,
            self.user_data,
            _ffi.NULL,
        )

    def __del__(self):
        if not hasattr(self, "pointer"):
            return
        if self.pointer is not None:
            _lib.extism_function_free(self.pointer)


class Plugin:
    """
    Plugin is used to call WASM functions.
    Plugins can kept in a context for as long as you need
    or be freed with the `del` keyword.
    """

    def __init__(
        self,
        context: Context,
        plugin: Union[str, bytes, dict],
        wasi=False,
        config=None,
        functions=None,
    ):
        """
        Construct a Plugin. Please use Context#plugin instead.
        """

        wasm = _wasm(plugin)

        # Register plugin
        if functions is not None:
            functions = [f.pointer for f in functions]
            ptr = _ffi.new("ExtismFunction*[]", functions)
            self.plugin = _lib.extism_plugin_new(
                context.pointer, wasm, len(wasm), ptr, len(functions), wasi
            )
        else:
            self.plugin = _lib.extism_plugin_new(
                context.pointer, wasm, len(wasm), _ffi.NULL, 0, wasi
            )

        self.ctx = context

        if self.plugin < 0:
            error = _lib.extism_error(self.ctx.pointer, -1)
            if error != _ffi.NULL:
                raise Error(_ffi.string(error).decode())
            raise Error("Unable to register plugin")

        if config is not None:
            s = json.dumps(config).encode()
            _lib.extism_plugin_config(self.ctx.pointer, self.plugin, s, len(s))

    def update(
        self, manifest: Union[str, bytes, dict], wasi=False, config=None, functions=None
    ):
        """
        Update a plugin with a new WASM module or manifest

        Parameters
        ----------
        plugin : Union[str, bytes, dict]
            A manifest dictionary describing the plugin or the raw bytes for a module. See [Extism > Concepts > Manifest](https://extism.org/docs/concepts/manifest/).
        wasi : bool
            Set to `True` to enable WASI support
        config : dict
            The plugin config dictionary
        """
        wasm = _wasm(manifest)
        if functions is not None:
            functions = [f.pointer for f in functions]
            ptr = _ffi.new("ExtismFunction*[]", functions)
            ok = _lib.extism_plugin_update(
                self.ctx.pointer,
                self.plugin,
                wasm,
                len(wasm),
                ptr,
                len(functions),
                wasi,
            )
        else:
            ok = _lib.extism_plugin_update(
                self.ctx.pointer, self.plugin, wasm, len(wasm), _ffi.NULL, 0, wasi
            )
        if not ok:
            error = _lib.extism_error(self.ctx.pointer, -1)
            if error != _ffi.NULL:
                raise Error(_ffi.string(error).decode())
            raise Error("Unable to update plugin")

        if config is not None:
            s = json.dumps(config).encode()
            _lib.extism_plugin_config(self.ctx.pointer, self.plugin, s, len(s))

    def _check_error(self, rc):
        if rc != 0:
            error = _lib.extism_error(self.ctx.pointer, self.plugin)
            if error != _ffi.NULL:
                raise Error(_ffi.string(error).decode())
            raise Error(f"Error code: {rc}")

    def function_exists(self, name: str) -> bool:
        """
        Returns true if the given function exists

        Parameters
        ----------
        name : str
            The function name to check for

        Returns
        -------
            True if the function exists in the plugin, False otherwise
        """
        return _lib.extism_plugin_function_exists(
            self.ctx.pointer, self.plugin, name.encode()
        )

    def call(self, function_name: str, data: Union[str, bytes], parse=bytes):
        """
        Call a function by name with the provided input data

        Parameters
        ----------
            name : str
                The name of the function to invoke
            data : Union[str, bytes]
                The input data to the function, can be bytes or a string
            parse : Func
                Can be used to transform the output buffer into
                your expected type. It expects a function that takes a buffer as the
                only argument.

        Return
        ------
            The bytes or parsed data from the plugin function
        """
        if isinstance(data, str):
            data = data.encode()
        self._check_error(
            _lib.extism_plugin_call(
                self.ctx.pointer, self.plugin, function_name.encode(), data, len(data)
            )
        )
        out_len = _lib.extism_plugin_output_length(self.ctx.pointer, self.plugin)
        out_buf = _lib.extism_plugin_output_data(self.ctx.pointer, self.plugin)
        buf = _ffi.buffer(out_buf, out_len)
        if parse is None:
            return buf
        return parse(buf)

    def __del__(self):
        if not hasattr(self, "ctx"):
            return
        if self.ctx.pointer == _ffi.NULL:
            return
        _lib.extism_plugin_free(self.ctx.pointer, self.plugin)
        self.plugin = -1

    def __enter__(self):
        return self

    def __exit__(self, type, exc, traceback):
        self.__del__()


def _convert_value(x):
    if x.t == 0:
        return Val(ValType.I32, x.v.i32)
    elif x.t == 1:
        return Val(ValType.I64, x.v.i64)
    elif x.t == 2:
        return Val(ValType.F32, x.v.f32)
    elif x.y == 3:
        return Val(ValType.F64, x.v.f64)
    return None


def _convert_output(x, v):
    if v.t.value != x.t:
        raise Error(f"Output type mismatch, got {v.t} but expected {x.t}")

    if v.t == ValType.I32:
        x.v.i32 = int(v.value)
    elif v.t == ValType.I64:
        x.v.i64 = int(v.value)
    elif x.t == ValType.F32:
        x.v.f32 = float(v.value)
    elif x.t == ValType.F64:
        x.v.f64 = float(v.value)
    else:
        raise Error("Unsupported return type: " + str(x.t))


class ValType(Enum):
    I32 = 0
    I64 = 1
    F32 = 2
    F64 = 3
    V128 = 4
    FUNC_REF = 5
    EXTERN_REF = 6


class Val:
    """
    Low-level WebAssembly value
    """

    def __init__(self, t: ValType, v):
        self.t = t
        self.value = v

    def __repr__(self):
        return f"Val({self.t}, {self.value})"


class CurrentPlugin:
    """
    Wraps the current plugin from inside a host function
    """

    def __init__(self, p):
        self.pointer = p

    def memory(self, mem: Memory):
        """Access a block of memory"""
        p = _lib.extism_current_plugin_memory(self.pointer)
        if p == 0:
            return None
        return _ffi.buffer(p + mem.offset, mem.length)

    def alloc(self, n):
        """Allocate a new block of memory of [n] bytes"""
        offs = _lib.extism_current_plugin_memory_alloc(self.pointer, n)
        return Memory(offs, n)

    def free(self, mem):
        """Free a block of memory"""
        return _lib.extism_current_plugin_memory_free(self.pointer, mem.offset)

    def memory_at_offset(self, offs):
        """Get a block of memory at the specified offset"""
        if isinstance(offs, Val):
            offs = offs.value
        len = _lib.extism_current_plugin_memory_length(self.pointer, offs)
        return Memory(offs, len)


def host_fn(func):
    """
    A decorator for creating host functions, this decorator wraps a function that takes the following parameters:
    - current plugin: `CurrentPlugin`
    - inputs: `List[Val]`
    - user_data: any number of values passed as user data

    The function should return a list of `Val`
    """

    @_ffi.callback(
        "void(ExtismCurrentPlugin*, const ExtismVal*, ExtismSize, ExtismVal*, ExtismSize, void*)"
    )
    def handle_args(current, inputs, n_inputs, outputs, n_outputs, user_data):
        inp = []
        outp = []

        for i in range(n_inputs):
            inp.append(_convert_value(inputs[i]))

        for i in range(n_outputs):
            outp.append(_convert_value(outputs[i]))

        if user_data == _ffi.NULL:
            output = func(CurrentPlugin(current), inp, outp)
        else:
            udata = _ffi.from_handle(user_data)
            func(CurrentPlugin(current), inp, outp, *udata)

        for i in range(n_outputs):
            _convert_output(outputs[i], outp[i])

    return handle_args
