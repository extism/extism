import json
import os
from base64 import b64encode
from cffi import FFI
from typing import Union


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

    def plugin(self, manifest: Union[str, bytes, dict], wasi=False, config=None):
        """
        Register a new plugin from a WASM module or JSON encoded manifest

        Parameters
        ----------
        manifest : Union[str, bytes, dict]
            A manifest dictionary describing the plugin or the raw bytes for a module
        wasi : bool
            Set to `True` to enable WASI support
        config : dict
            The plugin config dictionary

        Returns
        -------
        Plugin
            The created plugin
        """
        return Plugin(self, manifest, wasi, config)


class Plugin:
    """
    Plugin is used to call WASM functions.
    Plugins can kept in a context for as long as you need
    or be freed with the `del` keyword.
    """

    def __init__(
        self, context: Context, plugin: Union[str, bytes, dict], wasi=False, config=None
    ):
        """
        Construct a Plugin. Please use Context#plugin instead.
        """

        wasm = _wasm(plugin)

        # Register plugin
        self.plugin = _lib.extism_plugin_new(context.pointer, wasm, len(wasm), wasi)

        self.ctx = context

        if self.plugin < 0:
            error = _lib.extism_error(self.ctx.pointer, -1)
            if error != _ffi.NULL:
                raise Error(_ffi.string(error).decode())
            raise Error("Unable to register plugin")

        if config is not None:
            s = json.dumps(config).encode()
            _lib.extism_plugin_config(self.ctx.pointer, self.plugin, s, len(s))

    def update(self, manifest: Union[str, bytes, dict], wasi=False, config=None):
        """
        Update a plugin with a new WASM module or manifest

        Parameters
        ----------
        plugin : Union[str, bytes, dict]
            A manifest dictionary describing the plugin or the raw bytes for a module
        wasi : bool
            Set to `True` to enable WASI support
        config : dict
            The plugin config dictionary
        """
        wasm = _wasm(manifest)
        ok = _lib.extism_plugin_update(
            self.ctx.pointer, self.plugin, wasm, len(wasm), wasi
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
