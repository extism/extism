import sys
import json
import os
from base64 import b64encode

from cffi import FFI

from typing import Union


class Error(Exception):
    '''Extism error type'''
    pass


search_dirs = [
    "/usr/local", "/usr",
    os.path.join(os.getenv("HOME"), ".local"), "."
]


def exists(a, *b):
    return os.path.exists(os.path.join(a, *b))


def check_for_header_and_lib(p):
    if exists(p, "extism.h"):
        if exists(p, "libextism.so"):
            return os.path.join(p, "extism.h"), os.path.join(p, "libextism.so")

        if exists(p, "libextism.dylib"):
            return os.path.join(p, "extism.h"), os.path.join(
                p, "libextism.dylib")

    if exists(p, "include", "extism.h"):
        if exists(p, "lib", "libextism.so"):
            return os.path.join(p, "include", "extism.h"), os.path.join(
                p, "lib", "libextism.so")

        if exists(p, "lib", "libextism.dylib"):
            return os.path.join(p, "include", "extism.h"), os.path.join(
                p, "lib", "libextism.dylib")


def locate():
    '''Locate extism library and header'''
    script_dir = os.path.dirname(__file__)
    env = os.getenv("EXTISM_PATH")
    if env is not None:
        r = check_for_header_and_lib(env)
        if r is not None:
            return r

    r = check_for_header_and_lib(script_dir)
    if r is not None:
        return r

    r = check_for_header_and_lib(".")
    if r is not None:
        return r

    for d in search_dirs:
        r = check_for_header_and_lib(d)
        if r is not None:
            return r

    raise Error("Unable to locate the extism library and header file")


# Initialize the C library
ffi = FFI()
header, lib = locate()
with open(header) as f:
    lines = []
    for line in f.readlines():
        if line[0] != '#':
            lines.append(line)
    ffi.cdef(''.join(lines))
lib = ffi.dlopen(lib)


class Base64Encoder(json.JSONEncoder):
    # pylint: disable=method-hidden
    def default(self, o):
        if isinstance(o, bytes):
            return b64encode(o).decode()
        return json.JSONEncoder.default(self, o)


def set_log_file(file, level=ffi.NULL):
    '''Sets the log file and level, this is a global configuration'''
    if isinstance(level, str):
        level = level.encode()
    lib.extism_log_file(file.encode(), level)


def _wasm(plugin):
    if isinstance(plugin, str) and os.path.exists(plugin):
        with open(plugin, 'rb') as f:
            wasm = f.read()
    elif isinstance(plugin, str):
        wasm = plugin.encode()
    elif isinstance(plugin, dict):
        wasm = json.dumps(plugin, cls=Base64Encoder).encode()
    else:
        wasm = plugin
    return wasm


class Context:
    '''Context is used to store and manage plugins'''

    def __init__(self):
        self.pointer = lib.extism_context_new()

    def __del__(self):
        lib.extism_context_free(self.pointer)
        self.pointer = ffi.NULL

    def __enter__(self):
        return self

    def __exit__(self, type, exc, traceback):
        self.__del__()

    def reset(self):
        '''Remove all registered plugins'''
        lib.extism_context_reset(self.pointer)

    def plugin(self, plugin: Union[str, bytes, dict], wasi=False, config=None):
        '''Register a new plugin from a WASM module or JSON encoded manifest'''
        return Plugin(self, plugin, wasi, config)


class Plugin:
    '''Plugin is used to call WASM functions'''

    def __init__(self,
                 context: Context,
                 plugin: Union[str, bytes, dict],
                 wasi=False,
                 config=None):
        wasm = _wasm(plugin)

        # Register plugin
        self.plugin = lib.extism_plugin_new(context.pointer, wasm, len(wasm),
                                            wasi)

        self.ctx = context

        if self.plugin < 0:
            error = lib.extism_error(self.ctx.pointer, -1)
            if error != ffi.NULL:
                raise Error(ffi.string(error).decode())
            raise Error("Unable to register plugin")

        if config is not None:
            s = json.dumps(config).encode()
            lib.extism_plugin_config(self.ctx.pointer, self.plugin, s, len(s))

    def update(self, plugin: Union[str, bytes, dict], wasi=False, config=None):
        '''Update a plugin with a new WASM module or manifest'''
        wasm = _wasm(plugin)
        ok = lib.extism_plugin_update(self.ctx.pointer, self.plugin, wasm,
                                      len(wasm), wasi)
        if not ok:
            error = lib.extism_error(self.ctx.pointer, -1)
            if error != ffi.NULL:
                raise Error(ffi.string(error).decode())
            raise Error("Unable to update plugin")

        if config is not None:
            s = json.dumps(config).encode()
            lib.extism_plugin_config(self.ctx.pointer, self.plugin, s, len(s))

    def _check_error(self, rc):
        if rc != 0:
            error = lib.extism_error(self.ctx.pointer, self.plugin)
            if error != ffi.NULL:
                raise Error(ffi.string(error).decode())
            raise Error(f"Error code: {rc}")

    def function_exists(self, name: str) -> bool:
        '''Returns true if the given function exists'''
        return lib.extism_plugin_function_exists(self.ctx.pointer, self.plugin,
                                                 name.encode())

    def call(self, name: str, data: Union[str, bytes], parse=bytes):
        '''
        Call a function by name with the provided input data

        The `parse` argument can be used to transform the output buffer into
        your expected type. It expects a function that takes a buffer as the
        only argument
        '''
        if isinstance(data, str):
            data = data.encode()
        self._check_error(
            lib.extism_plugin_call(self.ctx.pointer, self.plugin,
                                   name.encode(), data, len(data)))
        out_len = lib.extism_plugin_output_length(self.ctx.pointer,
                                                  self.plugin)
        out_buf = lib.extism_plugin_output_data(self.ctx.pointer, self.plugin)
        buf = ffi.buffer(out_buf, out_len)
        if parse is None:
            return buf
        return parse(buf)

    def __del__(self):
        if not hasattr(self, 'ctx'):
            return
        if self.ctx.pointer == ffi.NULL:
            return
        lib.extism_plugin_free(self.ctx.pointer, self.plugin)
        self.plugin = -1

    def __enter__(self):
        return self

    def __exit__(self, type, exc, traceback):
        self.__del__()
