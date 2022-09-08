import sys
import json
import os
from base64 import b64encode

from cffi import FFI

from typing import Union


class Error(Exception):
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


class Plugin:

    def __init__(self,
                 plugin: Union[str, bytes, dict],
                 wasi=False,
                 config=None):
        wasm = _wasm(plugin)

        # Register plugin
        self.plugin = lib.extism_plugin_register(wasm, len(wasm), wasi)

        if config is not None:
            s = json.dumps(config).encode()
            lib.extism_plugin_config(s, len(s))

    def update(self, plugin: Union[str, bytes, dict], wasi=False, config=None):
        wasm = _wasm(plugin)
        ok = lib.extism_plugin_update(self.plugin, wasm, len(wasm), wasi)
        if not ok:
            return False

        if config is not None:
            s = json.dumps(config).encode()
            lib.extism_plugin_config(s, len(s))
        return True

    def _check_error(self, rc):
        if rc != 0:
            error = lib.extism_error(self.plugin)
            if error != ffi.NULL:
                raise Error(ffi.string(error).decode())
            raise Error(f"Error code: {rc}")

    def call(self, name: str, data: Union[str, bytes]) -> bytes:
        if isinstance(data, str):
            data = data.encode()
        self._check_error(
            lib.extism_call(self.plugin, name.encode(), data, len(data)))
        out_len = lib.extism_output_length(self.plugin)
        out_buf = ffi.new("uint8_t[]", out_len)
        lib.extism_output_get(self.plugin, out_buf, out_len)
        return ffi.string(out_buf)
