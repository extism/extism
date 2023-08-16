import sys

import json
import hashlib
import pathlib

sys.path.append(".")
from extism import Function, host_fn, ValType, Plugin, set_log_file

set_log_file("stderr", "trace")


@host_fn
def hello_world(plugin, input_, output, a_string):
    print("Hello from Python!")
    print(a_string)
    print(input_)
    print(plugin.input_string(input_[0]))
    output[0] = input_[0]


# Compare against Python implementation.
def count_vowels(data):
    return sum(letter in b"AaEeIiOoUu" for letter in data)


def main(args):
    set_log_file("stderr", "trace")
    if len(args) > 1:
        data = args[1].encode()
    else:
        data = b"a" * 1024

    wasm_file_path = (
        pathlib.Path(__file__).parent.parent / "wasm" / "code-functions.wasm"
    )
    wasm = wasm_file_path.read_bytes()
    hash = hashlib.sha256(wasm).hexdigest()
    manifest = {"wasm": [{"data": wasm, "hash": hash}]}

    functions = [
        Function(
            "hello_world",
            [ValType.I64],
            [ValType.I64],
            hello_world,
            "Hello again!",
        )
    ]
    plugin = Plugin(manifest, wasi=True, functions=functions)
    print(plugin.id)
    # Call `count_vowels`
    wasm_vowel_count = plugin.call("count_vowels", data)
    print(wasm_vowel_count)
    j = json.loads(wasm_vowel_count)

    print("Number of vowels:", j["count"])

    assert j["count"] == count_vowels(data)


if __name__ == "__main__":
    main(sys.argv)
