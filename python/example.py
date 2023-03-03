import sys

import json
import hashlib
import pathlib

sys.path.append(".")
from extism import Context, Function, host_fn, ValType


@host_fn
def hello_world(plugin, input_, output, context, a_string):
    print("Hello from Python!")
    print(a_string)
    print(input_)
    print(plugin.input_string(input_[0]))
    output[0] = input_[0]


# Compare against Python implementation.
def count_vowels(data):
    return sum(letter in b"AaEeIiOoUu" for letter in data)


def main(args):
    if len(args) > 1:
        data = args[1].encode()
    else:
        data = b"some data from python!"

    wasm_file_path = (
        pathlib.Path(__file__).parent.parent / "wasm" / "code-functions.wasm"
    )
    wasm = wasm_file_path.read_bytes()
    hash = hashlib.sha256(wasm).hexdigest()
    config = {"wasm": [{"data": wasm, "hash": hash}], "memory": {"max": 5}}

    # a Context provides a scope for plugins to be managed within. creating multiple contexts
    # is expected and groups plugins based on source/tenant/lifetime etc.
    with Context() as context:
        functions = [
            Function(
                "hello_world",
                [ValType.I64],
                [ValType.I64],
                hello_world,
                context,
                "Hello again!",
            )
        ]
        plugin = context.plugin(config, wasi=True, functions=functions)
        # Call `count_vowels`
        wasm_vowel_count = json.loads(plugin.call("count_vowels", data))

    print("Number of vowels:", wasm_vowel_count["count"])

    assert wasm_vowel_count["count"] == count_vowels(data)


if __name__ == "__main__":
    main(sys.argv)
