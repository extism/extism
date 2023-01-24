import sys
import json
import hashlib

sys.path.append(".")
from extism import Context, Function, host_fn, ValType

if len(sys.argv) > 1:
    data = sys.argv[1].encode()
else:
    data = b"some data from python!"


@host_fn
def hello_world(plugin, input, output, context, a_string):
    print("Hello from Python!")
    print(a_string)
    print(input)
    print(plugin.input_string(input[0]))
    output[0] = input[0]


# a Context provides a scope for plugins to be managed within. creating multiple contexts
# is expected and groups plugins based on source/tenant/lifetime etc.
with Context() as context:
    wasm = open("../wasm/code-functions.wasm", "rb").read()
    hash = hashlib.sha256(wasm).hexdigest()
    config = {"wasm": [{"data": wasm, "hash": hash}], "memory": {"max": 5}}

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
    j = json.loads(plugin.call("count_vowels", data))
    print("Number of vowels:", j["count"])


# Compare against Python implementation
def count_vowels(data):
    count = 0
    for c in data:
        if c in b"AaEeIiOoUu":
            count += 1
    return count


assert j["count"] == count_vowels(data)
