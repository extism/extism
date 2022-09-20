import sys
import os
import json
import hashlib

sys.path.append(".")
from extism import Plugin, Context

if len(sys.argv) > 1:
    data = sys.argv[1].encode()
else:
    data = b"some data from python!"

# a Context provides a scope for plugins to be managed within. creating multiple contexts
# is expected and groups plugins based on source/tenant/lifetime etc.
with Context() as context:
    wasm = open("../wasm/code.wasm", 'rb').read()
    hash = hashlib.sha256(wasm).hexdigest()
    config = {"wasm": [{"data": wasm, "hash": hash}], "memory": {"max": 5}}

    plugin = context.plugin(config)
    # Call `count_vowels`
    j = json.loads(plugin.call("count_vowels", data))
    print("Number of vowels:", j["count"])


# Compare against Python implementation
def count_vowels(data):
    count = 0
    for c in data:
        if c in b'AaEeIiOoUu':
            count += 1
    return count


assert (j["count"] == count_vowels(data))
