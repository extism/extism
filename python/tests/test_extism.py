import unittest
import extism
import hashlib
import json
import time
from threading import Thread
from datetime import datetime, timedelta
from os.path import join, dirname


class TestExtism(unittest.TestCase):
    def test_call_plugin(self):
        plugin = extism.Plugin(self._manifest())
        j = json.loads(plugin.call("count_vowels", "this is a test"))
        self.assertEqual(j["count"], 4)
        j = json.loads(plugin.call("count_vowels", "this is a test again"))
        self.assertEqual(j["count"], 7)
        j = json.loads(plugin.call("count_vowels", "this is a test thrice"))
        self.assertEqual(j["count"], 6)
        j = json.loads(plugin.call("count_vowels", "🌎hello🌎world🌎"))
        self.assertEqual(j["count"], 3)

    def test_function_exists(self):
        plugin = extism.Plugin(self._manifest())
        self.assertTrue(plugin.function_exists("count_vowels"))
        self.assertFalse(plugin.function_exists("i_dont_exist"))

    def test_errors_on_unknown_function(self):
        plugin = extism.Plugin(self._manifest())
        self.assertRaises(
            extism.Error, lambda: plugin.call("i_dont_exist", "someinput")
        )

    def test_can_free_plugin(self):
        plugin = extism.Plugin(self._manifest())
        del plugin

    def test_errors_on_bad_manifest(self):
        self.assertRaises(
            extism.Error, lambda: extism.Plugin({"invalid_manifest": True})
        )

    def test_extism_version(self):
        self.assertIsNotNone(extism.extism_version())

    def test_extism_plugin_timeout(self):
        plugin = extism.Plugin(self._loop_manifest())
        start = datetime.now()
        self.assertRaises(extism.Error, lambda: plugin.call("infinite_loop", b""))
        end = datetime.now()
        self.assertLess(
            end,
            start + timedelta(seconds=1.01),
            "plugin timeout exceeded 1000ms expectation",
        )

    def test_extism_host_function(self):
        @extism.host_fn
        def hello_world(plugin, params, results, user_data):
            offs = plugin.alloc(len(user_data))
            mem = plugin.memory(offs)
            mem[:] = user_data
            results[0].value = offs.offset

        f = [
            extism.Function(
                "hello_world",
                [extism.ValType.I64],
                [extism.ValType.I64],
                hello_world,
                b"test",
            )
        ]
        plugin = extism.Plugin(self._manifest(functions=True), functions=f, wasi=True)
        res = plugin.call("count_vowels", "aaa")
        self.assertEqual(res, b"test")

    def test_extism_plugin_cancel(self):
        plugin = extism.Plugin(self._loop_manifest())
        cancel_handle = plugin.cancel_handle()

        def cancel(handle):
            time.sleep(0.5)
            handle.cancel()

        Thread(target=cancel, args=[cancel_handle]).run()
        self.assertRaises(extism.Error, lambda: plugin.call("infinite_loop", b""))

    def _manifest(self, functions=False):
        wasm = self._count_vowels_wasm(functions)
        hash = hashlib.sha256(wasm).hexdigest()
        return {"wasm": [{"data": wasm, "hash": hash}]}

    def _loop_manifest(self):
        wasm = self._infinite_loop_wasm()
        hash = hashlib.sha256(wasm).hexdigest()
        return {
            "wasm": [{"data": wasm, "hash": hash}],
            "timeout_ms": 1000,
        }

    def _count_vowels_wasm(self, functions=False):
        return read_test_wasm("code.wasm" if not functions else "code-functions.wasm")

    def _infinite_loop_wasm(self):
        return read_test_wasm("loop.wasm")


def read_test_wasm(p):
    path = join(dirname(__file__), "..", "..", "wasm", p)
    with open(path, "rb") as wasm_file:
        return wasm_file.read()
