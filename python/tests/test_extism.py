import unittest
import extism
import hashlib
import json
from os.path import join, dirname


class TestExtism(unittest.TestCase):
    def test_context_new(self):
        ctx = extism.Context()
        self.assertIsNotNone(ctx)
        del ctx

    def test_call_plugin(self):
        with extism.Context() as ctx:
            plugin = ctx.plugin(self._manifest())
            j = json.loads(plugin.call("count_vowels", "this is a test"))
            self.assertEqual(j["count"], 4)
            j = json.loads(plugin.call("count_vowels", "this is a test again"))
            self.assertEqual(j["count"], 7)
            j = json.loads(plugin.call("count_vowels", "this is a test thrice"))
            self.assertEqual(j["count"], 6)
            j = json.loads(plugin.call("count_vowels", "🌎hello🌎world🌎"))
            self.assertEqual(j["count"], 3)

    def test_update_plugin_manifest(self):
        with extism.Context() as ctx:
            plugin = ctx.plugin(self._manifest())
            # update with just the raw bytes of the wasm
            plugin.update(self._count_vowels_wasm())
            # should still work
            j = json.loads(plugin.call("count_vowels", "this is a test"))
            self.assertEqual(j["count"], 4)

    def test_function_exists(self):
        with extism.Context() as ctx:
            plugin = ctx.plugin(self._manifest())
            self.assertTrue(plugin.function_exists("count_vowels"))
            self.assertFalse(plugin.function_exists("i_dont_exist"))

    def test_errors_on_unknown_function(self):
        with extism.Context() as ctx:
            plugin = ctx.plugin(self._manifest())
            self.assertRaises(
                extism.Error, lambda: plugin.call("i_dont_exist", "someinput")
            )

    def test_can_free_plugin(self):
        with extism.Context() as ctx:
            plugin = ctx.plugin(self._manifest())
            del plugin

    def test_errors_on_bad_manifest(self):
        with extism.Context() as ctx:
            self.assertRaises(
                extism.Error, lambda: ctx.plugin({"invalid_manifest": True})
            )
            plugin = ctx.plugin(self._manifest())
            self.assertRaises(
                extism.Error, lambda: plugin.update({"invalid_manifest": True})
            )

    def test_extism_version(self):
        self.assertIsNotNone(extism.extism_version())

    def _manifest(self):
        wasm = self._count_vowels_wasm()
        hash = hashlib.sha256(wasm).hexdigest()
        return {"wasm": [{"data": wasm, "hash": hash}], "memory": {"max": 5}}

    def _count_vowels_wasm(self):
        path = join(dirname(__file__), "code.wasm")
        with open(path, "rb") as wasm_file:
            return wasm_file.read()
