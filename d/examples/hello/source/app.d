import std.conv: castFrom, to;
import std.file;
import std.functional: toDelegate;
import std.stdio;
import std.string: representation;
import std.typecons : Yes;

import extism;

void main() {
	auto wasm = cast(ubyte[]) read("wasm/code-functions.wasm");
	// FIXME: Creating the plugin results in EXC_BAD_ACCESS (segfault?) from `extism_plugin_new`
	auto plugin = new Plugin(wasm, [
		Function!string(
			"hello_world", /* Inputs */ [ValType.i64], /* Outputs */ [ValType.i64], toDelegate(&helloWorld), "Hello, again!"
		),
	], Yes.withWasi);

	auto input = "aeiou";
	plugin.call("count_vowels", cast(ubyte[]) input.representation);
	writeln(plugin.outputData);
}

///
void helloWorld(CurrentPlugin plugin, const Val[] inputs, Val[] outputs, void *data) {
	writeln("Hello from D!");
	writeln(data);

	ExtismSize ptr_offs = inputs[0].v.i64;
	auto buf = plugin.memory(ptr_offs);
	writeln(buf);
	outputs[0].v.i64 = inputs[0].v.i64;
}
