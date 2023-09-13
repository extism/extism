using Extism.Sdk;
using Extism.Sdk.Native;

using System.Runtime.InteropServices;
using System.Text;

Console.WriteLine($"Version: {Plugin.ExtismVersion()}");

var userData = Marshal.StringToHGlobalAnsi("Hello again!");

using var helloWorld = new HostFunction(
    "hello_world",
    "env",
    new[] { ExtismValType.I64 },
    new[] { ExtismValType.I64 },
    userData,
    HelloWorld);

void HelloWorld(CurrentPlugin plugin, Span<ExtismVal> inputs, Span<ExtismVal> outputs, nint data)
{
    Console.WriteLine("Hello from .NET!");

    var text = Marshal.PtrToStringAnsi(data);
    Console.WriteLine(text);

    var input = plugin.ReadString(new nint(inputs[0].v.i64));
    Console.WriteLine($"Input: {input}");

    outputs[0].v.i64 = plugin.WriteString(input);
}

var manifest = new Manifest(new PathWasmSource("./code-functions.wasm"))
{
    Config = new Dictionary<string, string>
    {
        { "my-key", "some cool value" }
    },
};

using var plugin = new Plugin(manifest, new[] { helloWorld }, withWasi: true);

Console.WriteLine("Plugin creatd!!!");

var output = Encoding.UTF8.GetString(
    plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World!"))
);

Console.WriteLine($"Output: {output}");

