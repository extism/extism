using Extism.Sdk;
using Extism.Sdk.Native;
using System.Text;

var context = new Context();

var helloWorld = new HostFunction(
    "hello_world",
    new[] { ExtismValType.I64 },
    new[] { ExtismValType.I64 },
    0,
    HelloWorld);

void HelloWorld(nint plugin, Span<ExtismVal> inputs, Span<ExtismVal> outputs, nint data)
{
    Console.WriteLine("HELLO WORLD!");
}

var wasm = await File.ReadAllBytesAsync("./code-functions.wasm");
using var plugin = context.CreatePlugin(wasm, new[] { helloWorld },  withWasi: true);

var output = Encoding.UTF8.GetString(
    plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World!"))
);

Console.WriteLine(output); // prints {"count": 3}
