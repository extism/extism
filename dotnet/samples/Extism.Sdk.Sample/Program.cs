using Extism.Sdk;
using Extism.Sdk.Native;

using System.Runtime.InteropServices;
using System.Text;

var context = new Context();

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

var wasm = File.ReadAllBytes("./code-functions.wasm");
using var plugin = context.CreatePlugin(wasm, new[] { helloWorld }, withWasi: true);

var output = Encoding.UTF8.GetString(
    plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World!"))
);

Console.WriteLine($"Output: {output}");

