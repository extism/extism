using Extism.Sdk;
using Extism.Sdk.Native;

using System.Runtime.InteropServices;
using System.Text;

var context = new Context();

var userData = Marshal.StringToHGlobalAnsi("Hello again!");

var helloWorld = new HostFunction(
    "hello_world",
    new[] { ExtismValType.I64 },
    new[] { ExtismValType.I64 },
    userData,
    HelloWorld);

helloWorld.SetNamespace("env");

void HelloWorld(int plugin, ExtismVal[] inputs, ExtismVal[] outputs, nint data)
{
    var text = Marshal.PtrToStringAnsi(data);
    Console.WriteLine(text);
}

var wasm = File.ReadAllBytes("./code-functions.wasm");
using var plugin = context.CreatePlugin(wasm, new[] { helloWorld }, withWasi: true);

var output = Encoding.UTF8.GetString(
    plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World!"))
);

Console.WriteLine($"Output: {output}");

