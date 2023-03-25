using Extism.Sdk;
using Extism.Sdk.Native;

using System;
using System.Runtime.InteropServices;
using System.Text;

var context = new Context();

var userData = Marshal.StringToHGlobalAnsi("Hello again!");

using var helloWorld = new HostFunction(
    "hello_world",
    new[] { ExtismValType.I64 },
    new[] { ExtismValType.I64 },
    userData,
    HelloWorld);

helloWorld.SetNamespace("env");

void HelloWorld(CurrentPlugin plugin, ExtismVal[] inputs, ExtismVal[] outputs, nint data)
{
    Console.WriteLine("Hello from .NET!");

    var text = Marshal.PtrToStringAnsi(data);
    Console.WriteLine(text);

    var ptr = new nint(inputs[0].v.i64);
    var str = Marshal.PtrToStringAnsi(ptr);

    var mem = plugin.GetMemory();
    var input = Marshal.PtrToStringAnsi(mem + ptr);
    Console.WriteLine(input);

    outputs[0] = inputs[0];
}

var wasm = File.ReadAllBytes("./code-functions.wasm");
using var plugin = context.CreatePlugin(wasm, new[] { helloWorld }, withWasi: true);

var output = Encoding.UTF8.GetString(
    plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World!"))
);

Console.WriteLine($"Output: {output}");

