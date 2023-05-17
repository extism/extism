using Extism.Sdk.Native;

using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;

using Xunit;

namespace Extism.Sdk.Tests;

public class BasicTests
{
    [Fact]
    public void CountHelloWorldVowelsWithoutContext()
    {
        var binDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
        var wasm = File.ReadAllBytes(Path.Combine(binDirectory, "code.wasm"));
        using var plugin = Plugin.Create(wasm, Array.Empty<HostFunction>(), withWasi: true);

        var response = plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World"));
        Assert.Equal("{\"count\": 3}", Encoding.UTF8.GetString(response));
    }

    [Fact]
    public void CountHelloWorldVowels()
    {
        using var context = new Context();

        var binDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
        var wasm = File.ReadAllBytes(Path.Combine(binDirectory, "code.wasm"));
        using var plugin = context.CreatePlugin(wasm, Array.Empty<HostFunction>(), withWasi: true);

        var response = plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World"));
        Assert.Equal("{\"count\": 3}", Encoding.UTF8.GetString(response));
    }

    [Fact]
    public void CountVowelsHostFunctions()
    {
        using var context = new Context();

        var userData = Marshal.StringToHGlobalAnsi("Hello again!");

        using var helloWorld = new HostFunction(
            "hello_world",
            "env",
            new[] { ExtismValType.I64 },
            new[] { ExtismValType.I64 },
            userData,
            HelloWorld);

        var binDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
        var wasm = File.ReadAllBytes(Path.Combine(binDirectory, "code-functions.wasm"));
        using var plugin = context.CreatePlugin(wasm, new[] { helloWorld }, withWasi: true);

        var response = plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World"));
        Assert.Equal("{\"count\": 3}", Encoding.UTF8.GetString(response));

        void HelloWorld(CurrentPlugin plugin, Span<ExtismVal> inputs, Span<ExtismVal> outputs, nint data)
        {
            Console.WriteLine("Hello from .NET!");

            var text = Marshal.PtrToStringAnsi(data);
            Console.WriteLine(text);

            var input = plugin.ReadString(new nint(inputs[0].v.i64));
            Console.WriteLine($"Input: {input}");

            var output = new string(input); // clone the string
            outputs[0].v.i64 = plugin.WriteString(output);
        }
    }
}
