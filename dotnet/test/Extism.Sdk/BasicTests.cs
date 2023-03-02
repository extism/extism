using System.Diagnostics;
using Extism.Sdk.Native;

using System.Reflection;
using System.Text;

using Xunit;
using Xunit.Abstractions;

namespace Extism.Sdk.Tests;

public class BasicTests
{
    private byte[] count_vowels;
    public BasicTests(ITestOutputHelper output)
    {
        var binDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
        count_vowels = File.ReadAllBytes(Path.Combine(binDirectory, "code.wasm"));
    }
    
    [Fact]
    public void SingleInvocation_InvokesMethod_ReturnsExpectedValue()
    {
        using var context = new Context();
        // Test multiple plugin invocations to ensure that plugin calls can be repeated
        using var plugin = context.CreatePlugin(count_vowels, withWasi: true);
        var response = plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World"));
        Assert.Equal("{\"count\": 3}", Encoding.UTF8.GetString(response));
    }
    
    [Fact]
    public void PluginWithoutWasi_CanBeInvokedWithoutWasi_ReturnsExpectedValue()
    {
        using var context = new Context();
        // Test multiple plugin invocations to ensure that plugin calls can be repeated
        using var plugin = context.CreatePlugin(count_vowels, withWasi: false);
        var response = plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World"));
        Assert.Equal("{\"count\": 3}", Encoding.UTF8.GetString(response));
    }


    [Fact]
    public void MultipleInvocations_InvokesMethod_ReturnsExpectedValues()
    {
        using var context = new Context();
        // Test multiple plugin invocations to ensure that plugin calls can be repeated
        using var plugin = context.CreatePlugin(count_vowels, withWasi: true);
        for (int i = 0; i < 1000; i++)
        {
            var response = plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World"));
            Assert.Equal("{\"count\": 3}", Encoding.UTF8.GetString(response));
        }
    }
    
    [Fact]
    public void InvokingUnknownFunction_DoesntWork_ThrowsException() {
            using var context = new Context();
            // Test multiple plugin invocations to ensure that plugin calls can be repeated
            using var plugin = context.CreatePlugin(count_vowels, withWasi: true);
            var exception = Assert.Throws<ExtismException>(
                () => { plugin.CallFunction("unknown_function_name", Encoding.UTF8.GetBytes("Hello World")); });
            Assert.Equal("Function not found: unknown_function_name", exception.Message);
    }
    // TODO: implement these from Java SDK
    /*
    @Test
    public void shouldInvokeFunctionWithMemoryOptions() {
        //FIXME check whether memory options are effective
        var manifest = new Manifest(List.of(CODE.pathWasmSource()), new MemoryOptions(0));
        var output = Extism.invokeFunction(manifest, "count_vowels", "Hello World");
        assertThat(output).isEqualTo("{\"count\": 3}");
    }

    @Test
    public void shouldInvokeFunctionWithConfig() {
        //FIXME check if config options are available in wasm call
        var config = Map.of("key1", "value1");
        var manifest = new Manifest(List.of(CODE.pathWasmSource()), null, config);
        var output = Extism.invokeFunction(manifest, "count_vowels", "Hello World");
        assertThat(output).isEqualTo("{\"count\": 3}");
    }
    */
    
}