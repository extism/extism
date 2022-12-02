using Extism.Sdk.Native;
using System.Text;

using Xunit;

namespace Extism.Sdk.Tests;

public class BasicTests
{
    [Fact]
    public async Task CountHelloWorldVowels()
    {
        using var context = new Context();

        var wasm = await File.ReadAllBytesAsync("code.wasm");
        using var plugin = context.CreatePlugin(wasm, withWasi: true);

        var response = plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World"));
        Assert.Equal(0, response);

        var output = Encoding.UTF8.GetString(plugin.OutputData());
        Assert.Equal("{\"count\": 3}", output);
    }
}