using System.Diagnostics;
using Extism.Sdk.Native;

using System.Reflection;
using System.Text;

using Xunit;
using Xunit.Abstractions;

namespace Extism.Sdk.Tests;

public class HostFunctionTests
{
    private byte[] code_functions;
    public HostFunctionTests(ITestOutputHelper output)
    {
        var binDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
        code_functions = File.ReadAllBytes(Path.Combine(binDirectory, "code-functions.wasm"));
    }
    
    [Fact]
    public void InvokeCodeThatDependsOnHostFunction_WithNoHostFunctionDefined_ThrowsException()
    {
        using var context = new Context();
        
        var exception = Assert.Throws<ExtismException>(() => context.CreatePlugin(code_functions, withWasi: true));
        Assert.Equal("unknown import: `env::hello_world` has not been defined", exception.Message);
    }
    
}