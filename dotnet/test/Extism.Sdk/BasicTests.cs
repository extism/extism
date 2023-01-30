using Extism.Sdk.Native;

using System.Reflection;
using System.Text;

using Xunit;

namespace Extism.Sdk.Tests;

public class BasicTests
{
    private byte[] count_vowels;
    private byte[] sleepMs;
    public BasicTests()
    {
        var binDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
        count_vowels = File.ReadAllBytes(Path.Combine(binDirectory, "code.wasm"));
        sleepMs = File.ReadAllBytes(Path.Combine(binDirectory, "sleepMs.wasm"));
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

    // TODO: Test that calls multiple functions on the same plugin

    [Fact]
    public async void MultipleInvocations_InvokesAsyncMethod_ReturnsExpectedValues()
    {
        using var context = new Context();

        using var plugin = context.CreatePlugin(count_vowels, withWasi: true);
        for (int i = 0; i < 1000; i++)
        {
            var response = await plugin.CallFunctionAsync("count_vowels", Encoding.UTF8.GetBytes("Hello World"));
            Assert.Equal("{\"count\": 3}", Encoding.UTF8.GetString(response));
        }
    }

    // TODO: write tests with long running functions and also test pre-cancelling the task

    [Fact]
    public async void PrecancelledToken_DoesntInvoke_Throws()
    {
        using var context = new Context();
        using var plugin = context.CreatePlugin(count_vowels, withWasi: true);

        CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
        var token = cancellationTokenSource.Token;
        cancellationTokenSource.Cancel();
        await Assert.ThrowsAsync<TaskCanceledException>(async () =>
        {
            await plugin.CallFunctionAsync("count_vowels", Encoding.UTF8.GetBytes("Hello World"), null, token);
        });
    }

    [Fact]
    public async void LongRunningTask_DoesntComplete_IsCanceled()
    {
        using var context = new Context();
        using var plugin = context.CreatePlugin(sleepMs, withWasi: true);

        await Assert.ThrowsAsync<TaskCanceledException>(async () =>
        {
            // This should throw after ~2500 ms and not complete execution.
            var response = await plugin.CallFunctionAsync("sleepMs", Encoding.UTF8.GetBytes("[5000]"), 1500);
            Assert.Fail("This should be unreachable.");
        });
    }
}