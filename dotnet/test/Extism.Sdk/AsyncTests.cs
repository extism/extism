using System.Diagnostics;
using Extism.Sdk.Native;

using System.Reflection;
using System.Text;

using Xunit;
using Xunit.Abstractions;

namespace Extism.Sdk.Tests;

public class AsyncTests
{
    private readonly ITestOutputHelper output;
    private byte[] count_vowels;
    private byte[] sleepMs;
    public AsyncTests(ITestOutputHelper output)
    {
        this.output = output;
        var binDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
        count_vowels = File.ReadAllBytes(Path.Combine(binDirectory, "code.wasm"));
        sleepMs = File.ReadAllBytes(Path.Combine(binDirectory, "sleepMs.wasm"));
    }
    
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
    
    [Fact]
    public async void InvokingUnknownFunctionAsync_DoesntWork_ThrowsException() {
        using var context = new Context();
        // Test multiple plugin invocations to ensure that plugin calls can be repeated
        using var plugin = context.CreatePlugin(count_vowels, withWasi: true);
        var exception = await Assert.ThrowsAsync<ExtismException>(
            async () => { await plugin.CallFunctionAsync("unknown_function_name", Encoding.UTF8.GetBytes("Hello World")); });
        Assert.Equal("Function not found: unknown_function_name", exception.Message);
    }
    
    [Fact]
    public async void LongRunningtask_WithScheduledCancellation_ThrowsTaskCanceledException()
    {
        using var context = new Context();
        using var plugin = context.CreatePlugin(sleepMs, withWasi: true);
    
        CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
        
        // Schedule token to automatically cancel after 1 second.
        cancellationTokenSource.CancelAfter(1000);

        var sw = Stopwatch.StartNew();
        await Assert.ThrowsAsync<TaskCanceledException>(async () =>
        {
            // This should throw after ~1000 ms and not complete execution.
            var response = await plugin.CallFunctionAsync("sleepMs", Encoding.UTF8.GetBytes("[50000]"), null, cancellationTokenSource.Token);
            Assert.Fail("This should be unreachable.");
        });
        
        
        // Verify that the time that has passed is less than 1.5 seconds (should be very close to 1 second).
        output.WriteLine($"Expected: 1000 ms. Actual: {sw.ElapsedMilliseconds}.");
        Assert.True(sw.ElapsedMilliseconds < 1500);
        
    }
    
    // TODO: No cancellation of multiple parallel plugins (should make the wasm return the # of ms slept or something)
    // TODO: Cancel a plugin early and try running it again several times
    // TODO: Test native timeout support in Extism Context
    
    
    [Fact]
    public async void PrecancelledToken_DoesntRunWASMModule_ThrowsTaskCanceledException()
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
        // TODO: once Host Functions are implemented, we can verify that a host function is not called to verify the WASM isn't actually executing.
    }
    
    [Fact]
    public async void ExecutingPlugin_IsCanceledEarlyWhenCallingCancelMethod_StopsExecutingWithin500ms()
    {
        using var context = new Context();
        using var plugin = context.CreatePlugin(sleepMs, withWasi: true);
        
        // Register a WASM function that will take 50 seconds to complete.
        // NOTE: Purposely not awaited so plugin can run asynchronously in a parallel with our cancellation request.
        var functionInvocationTask = plugin.CallFunctionAsync("sleepMs", Encoding.UTF8.GetBytes("[50000]"));

        // Cancel the plugin after 1 second
        Stopwatch sw = new Stopwatch();
        await Task.Run(async () =>
        {
            sw.Start();
            await Task.Delay(1000);
            
            plugin.Cancel();
        });
        
        // Verify that the task throws a TaskCanceledException
        await Assert.ThrowsAsync<TaskCanceledException>(async () =>
        {
            var response = await functionInvocationTask;
        });
        
        sw.Stop();
        
        // Verify that the time that has passed is less than 1.5 seconds (should be very close to 1 second).
        output.WriteLine($"Expected: 1000 ms. Actual: {sw.ElapsedMilliseconds}.");
        Assert.True(sw.ElapsedMilliseconds < 1500);

    }
    
}