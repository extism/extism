using Extism.Sdk.Native;

using System.Reflection;
using System.Text;

Console.WriteLine($"Extism version {Context.GetExtismVersion()}");
var x = Context.SetExtismLogFile("log.log", LogLevel.Trace);

using var context = new Context();

var binDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
var wasm = File.ReadAllBytes(Path.Combine(binDirectory, "code.wasm"));
using var plugin = context.CreatePlugin(wasm, withWasi: true);

Console.Write("Input: ");
var input = Console.ReadLine() ?? "";

var output = plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes(input));
Console.WriteLine(Encoding.UTF8.GetString(output));