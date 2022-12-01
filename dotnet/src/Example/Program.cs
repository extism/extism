using ExtismSharp.Native;

using System.Text;

using var context = new Context();

var wasm = await File.ReadAllBytesAsync("code.wasm");
using var plugin = context.CreatePlugin(wasm, withWasi: true);

Console.Write("Input: ");
var input = Console.ReadLine() ?? "";

var response = plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes(input));
var output = Encoding.UTF8.GetString(plugin.OutputData());
Console.WriteLine(output);