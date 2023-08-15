using Extism.Sdk;
using Extism.Sdk.Native;

using System.Runtime.InteropServices;
using System.Text;

var context = new Context();

var userData = Marshal.StringToHGlobalAnsi("Hello again!");

var isVowel = new HostFunction(
    "is_vowel", 
    "host",
    new ExtismValType[] { ExtismValType.I32 },
    new ExtismValType[] { ExtismValType.I32 },
    0, 
    IsVowel);

void IsVowel(CurrentPlugin plugin, Span<ExtismVal> inputs, Span<ExtismVal> outputs, nint userData)
{
    var bytes = plugin.ReadBytes(inputs[0].v.i32);

    var text = Encoding.UTF8.GetString(bytes);

    switch (char.ToLowerInvariant(text[0]))
    {

        case 'a':
        case 'A':
        case 'e':
        case 'E':
        case 'i':
        case 'I':
        case 'o':
        case 'O':
        case 'u':
        case 'U':
            outputs[0].v.i32 = 1;
            return;
    }

    outputs[0].v.i32 = 0;
}

var wasm = File.ReadAllBytes(@"csharp-plugin.wasm");
using var plugin = context.CreatePlugin(wasm, new[] { isVowel }, withWasi: true);

var output = Encoding.UTF8.GetString(
    plugin.CallFunction("count_vowels", Encoding.UTF8.GetBytes("Hello World!"))
);

Console.WriteLine($"Output: {output}");

