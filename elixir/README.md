# Extism

Extism Host SDK for Elixir and Erlang

## Docs

Read the [docs on hexdocs.pm](https://hexdocs.pm/extism/).

## Installation

You can find this package on [hex.pm](https://hex.pm/packages/extism).

```elixir
def deps do
  [
    {:extism, "~> 0.1.0"}
  ]
end
```

## Getting Started

### Example

```elixir
# point to some wasm code, this is the count_vowels example that ships with extism
manifest = %{ wasm: [ %{ path: "/Users/ben/code/extism/wasm/code.wasm" } ]}
{:ok, plugin} = Extism.Plugin.new(manifest, false)
# {:ok,
# %Extism.Plugin{
#   resource: 0,
#   reference: #Reference<0.520418104.1263009793.80956>
# }}
{:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
# {:ok, "{\"count\": 4}"}
{:ok, result} = JSON.decode(output)
# {:ok, %{"count" => 4}}
```

### Modules

The primary modules you should learn is:

* [Extism.Plugin](Extism.Plugin.html)

#### Plugin

The [Plugin](Extism.Plugin.html) represents an instance of your WASM program from the given manifest.
The key method to know here is [Extism.Plugin#call](Extism.Plugin.html#call/3) which takes a function name to invoke and some input data, and returns the results from the plugin.

```elixir
{:ok, plugin} = Extism.Plugin.new(manifest, false)
{:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
```
