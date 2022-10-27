# Extism

Extism Host SDK for Elixir and Erlang

## Docs

Read the [docs on hexdocs.pm](https://hexdocs.pm/extism/).

## Installation

You can find this package on [hex.pm](https://hex.pm/packages/extism).

```elixir
def deps do
  [
    {:extism, "~> 0.0.1-rc.5"}
  ]
end
```

## Getting Started

### Example

```elixir
# Create a context for which plugins can be allocated and cleaned
ctx = Extism.Context.new()

# point to some wasm code, this is the count_vowels example that ships with extism
manifest = %{ wasm: [ %{ path: "/Users/ben/code/extism/wasm/code.wasm" } ]}
{:ok, plugin} = Extism.Context.new_plugin(ctx, manifest, false)
# {:ok,
# %Extism.Plugin{
#   resource: 0,
#   reference: #Reference<0.520418104.1263009793.80956>
# }}
{:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
# {:ok, "{\"count\": 4}"}
{:ok, result} = JSON.decode(output)
# {:ok, %{"count" => 4}}

# free up the context and any plugins we allocated
Extism.Context.free(ctx)
```

### Modules

The two primary modules you should learn are:

* [Extism.Context](Extism.Context.html)
* [Extism.Plugin](Extism.Plugin.html)

#### Context

The [Context](Extism.Context.html) can be thought of as a session. You need a context to interact with the Extism runtime. The context holds your plugins and when you free the context, it frees your plugins. It's important to free up your context and plugins when you are done with them.

```elixir
ctx = Extism.Context.new()
# frees all the plugins 
Extism.Context.reset(ctx)
# frees the context and all its plugins
Extism.Context.free(ctx)
```

#### Plugin

The [Plugin](Extism.Plugin.html) represents an instance of your WASM program from the given manifest.
The key method to know here is [Extism.Plugin#call](Extism.Plugin.html#call/3) which takes a function name to invoke and some input data, and returns the results from the plugin.

```elixir
{:ok, plugin} = Extism.Context.new_plugin(ctx, manifest, false)
{:ok, output} = Extism.Plugin.call(plugin, "count_vowels", "this is a test")
```