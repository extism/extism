# Extism

Extism Host SDK for Elixir and Erlang

## Installation

You can find this package on [hex.pm](https://hex.pm/packages/extism).

```elixir
def deps do
  [
    {:extism, "~> 0.0.1-rc.5"}
  ]
end
```

## Usage

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
