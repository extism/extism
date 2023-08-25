# Extism

## Getting Started

### Example

```ruby
require "extism"
require "json"

manifest = {
  :wasm => [{ :path => "../wasm/code.wasm" }],
}
plugin = Plugin.new(manifest)
res = JSON.parse(plugin.call("count_vowels", "this is a test"))
```

### API

There is just one primary class you need to understand:

* [Plugin](Extism/Plugin.html)

#### Plugin

The [Plugin](Extism/Plugin.html) represents an instance of your WASM program from the given manifest.
The key method to know here is [Extism::Plugin#call](Extism/Plugin.html#call-instance_method) which takes a function name to invoke and some input data, and returns the results from the plugin.
