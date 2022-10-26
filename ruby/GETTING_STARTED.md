# Extism

## Getting Started

### Example

```ruby
require "extism"
require "json"

Extism.with_context do |ctx|
  manifest = {
    :wasm => [{ :path => "../wasm/code.wasm" }],
  }
  plugin = ctx.plugin(manifest)
  res = JSON.parse(plugin.call("count_vowels", "this is a test"))
  puts res["count"] # => 4
end
```

### API

There are two primary classes you need to understand:

* [Context](Extism/Context.html)
* [Plugin](Extism/Plugin.html)

#### Context

The [Context](Extism/Context.html) can be thought of as a session. You need a context to interact with the Extism runtime. The context holds your plugins and when you free the context, it frees your plugins. We recommend using the [Extism.with_context](Extism.html#with_context-class_method) method to ensure that your plugins are cleaned up. But if you need a long lived context for some reason, you can use the constructor [Extism::Context.new](Extism/Context.html#initialize-instance_method).

#### Plugin

The [Plugin](Extism/Plugin.html) represents an instance of your WASM program from the given manifest.
The key method to know here is [Extism::Plugin#call](Extism/Plugin.html#call-instance_method) which takes a function name to invoke and some input data, and returns the results from the plugin.
