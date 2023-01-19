require "./lib/extism"
require "json"

# a Context provides a scope for plugins to be managed within. creating multiple contexts
# is expected and groups plugins based on source/tenant/lifetime etc.
# We recommend you use `Extism.with_context` unless you have a reason to keep your context around.
# If you do you can create a context with `Extism#new`, example: `ctx = Extism.new`
Extism.with_context do |ctx|
  manifest = {
    :wasm => [{ :path => "code.wasm" }],
  }

  plugin = ctx.plugin(manifest)
  res = JSON.parse(plugin.call("count_vowels", ARGV[0] || "this is a test"))

  puts res["count"]
end
