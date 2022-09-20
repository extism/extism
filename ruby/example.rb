require './lib/extism'
require 'json'

# a Context provides a scope for plugins to be managed within. creating multiple contexts
# is expected and groups plugins based on source/tenant/lifetime etc.
ctx = Extism::Context.new
Extism::with_context {|ctx| 
  manifest = {
    :wasm => [{:path => "../wasm/code.wasm"}]
  }

  plugin = ctx.plugin(manifest)
  res = JSON.parse(plugin.call("count_vowels", ARGV[0] || "this is a test"))
  
  puts res['count']
}
