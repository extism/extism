require './lib/extism'
require 'json'

manifest = {
  :wasm => [{:path => "../wasm/code.wasm"}]
}

ctx = Extism::Context.new
Extism::with_context {|ctx| 
  plugin = ctx.plugin(manifest)
  res = JSON.parse(plugin.call("count_vowels", ARGV[0] || "this is a test"))
  puts res['count']
}
