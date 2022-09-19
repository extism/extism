require './lib/extism'
require 'json'

manifest = {
  :wasm => [{:path => "../wasm/code.wasm"}]
}

ctx = Extism::Context.new
plugin = ctx.plugin(manifest)
res = JSON.parse(plugin.call("count_vowels", ARGV[0] || "this is a test"))
puts res['count']
plugin.free
ctx.free