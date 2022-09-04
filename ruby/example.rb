require './lib/extism'
require 'json'

manifest = {
  :wasm => [{:path => "../wasm/code.wasm"}]
}
plugin = Extism::Plugin.new(manifest)
res = JSON.parse(plugin.call("count_vowels", ARGV[0] || "this is a test"))
puts res['count']

