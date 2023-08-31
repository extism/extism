# frozen_string_literal: true

require 'test_helper'

class TestExtism < Minitest::Test
  def test_that_it_has_a_version_number
    refute_nil Extism::VERSION
  end

  def test_plugin_call
    plugin = Extism::Plugin.new(manifest)
    res = JSON.parse(plugin.call('count_vowels', 'this is a test'))
    assert_equal res['count'], 4
    res = JSON.parse(plugin.call('count_vowels', 'this is a test again'))
    assert_equal res['count'], 7
    res = JSON.parse(plugin.call('count_vowels', 'this is a test thrice'))
    assert_equal res['count'], 6
    res = JSON.parse(plugin.call('count_vowels', '🌎hello🌎world🌎'))
    assert_equal res['count'], 3
  end

  def test_can_free_plugin
    plugin = Extism::Plugin.new(manifest)
    _res = plugin.call('count_vowels', 'this is a test')
    plugin.free
    assert_raises(Extism::Error) do
      _res = plugin.call('count_vowels', 'this is a test')
    end
  end

  def test_errors_on_bad_manifest
    assert_raises(Extism::Error) do
      _plugin = Extism::Plugin.new({ not_a_real_manifest: true })
    end
  end

  def test_has_function
    plugin = Extism::Plugin.new(manifest)
    assert plugin.has_function? 'count_vowels'
    refute plugin.has_function? 'i_am_not_a_function'
  end

  def test_errors_on_unknown_function
    plugin = Extism::Plugin.new(manifest)
    assert_raises(Extism::Error) do
      plugin.call('non_existent_function', 'input')
    end
  end

  def test_host_functions
    # Extism.set_log_file('stdout', 'trace')
    func = proc do |current_plugin, inputs, outputs, user_data|
      puts current_plugin
      puts inputs
      puts outputs
      puts user_data
    end
    f = Extism::Function.new('hello_world', [Extism::ValType::I64], [Extism::ValType::I64], func, nil)
    plugin = Extism::Plugin.new(host_manifest, [f], true)
    result = plugin.call('count_vowels', 'Hello, World!')
    puts result
  end

  private

  def manifest
    {
      wasm: [
        {
          path: File.join(__dir__, '../../wasm/code.wasm')
        }
      ]
    }
  end

  def host_manifest
    {
      wasm: [
        {
          path: File.join(__dir__, '../../wasm/code-functions.wasm')
        }
      ]
    }
  end
end
