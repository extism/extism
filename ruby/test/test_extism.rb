# frozen_string_literal: true

require "test_helper"

class TestExtism < Minitest::Test
  def test_that_it_has_a_version_number
    refute_nil Extism::VERSION
  end

  def test_plugin_call
    plugin = Extism::Plugin.new(manifest)
    res = JSON.parse(plugin.call("count_vowels", "this is a test"))
    assert_equal res["count"], 4
    res = JSON.parse(plugin.call("count_vowels", "this is a test again"))
    assert_equal res["count"], 7
    res = JSON.parse(plugin.call("count_vowels", "this is a test thrice"))
    assert_equal res["count"], 6
    res = JSON.parse(plugin.call("count_vowels", "🌎hello🌎world🌎"))
    assert_equal res["count"], 3
  end

  def test_can_free_plugin
    plugin = Extism::Plugin.new(manifest)
    _res = plugin.call("count_vowels", "this is a test")
    plugin.free
    assert_raises(Extism::Error) do
      _res = plugin.call("count_vowels", "this is a test")
    end
  end

  def test_errors_on_bad_manifest
    assert_raises(Extism::Error) do
      _plugin = Extism::Plugin.new({ not_a_real_manifest: true })
    end
  end

  def test_has_function
    plugin = Extism::Plugin.new(manifest)
    assert plugin.has_function? "count_vowels"
    refute plugin.has_function? "i_am_not_a_function"
  end

  def test_errors_on_unknown_function
    plugin = Extism::Plugin.new(manifest)
    assert_raises(Extism::Error) do
      plugin.call("non_existent_function", "input")
    end
  end

  private

  def manifest
    {
      wasm: [
        {
          path: File.join(__dir__, "../../wasm/code.wasm"),
        },
      ],
    }
  end
end
