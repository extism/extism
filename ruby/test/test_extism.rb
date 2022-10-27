# frozen_string_literal: true

require "test_helper"

class TestExtism < Minitest::Test
  def test_that_it_has_a_version_number
    refute_nil Extism::VERSION
  end

  def test_create_context
    refute_nil Extism::Context.new
  end

  def test_plugin_call
    Extism.with_context do |ctx|
      plugin = ctx.plugin(manifest)
      res = JSON.parse(plugin.call("count_vowels", "this is a test"))
      assert_equal res["count"], 4
      res = JSON.parse(plugin.call("count_vowels", "this is a test again"))
      assert_equal res["count"], 7
      res = JSON.parse(plugin.call("count_vowels", "this is a test thrice"))
      assert_equal res["count"], 6
    end
  end

  def test_can_free_plugin
    ctx = Extism::Context.new
    plugin = ctx.plugin(manifest)
    _res = plugin.call("count_vowels", "this is a test")
    plugin.free
    assert_raises(Extism::Error) do
      _res = plugin.call("count_vowels", "this is a test")
    end
    ctx.free
  end

  def test_can_update_a_manifest
    Extism.with_context do |ctx|
      plugin = ctx.plugin(manifest)
      # let's load a raw wasm module rather than use a manifest
      raw_module = IO.read("../wasm/code.wasm")
      plugin.update(raw_module)
      # check we can still call it
      res = JSON.parse(plugin.call("count_vowels", "this is a test"))
      assert_equal res["count"], 4
    end
  end

  def test_errors_on_bad_manifest
    Extism.with_context do |ctx|
      assert_raises(Extism::Error) do
        _plugin = ctx.plugin({ not_a_real_manifest: true })
      end
      plugin = ctx.plugin(manifest)
      assert_raises(Extism::Error) do
        plugin.update({ not_a_real_manifest: true })
      end
    end
  end

  def test_has_function
    Extism.with_context do |ctx|
      plugin = ctx.plugin(manifest)
      assert plugin.has_function? "count_vowels"
      refute plugin.has_function? "i_am_not_a_function"
    end
  end

  def test_errors_on_unknown_function
    Extism.with_context do |ctx|
      plugin = ctx.plugin(manifest)
      assert_raises(Extism::Error) do
        plugin.call("non_existent_function", "input")
      end
    end
  end

  private

  def manifest
    {
      wasm: [
        {
          path: File.join(__dir__, "code.wasm"),
        },
      ],
    }
  end
end
