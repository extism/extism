#include "../extism.hpp"

#include <fstream>

#include <gtest/gtest.h>

std::vector<uint8_t> read(const char *filename) {
  std::ifstream file(filename, std::ios::binary);
  return std::vector<uint8_t>((std::istreambuf_iterator<char>(file)),
                              std::istreambuf_iterator<char>());
}

namespace {
using namespace extism;

TEST(Context, Basic) {
  Context context;
  ASSERT_NE(context.pointer, nullptr);
}

TEST(Plugin, Manifest) {
  Context context;
  Manifest manifest = Manifest::path("code.wasm");
  manifest.set_config("a", "1");

  ASSERT_NO_THROW(Plugin plugin = context.plugin(manifest));
  Plugin plugin = context.plugin(manifest);

  Buffer buf = plugin.call("count_vowels", "this is a test");
  ASSERT_EQ((std::string)buf, "{\"count\": 4}");
}

TEST(Plugin, BadManifest) {
  Context context;
  Manifest manifest;
  ASSERT_THROW(Plugin plugin = context.plugin(manifest), Error);
}

TEST(Plugin, Bytes) {
  Context context;
  auto wasm = read("code.wasm");
  ASSERT_NO_THROW(Plugin plugin = context.plugin(wasm));
  Plugin plugin = context.plugin(wasm);

  Buffer buf = plugin.call("count_vowels", "this is another test");
  ASSERT_EQ(buf.string(), "{\"count\": 6}");
}

TEST(Plugin, UpdateConfig) {
  Context context;
  auto wasm = read("code.wasm");
  Plugin plugin = context.plugin(wasm);

  Config config;
  config["abc"] = "123";
  ASSERT_NO_THROW(plugin.config(config));
}

TEST(Plugin, FunctionExists) {
  Context context;
  auto wasm = read("code.wasm");
  Plugin plugin = context.plugin(wasm);

  ASSERT_FALSE(plugin.function_exists("bad_function"));
  ASSERT_TRUE(plugin.function_exists("count_vowels"));
}

}; // namespace

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}