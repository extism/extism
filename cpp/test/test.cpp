#include "../extism.hpp"

#include <fstream>

#include <gtest/gtest.h>

std::vector<uint8_t> read(const char *filename) {
  std::ifstream file(filename, std::ios::binary);
  return std::vector<uint8_t>((std::istreambuf_iterator<char>(file)),
                              std::istreambuf_iterator<char>());
}

const std::string code = "../../wasm/code.wasm";

namespace {
using namespace extism;

TEST(Context, Basic) {
  Context context;
  ASSERT_NE(context.pointer, nullptr);
}

TEST(Plugin, Manifest) {
  Manifest manifest = Manifest::path(code);
  manifest.set_config("a", "1");

  ASSERT_NO_THROW(Plugin plugin(manifest));
  Plugin plugin(manifest);

  Buffer buf = plugin.call("count_vowels", "this is a test");
  ASSERT_EQ((std::string)buf, "{\"count\": 4}");
}

TEST(Plugin, BadManifest) {
  Manifest manifest;
  ASSERT_THROW(Plugin plugin(manifest), Error);
}

TEST(Plugin, Bytes) {
  Context context;
  auto wasm = read(code.c_str());
  ASSERT_NO_THROW(Plugin plugin = context.plugin(wasm));
  Plugin plugin = context.plugin(wasm);

  Buffer buf = plugin.call("count_vowels", "this is another test");
  ASSERT_EQ(buf.string(), "{\"count\": 6}");
}

TEST(Plugin, UpdateConfig) {
  Context context;
  auto wasm = read(code.c_str());
  Plugin plugin = context.plugin(wasm);

  Config config;
  config["abc"] = "123";
  ASSERT_NO_THROW(plugin.config(config));
}

TEST(Plugin, FunctionExists) {
  Context context;
  auto wasm = read(code.c_str());
  Plugin plugin = context.plugin(wasm);

  ASSERT_FALSE(plugin.function_exists("bad_function"));
  ASSERT_TRUE(plugin.function_exists("count_vowels"));
}

TEST(Plugin, HostFunction) {
  auto wasm = read("../../wasm/code-functions.wasm");
  auto t = std::vector<ValType>{ValType::I64};
  Function hello_world =
      Function("hello_world", t, t,
               [](CurrentPlugin plugin, const std::vector<Val> &params,
                  std::vector<Val> &results, void *user_data) {
                 auto offs = plugin.alloc(4);
                 memcpy(plugin.memory() + offs, "test", 4);
                 results[0].v.i64 = (int64_t)offs;
               });
  auto functions = std::vector<Function>{
      hello_world,
  };
  Plugin plugin(wasm, true, functions);
  auto buf = plugin.call("count_vowels", "aaa");
  ASSERT_EQ(buf.length, 4);
  ASSERT_EQ((std::string)buf, "test");
}

}; // namespace

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}