#include "../extism.hpp"

#include <fstream>
#include <thread>

#include <gtest/gtest.h>

std::vector<uint8_t> read(const char *filename) {
  std::ifstream file(filename, std::ios::binary);
  return std::vector<uint8_t>((std::istreambuf_iterator<char>(file)),
                              std::istreambuf_iterator<char>());
}

const std::string code = "../../wasm/code.wasm";

namespace {
using namespace extism;

TEST(Plugin, Manifest) {
  Manifest manifest = Manifest::path(code);
  manifest.set_config("a", "1");

  Plugin plugin(manifest);

  Buffer buf = plugin.call("count_vowels", "this is a test");
  ASSERT_EQ((std::string)buf, "{\"count\": 4}");
}

TEST(Plugin, BadManifest) {
  Manifest manifest;
  ASSERT_THROW(Plugin plugin(manifest), Error);
}

TEST(Plugin, Bytes) {
  auto wasm = read(code.c_str());
  ASSERT_NO_THROW(Plugin plugin(wasm));
  Plugin plugin(wasm);

  Buffer buf = plugin.call("count_vowels", "this is another test");
  ASSERT_EQ(buf.string(), "{\"count\": 6}");
}

TEST(Plugin, UpdateConfig) {
  auto wasm = read(code.c_str());
  Plugin plugin(wasm);

  Config config;
  config["abc"] = "123";
  ASSERT_NO_THROW(plugin.config(config));
}

TEST(Plugin, FunctionExists) {
  auto wasm = read(code.c_str());
  Plugin plugin(wasm);

  ASSERT_FALSE(plugin.functionExists("bad_function"));
  ASSERT_TRUE(plugin.functionExists("count_vowels"));
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

void callThread(Plugin *plugin) {
  auto buf = plugin->call("count_vowels", "aaa").string();
  ASSERT_EQ(buf.size(), 10);
  ASSERT_EQ(buf, "testing123");
}

TEST(Plugin, MultipleThreads) {
  auto wasm = read("../../wasm/code-functions.wasm");
  auto t = std::vector<ValType>{ValType::I64};
  Function hello_world =
      Function("hello_world", t, t,
               [](CurrentPlugin plugin, const std::vector<Val> &params,
                  std::vector<Val> &results, void *user_data) {
                 auto offs = plugin.alloc(10);
                 memcpy(plugin.memory() + offs, "testing123", 10);
                 results[0].v.i64 = (int64_t)offs;
               });
  auto functions = std::vector<Function>{
      hello_world,
  };
  Plugin plugin(wasm, true, functions);

  std::vector<std::thread> threads;
  for (int i = 0; i < 3; i++) {
    threads.push_back(std::thread(callThread, &plugin));
  }

  for (auto &th : threads) {
    th.join();
  }
}

}; // namespace

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}