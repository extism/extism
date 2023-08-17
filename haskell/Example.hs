module Main where

import Extism
import Extism.HostFunction
import Extism.Manifest(manifest, wasmFile)

hello plugin params msg = do
  putStrLn "Hello from Haskell!"
  putStrLn msg
  offs <- allocBytes plugin (toByteString "{\"count\": 999}")
  return [toI64 offs]

main = do
  setLogFile "stdout" LogError
  let m = manifest [wasmFile "../wasm/code-functions.wasm"]
  f <- hostFunction "hello_world" [I64] [I64] hello "Hello, again"
  plugin <- unwrap <$> pluginFromManifest m [f] True
  id <- pluginID plugin
  print id
  res <- unwrap <$> call plugin "count_vowels" (toByteString "this is a test")
  putStrLn (fromByteString res)
