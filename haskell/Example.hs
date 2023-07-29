module Main where

import Extism
import Extism.Manifest(manifest, wasmFile)

unwrap (Right x) = x
unwrap (Left (ExtismError msg)) = do
  error msg

hello plugin params () = do
  putStrLn "Hello from Haskell!"
  offs <- currentPluginAllocBytes plugin (Extism.toByteString "{\"count\": 999}")
  return [toI64 offs]

main = do
  setLogFile "stdout" Error
  let m = manifest [wasmFile "../wasm/code-functions.wasm"]
  f <- hostFunction "hello_world" [I64] [I64] hello ()
  plugin <- unwrap <$> createPluginFromManifest m [f] True
  res <- unwrap <$> call plugin "count_vowels" (toByteString "this is a test")
  putStrLn (fromByteString res)
  free plugin
