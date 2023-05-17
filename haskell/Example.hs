module Main where

import Extism
import Extism.Manifest(manifest, wasmFile)

unwrap (Right x) = x
unwrap (Left (ExtismError msg)) = do
  error msg

main = do
  let m = manifest [wasmFile "../wasm/code.wasm"]
  plugin <- unwrap <$> Extism.createPluginFromManifest m False
  res <- unwrap <$> Extism.call plugin "count_vowels" (Extism.toByteString "this is a test")
  putStrLn (Extism.fromByteString res)
  Extism.free plugin
