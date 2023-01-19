module Main where

import Extism
import Extism.Manifest(manifest, wasmFile)

unwrap (Right x) = x
unwrap (Left (ExtismError msg)) = do
  error msg

main = do
  let m = manifest [wasmFile "code.wasm"]
  context <- Extism.newContext
  plugin <- unwrap <$> Extism.pluginFromManifest context m False
  res <- unwrap <$> Extism.call plugin "count_vowels" (Extism.toByteString "this is a test")
  putStrLn (Extism.fromByteString res)
  Extism.free plugin
