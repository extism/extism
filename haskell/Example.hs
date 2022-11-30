module Main where

import Extism
import Extism.Manifest(manifest, wasmFile)

unwrap (Right x) = x
unwrap (Left (ErrorMessage msg)) = do
  error msg

main = do
  context <- Extism.newContext
  plugin <- unwrap <$> Extism.pluginFromManifest context (manifest [wasmFile "../wasm/code.wasm"]) False 
  res <- unwrap <$> Extism.call plugin "count_vowels" (Extism.toByteString "this is a test")
  putStrLn (Extism.fromByteString res)
  Extism.free plugin
