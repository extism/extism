module Main where

import Extism
import Extism.Manifest(manifest, wasmFile)

unwrap (Right x) = x
unwrap (Left (ExtismError msg)) = do
  error msg

hello plugin params () = do
  putStrLn "Hello from Haskell!"
  offs <- currentPluginMemoryAlloc plugin 7
  print (fromI64 $ (params!!0))
  print offs
  return [toI64 (fromIntegral offs)]

main = do
  let m = manifest [wasmFile "../wasm/code-functions.wasm"]
  f <- hostFunction "hello_world" [I64] [I64] hello ()
  plugin <- unwrap <$> Extism.createPluginFromManifest m [f] True
  res <- unwrap <$> Extism.call plugin "count_vowels" (Extism.toByteString "this is a test")
  putStrLn (Extism.fromByteString res)
  Extism.free plugin
