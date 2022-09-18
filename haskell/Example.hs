module Main where

import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString as B
import Extism
import Extism.Manifest

try f (Right x) = f x
try f (Left (Error msg)) = do
  _ <- putStrLn msg
  exitFailure

handlePlugin plugin = do
  res <- Extism.call plugin "count_vowels" (Extism.toByteString "this is a test")
  try (\bs -> do
    _ <- putStrLn (Extism.fromByteString bs)
    _ <- Extism.destroy plugin
    exitSuccess) res

main = do
  plugin <- Extism.registerManifest (manifest [wasmFile "../wasm/code.wasm"]) False
  try handlePlugin plugin