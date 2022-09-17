module Main where

import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString as B
import Extism
import Extism.Manifest

main = do
  plugin <- Extism.registerManifest (manifest [wasmFile "../wasm/code.wasm"]) False
  case plugin of
    Right (Error msg) -> do
      _ <- putStrLn msg
      exitFailure
    Left plugin -> do
      res <- Extism.call plugin "count_vowels" (Extism.toByteString "this is a test")
      case res of
        Right (Error msg) -> do
          _ <- putStrLn msg
          exitFailure
        Left bs -> do
          _ <- putStrLn (Extism.fromByteString bs)
          exitSuccess
