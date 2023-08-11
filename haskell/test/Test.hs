import Test.HUnit
import Extism
import Extism.Manifest
import Extism.CurrentPlugin


unwrap (Right x) = return x
unwrap (Left (ExtismError msg)) =
  assertFailure msg

defaultManifest = manifest [wasmFile "../../wasm/code.wasm"]
hostFunctionManifest = manifest [wasmFile "../../wasm/code-functions.wasm"]

initPlugin :: IO Plugin
initPlugin =
  Extism.pluginFromManifest defaultManifest [] False >>= unwrap

pluginFunctionExists = do
  p <- initPlugin 
  exists <- functionExists p "count_vowels"
  assertBool "function exists" exists
  exists' <- functionExists p "function_doesnt_exist"
  assertBool "function doesn't exist" (not exists')

checkCallResult p = do
  res <- call p "count_vowels" (toByteString "this is a test") >>= unwrap
  assertEqual "count vowels output" "{\"count\": 4}" (fromByteString res)

pluginCall = do
  p <- initPlugin 
  checkCallResult p


hello plugin params () = do
  putStrLn "Hello from Haskell!"
  offs <- allocBytes plugin (toByteString "{\"count\": 999}")
  return [toI64 offs]

pluginCallHostFunction = do
  p <- Extism.pluginFromManifest hostFunctionManifest [] False >>= unwrap
  res <- call p "count_vowels" (toByteString "this is a test") >>= unwrap
  assertEqual "count vowels output" "{\"count\": 999}" (fromByteString res)

pluginMultiple = do
    p <- initPlugin
    checkCallResult p
    q <- initPlugin
    r <- initPlugin
    checkCallResult q
    checkCallResult r

pluginConfig = do
  p <- initPlugin
  b <- setConfig p [("a", Just "1"), ("b", Just "2"), ("c", Just "3"), ("d", Nothing)]
  assertBool "set config" b

testSetLogFile = do
  b <- setLogFile "stderr" Extism.Error
  assertBool "set log file" b

t name f = TestLabel name (TestCase f)

main = do
  runTestTT (TestList
    [
      t "Plugin.FunctionExists" pluginFunctionExists
      , t "Plugin.Call" pluginCall
      , t "Plugin.CallHostFunction" pluginCallHostFunction
      , t "Plugin.Multiple" pluginMultiple
      , t "Plugin.Config" pluginConfig
      , t "SetLogFile" testSetLogFile
    ])

