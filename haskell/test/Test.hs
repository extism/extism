import Test.HUnit
import Extism
import Extism.Manifest


unwrap (Right x) = return x
unwrap (Left (ExtismError msg)) =
  assertFailure msg

defaultManifest = manifest [wasmFile "../../wasm/code.wasm"]

initPlugin :: Maybe Context -> IO Plugin
initPlugin Nothing =
  Extism.createPluginFromManifest defaultManifest False >>= unwrap
initPlugin (Just ctx) =
  Extism.pluginFromManifest ctx defaultManifest False >>= unwrap

pluginFunctionExists = do
  p <- initPlugin Nothing
  exists <- functionExists p "count_vowels"
  assertBool "function exists" exists
  exists' <- functionExists p "function_doesnt_exist"
  assertBool "function doesn't exist" (not exists')

checkCallResult p = do
  res <- call p "count_vowels" (toByteString "this is a test") >>= unwrap
  assertEqual "count vowels output" "{\"count\": 4}" (fromByteString res)

pluginCall = do
  p <- initPlugin Nothing
  checkCallResult p

pluginMultiple = do
  withContext(\ctx -> do
    p <- initPlugin (Just ctx)
    checkCallResult p
    q <- initPlugin (Just ctx)
    r <- initPlugin (Just ctx)
    checkCallResult q
    checkCallResult r)

pluginUpdate = do
  withContext (\ctx -> do
    p <- initPlugin (Just ctx)
    updateManifest p defaultManifest True >>= unwrap
    checkCallResult p)

pluginConfig = do
  withContext (\ctx -> do
    p <- initPlugin (Just ctx)
    b <- setConfig p [("a", Just "1"), ("b", Just "2"), ("c", Just "3"), ("d", Nothing)]
    assertBool "set config" b)

testSetLogFile = do
  b <- setLogFile "stderr" Extism.Error
  assertBool "set log file" b

t name f = TestLabel name (TestCase f)

main = do
  runTestTT (TestList
    [
      t "Plugin.FunctionExists" pluginFunctionExists
      , t "Plugin.Call" pluginCall
      , t "Plugin.Multiple" pluginMultiple
      , t "Plugin.Update" pluginUpdate
      , t "Plugin.Config" pluginConfig
      , t "SetLogFile" testSetLogFile
    ])

