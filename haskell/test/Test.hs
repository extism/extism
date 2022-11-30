import Test.HUnit
import Extism
import Extism.Manifest


unwrap (Right x) = return x
unwrap (Left (ErrorMessage msg)) =
  assertFailure msg

defaultManifest = manifest [wasmFile "test/code.wasm"]

initPlugin :: Context -> IO Plugin
initPlugin context =
  Extism.pluginFromManifest context defaultManifest False >>= unwrap

pluginFunctionExists = do
  withContext (\ctx -> do
    p <- initPlugin ctx
    exists <- functionExists p "count_vowels"
    assertBool "function exists" exists
    exists' <- functionExists p "function_doesnt_exist"
    assertBool "function doesn't exist" (not exists'))

checkCallResult p = do
    res <- call p "count_vowels" (toByteString "this is a test") >>= unwrap
    assertEqual "count vowels output" "{\"count\": 4}" (fromByteString res)

pluginCall = do
  withContext (\ctx -> do
    p <- initPlugin ctx
    checkCallResult p)

pluginMultiple = do
  withContext (\ctx -> do
    p <- initPlugin ctx
    checkCallResult p
    q <- initPlugin ctx
    r <- initPlugin ctx
    checkCallResult q
    checkCallResult r)

pluginUpdate = do
  withContext (\ctx -> do
    p <- initPlugin ctx
    updateManifest p defaultManifest True >>= unwrap
    checkCallResult p)

pluginConfig = do
  withContext (\ctx -> do
    p <- initPlugin ctx
    b <- setConfig p [("a", Just "1"), ("b", Just "2"), ("c", Just "3"), ("d", Nothing)]
    assertBool "set config" b)

testSetLogFile = do
  b <- setLogFile "stderr" Error
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

