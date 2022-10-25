import Test.HUnit
import Extism
import Extism.Manifest


unwrap' (Right x) = return x
unwrap' (Left (ErrorMessage msg)) =
  assertFailure msg

unwrap io = do
  x <- io
  unwrap' x

defaultManifest = manifest [wasmFile "test/code.wasm"]

initPlugin context =
  unwrap (Extism.pluginFromManifest context defaultManifest False)

pluginFunctionExists = do
  withContext (\ctx -> do
    p <- initPlugin ctx
    exists <- functionExists p "count_vowels"
    assertBool "function exists" exists
    exists' <- functionExists p "function_doesnt_exist"
    assertBool "function doesn't exist" (not exists'))
    
checkCallResult p = do
    res <- unwrap (call p "count_vowels" (toByteString "this is a test"))
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
    unwrap (updateManifest p defaultManifest True)
    checkCallResult p)

pluginConfig = do
  withContext (\ctx -> do
    p <- initPlugin ctx
    b <- setConfig p [("a", Just "1"), ("b", Just "2"), ("c", Just "3"), ("d", Nothing)]
    assertBool "set config" b)

t name f = TestLabel name (TestCase f)

main = do
  runTestTT (TestList
    [
      t "Plugin.FunctionExists" pluginFunctionExists
      , t "Plugin.Call" pluginCall
      , t "Plugin.Multiple" pluginMultiple
      , t "Plugin.Update" pluginUpdate
      , t "Plugin.Config" pluginConfig
    ])

