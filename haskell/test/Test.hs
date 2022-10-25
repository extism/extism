import Test.HUnit
import Extism
import Extism.Manifest
  

unwrap' (Right x) = return x
unwrap' (Left (ErrorMessage msg)) =
  assertFailure msg
  
unwrap io = do
  x <- io
  unwrap' x

plugin' context =
  Extism.pluginFromManifest context (manifest [wasmFile "test/code.wasm"]) False

pluginFunctionExists = do
  withContext (\ctx -> do
    p <- unwrap (plugin' ctx)
    exists <- functionExists p "count_vowels"
    () <- assertBool "function exists" exists
    exists' <- functionExists p "function_doesnt_exist"
    assertBool "function doesn't exist" (not exists'))

t name f = TestLabel name (TestCase f)

main = do
  runTestTT (TestList 
    [
      t "Plugin.FunctionExists" pluginFunctionExists
    ])

