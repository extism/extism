module Extism (module Extism, module Extism.Manifest) where
import Data.Int
import Data.Word
import Control.Monad (void)
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Ptr
import Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Bifunctor (second)
import Text.JSON (encode, toJSObject, showJSON)
import Extism.Manifest (Manifest, toString)
import Extism.Bindings

-- | Context for managing plugins
newtype Context = Context (ForeignPtr ExtismContext)

-- | Plugins can be used to call WASM function
data Plugin = Plugin Context Int32

data CancelHandle = CancelHandle (Ptr ExtismCancelHandle)

-- | Log level
data LogLevel = Error | Warn | Info | Debug | Trace deriving (Show)

-- | Extism error
newtype Error = ExtismError String deriving Show

-- | Result type
type Result a = Either Error a

-- | Helper function to convert a 'String' to a 'ByteString'
toByteString :: String -> ByteString
toByteString x = B.pack (Prelude.map c2w x)

-- | Helper function to convert a 'ByteString' to a 'String'
fromByteString :: ByteString -> String
fromByteString bs = Prelude.map w2c $ B.unpack bs

-- | Get the Extism version string
extismVersion :: () -> IO String
extismVersion () = do
  v <- extism_version
  peekCString v

-- | Remove all registered plugins in a 'Context'
reset :: Context -> IO ()
reset (Context ctx) =
  withForeignPtr ctx extism_context_reset

-- | Create a new 'Context'
newContext :: IO Context
newContext = do
  ptr <- extism_context_new
  fptr <- newForeignPtr extism_context_free ptr
  return (Context fptr)
 
-- | Execute a function with a new 'Context' that is destroyed when it returns
withContext :: (Context -> IO a) -> IO a
withContext f = do
  ctx <- newContext
  f ctx

-- | Create a 'Plugin' from a WASM module, `useWasi` determines if WASI should
-- | be linked
plugin :: Context -> B.ByteString -> Bool -> IO (Result Plugin)
plugin c wasm useWasi =
  let length = fromIntegral (B.length wasm) in
  let wasi = fromInteger (if useWasi then 1 else 0) in
  let Context ctx = c in
  do
    withForeignPtr ctx (\ctx -> do
      p <- unsafeUseAsCString wasm (\s ->
        extism_plugin_new ctx (castPtr s) length nullPtr 0 wasi )
      if p < 0 then do
        err <- extism_error ctx (-1)
        e <- peekCString err
        return $ Left (ExtismError e)
      else
        return $ Right (Plugin c p))
      
-- | Create a 'Plugin' with its own 'Context'
createPlugin :: B.ByteString -> Bool -> IO (Result Plugin)
createPlugin c useWasi = do
  ctx <- newContext
  plugin ctx c useWasi

-- | Create a 'Plugin' from a 'Manifest'
pluginFromManifest :: Context -> Manifest -> Bool -> IO (Result Plugin)
pluginFromManifest ctx manifest useWasi =
  let wasm = toByteString $ toString manifest in
  plugin ctx wasm useWasi

-- | Create a 'Plugin' with its own 'Context' from a 'Manifest'
createPluginFromManifest :: Manifest -> Bool -> IO (Result Plugin)
createPluginFromManifest manifest useWasi = do
  ctx <- newContext
  pluginFromManifest ctx manifest useWasi

-- | Update a 'Plugin' with a new WASM module
update :: Plugin -> B.ByteString -> Bool -> IO (Result ())
update (Plugin (Context ctx) id) wasm useWasi =
  let length = fromIntegral (B.length wasm) in
  let wasi = fromInteger (if useWasi then 1 else 0) in
  do
    withForeignPtr ctx (\ctx -> do
      b <- unsafeUseAsCString wasm (\s ->
        extism_plugin_update ctx id (castPtr s) length nullPtr 0 wasi)
      if b <= 0 then do
        err <- extism_error ctx (-1)
        e <- peekCString err
        return $ Left (ExtismError e)
      else
        return (Right ()))

-- | Update a 'Plugin' with a new 'Manifest'
updateManifest :: Plugin -> Manifest -> Bool -> IO (Result ())
updateManifest plugin manifest useWasi =
  let wasm = toByteString $ toString manifest in
  update plugin wasm useWasi

-- | Check if a 'Plugin' is valid
isValid :: Plugin -> Bool
isValid (Plugin _ p) = p >= 0

-- | Set configuration values for a plugin
setConfig :: Plugin -> [(String, Maybe String)] -> IO Bool
setConfig (Plugin (Context ctx) plugin) x =
  if plugin < 0
    then return False
  else
    let obj = toJSObject [(k, showJSON v) | (k, v) <- x] in
    let bs = toByteString (encode obj) in
    let length = fromIntegral (B.length bs) in
    unsafeUseAsCString bs (\s -> do
      withForeignPtr ctx (\ctx -> do
        b <- extism_plugin_config ctx plugin (castPtr s) length
        return $ b /= 0))

levelStr Error = "error"
levelStr Debug = "debug"
levelStr Warn = "warn"
levelStr Trace = "trace"
levelStr Info = "info"

-- | Set the log file and level, this is a global configuration
setLogFile :: String -> LogLevel -> IO Bool
setLogFile filename level =
  let s = levelStr level in
  withCString filename (\f ->
    withCString s (\l -> do
      b <- extism_log_file f l
      return $ b /= 0))

-- | Check if a function exists in the given plugin
functionExists :: Plugin -> String -> IO Bool
functionExists (Plugin (Context ctx) plugin) name = do
  withForeignPtr ctx (\ctx -> do
    b <- withCString name (extism_plugin_function_exists ctx plugin)
    if b == 1 then return True else return False)

--- | Call a function provided by the given plugin
call :: Plugin -> String -> B.ByteString -> IO (Result B.ByteString)
call (Plugin (Context ctx) plugin) name input =
  let length = fromIntegral (B.length input) in
  do
    withForeignPtr ctx (\ctx -> do
      rc <- withCString name (\name ->
        unsafeUseAsCString input (\input ->
          extism_plugin_call ctx plugin name (castPtr input) length))
      err <- extism_error ctx plugin
      if err /= nullPtr
        then do e <- peekCString err
                return $ Left (ExtismError e)
      else if rc == 0
        then do
          length <- extism_plugin_output_length ctx plugin
          ptr <- extism_plugin_output_data ctx plugin
          buf <- packCStringLen (castPtr ptr, fromIntegral length)
          return $ Right buf
      else return $ Left (ExtismError "Call failed"))

-- | Free a 'Plugin', this will automatically be called for every plugin
-- | associated with a 'Context' when that 'Context' is freed
free :: Plugin -> IO ()
free (Plugin (Context ctx) plugin) =
  withForeignPtr ctx (`extism_plugin_free` plugin)

cancelHandle :: Plugin -> IO CancelHandle
cancelHandle (Plugin (Context ctx) plugin) = do
  handle <- withForeignPtr ctx (\ctx -> extism_plugin_cancel_handle ctx plugin)
  return (CancelHandle handle)

cancel :: CancelHandle -> IO Bool
cancel (CancelHandle handle) = 
  extism_plugin_cancel handle

