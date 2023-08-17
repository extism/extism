module Extism (
  module Extism.Manifest,
  ValType(..),
  Val(..),
  Function(..),
  Plugin(..),
  CancelHandle(..),
  CurrentPlugin(..),
  LogLevel(..),
  Error(..),
  Result(..),
  toByteString,
  fromByteString,
  extismVersion,
  plugin,
  pluginFromManifest,
  isValid,
  setConfig,
  setLogFile,
  functionExists,
  hostFunction,
  call,
  cancelHandle,
  cancel,
  pluginID,
  unwrap
) where

import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.StablePtr
import Foreign.Concurrent
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import qualified Text.JSON (encode, toJSObject, showJSON)
import Extism.Manifest (Manifest, toString)
import Extism.Bindings
import qualified Data.UUID (UUID, fromByteString)

-- | Host function, see 'Extism.HostFunction.hostFunction'
data Function = Function (ForeignPtr ExtismFunction) (StablePtr ())

-- | Plugins can be used to call WASM function
newtype Plugin = Plugin (ForeignPtr ExtismPlugin)

-- | Cancellation handle for Plugins
newtype CancelHandle = CancelHandle (Ptr ExtismCancelHandle)

-- | Access the plugin that is currently executing from inside a host function
type CurrentPlugin = Ptr ExtismCurrentPlugin

-- | Log level
data LogLevel = LogError | LogWarn | LogInfo | LogDebug | LogTrace deriving (Show)

-- | Extism error
newtype Error = ExtismError String deriving Show

-- | Result type
type Result a = Either Error a

-- | Helper function to convert a 'String' to a 'ByteString'
toByteString :: String -> B.ByteString
toByteString x = B.pack (map c2w x)

-- | Helper function to convert a 'ByteString' to a 'String'
fromByteString :: B.ByteString -> String
fromByteString bs = map w2c $ B.unpack bs

-- | Get the Extism version string
extismVersion :: () -> IO String
extismVersion () = do
  v <- extism_version
  peekCString v

-- | Create a 'Plugin' from a WASM module, `useWasi` determines if WASI should
-- | be linked
plugin :: B.ByteString -> [Function] -> Bool -> IO (Result Plugin)
plugin wasm functions useWasi =
  let nfunctions = fromIntegral (length functions) in
  let length = fromIntegral (B.length wasm) in
  let wasi = fromInteger (if useWasi then 1 else 0) in
  do
    funcs <- mapM (\(Function ptr _) -> withForeignPtr ptr (\x -> do return x)) functions
    alloca (\e-> do
      let errmsg = (e :: Ptr CString)
      p <- unsafeUseAsCString wasm (\s ->
        withArray funcs (\funcs ->
          extism_plugin_new (castPtr s) length funcs nfunctions wasi errmsg ))
      if p == nullPtr then do
        err <- peek errmsg
        e <- peekCString err 
        extism_plugin_new_error_free err
        return $ Left (ExtismError e)
      else do
        ptr <- Foreign.Concurrent.newForeignPtr p (extism_plugin_free p)
        return $ Right (Plugin ptr))

-- | Create a 'Plugin' from a 'Manifest'
pluginFromManifest :: Manifest -> [Function] -> Bool -> IO (Result Plugin)
pluginFromManifest manifest functions useWasi =
  let wasm = toByteString $ toString manifest in
  plugin wasm functions useWasi

-- | Check if a 'Plugin' is valid
isValid :: Plugin -> IO Bool
isValid (Plugin p) = withForeignPtr p (\x -> return (x /= nullPtr))

-- | Set configuration values for a plugin
setConfig :: Plugin -> [(String, Maybe String)] -> IO Bool
setConfig (Plugin plugin) x =
  let obj = Text.JSON.toJSObject [(k, Text.JSON.showJSON v) | (k, v) <- x] in
  let bs = toByteString (Text.JSON.encode obj) in
  let length = fromIntegral (B.length bs) in
  unsafeUseAsCString bs (\s -> do
    withForeignPtr plugin (\plugin-> do
      b <- extism_plugin_config plugin (castPtr s) length
      return $ b /= 0))

levelStr LogError = "error"
levelStr LogDebug = "debug"
levelStr LogWarn = "warn"
levelStr LogTrace = "trace"
levelStr LogInfo = "info"

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
functionExists (Plugin plugin) name = do
  withForeignPtr plugin (\plugin -> do
    b <- withCString name (extism_plugin_function_exists plugin)
    if b == 1 then return True else return False)

--- | Call a function provided by the given plugin
call :: Plugin -> String -> B.ByteString -> IO (Result B.ByteString)
call (Plugin plugin) name input =
  let length = fromIntegral (B.length input) in
  do
    withForeignPtr plugin (\plugin -> do
      rc <- withCString name (\name ->
        unsafeUseAsCString input (\input ->
          extism_plugin_call plugin name (castPtr input) length))
      err <- extism_error plugin
      if err /= nullPtr
        then do e <- peekCString err
                return $ Left (ExtismError e)
      else if rc == 0
        then do
          length <- extism_plugin_output_length plugin
          ptr <- extism_plugin_output_data plugin
          buf <- B.packCStringLen (castPtr ptr, fromIntegral length)
          return $ Right buf
      else return $ Left (ExtismError "Call failed"))

-- | Call a function with a string argument and return a string
callString :: Plugin -> String -> String -> IO (Result String)
callString p name input = do
  res <- call p name (toByteString input)
  case res of 
    Left x -> return $ Left x
    Right x -> return $ Right (fromByteString x)


-- | Create a new 'CancelHandle' that can be used to cancel a running plugin
-- | from another thread.
cancelHandle :: Plugin -> IO CancelHandle
cancelHandle (Plugin plugin) = do
  handle <- withForeignPtr plugin extism_plugin_cancel_handle
  return (CancelHandle handle)

-- | Cancel a running plugin using a 'CancelHandle'
cancel :: CancelHandle -> IO Bool
cancel (CancelHandle handle) =
  extism_plugin_cancel handle


-- | Create a new 'Function' that can be called from a 'Plugin'
hostFunction :: String -> [ValType] -> [ValType] -> (CurrentPlugin -> [Val] -> a -> IO [Val]) -> a -> IO Function
hostFunction name params results f v =
  let nparams = fromIntegral $ length params in
  let nresults = fromIntegral $ length results in
  do
    cb <- callbackWrap (callback f :: CCallback)
    free <- freePtrWrap freePtr
    userData <- newStablePtr (v, free, cb)
    let userDataPtr = castStablePtrToPtr userData
    x <- withCString name (\name ->  do
      withArray params (\params ->
        withArray results (\results -> do
          extism_function_new name params nparams results nresults cb userDataPtr free)))
    let freeFn = extism_function_free x
    fptr <- Foreign.Concurrent.newForeignPtr x freeFn
    return $ Function fptr (castPtrToStablePtr userDataPtr)


pluginID :: Plugin -> IO Data.UUID.UUID
pluginID (Plugin plugin) =
  withForeignPtr plugin (\plugin -> do
    ptr <- extism_plugin_id plugin
    buf <- B.packCStringLen (castPtr ptr, 16)
    case Data.UUID.fromByteString (BL.fromStrict buf) of
      Nothing -> error "Invalid Plugin ID" 
      Just x -> return x)

    
unwrap (Right x) = x
unwrap (Left (ExtismError msg)) = do
  error msg
