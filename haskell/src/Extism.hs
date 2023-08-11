module Extism (
  module Extism,
  module Extism.Manifest,
  ValType(..),
  Val(..)
) where

import Data.Int
import Data.Word
import Control.Monad (void)
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.StablePtr
import Foreign.Concurrent
import Foreign.Marshal.Utils (copyBytes, moveBytes)
import Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Bifunctor (second)
import Text.JSON (encode, toJSObject, showJSON)
import Extism.Manifest (Manifest, toString)
import Extism.Bindings

-- | Host function
data Function = Function (ForeignPtr ExtismFunction) (StablePtr ())

-- | Plugins can be used to call WASM function
data Plugin = Plugin (ForeignPtr ExtismPlugin)

-- | Cancellation handle for Plugins
newtype CancelHandle = CancelHandle (Ptr ExtismCancelHandle)

-- | Access the plugin that is currently executing from inside a host function
type CurrentPlugin = Ptr ExtismCurrentPlugin

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

-- | Create a 'Plugin' from a WASM module, `useWasi` determines if WASI should
-- | be linked
plugin :: B.ByteString -> [Function] -> Bool -> IO (Result Plugin)
plugin wasm functions useWasi =
  let nfunctions = fromIntegral (Prelude.length functions) in
  let length = fromIntegral (B.length wasm) in
  let wasi = fromInteger (if useWasi then 1 else 0) in
  do
    funcs <- Prelude.mapM (\(Function ptr _) -> withForeignPtr ptr (\x -> do return x)) functions
    alloca (\e-> do
      let errmsg = (e :: Ptr CString)
      p <- unsafeUseAsCString wasm (\s ->
        withArray funcs (\funcs ->
          extism_plugin_new (castPtr s) length funcs nfunctions wasi errmsg ))
      if p == nullPtr then do
        err <- peek errmsg
        e <- peekCString err 
        extism_plugin_error_free err
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
isValid (Plugin p) = withForeignPtr p (\x -> return $ (x /= nullPtr))

-- | Set configuration values for a plugin
setConfig :: Plugin -> [(String, Maybe String)] -> IO Bool
setConfig (Plugin plugin) x =
  let obj = toJSObject [(k, showJSON v) | (k, v) <- x] in
  let bs = toByteString (encode obj) in
  let length = fromIntegral (B.length bs) in
  unsafeUseAsCString bs (\s -> do
    withForeignPtr plugin (\plugin-> do
      b <- extism_plugin_config plugin (castPtr s) length
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
          buf <- packCStringLen (castPtr ptr, fromIntegral length)
          return $ Right buf
      else return $ Left (ExtismError "Call failed"))

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
  let nparams = fromIntegral $ Prelude.length params in
  let nresults = fromIntegral $ Prelude.length results in
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


-- | Create a new I32 'Val'
toI32 :: Integral a => a -> Val
toI32 x = ValI32 (fromIntegral x)

-- | Create a new I64 'Val'
toI64 :: Integral a => a -> Val
toI64 x = ValI64 (fromIntegral x)

-- | Create a new F32 'Val'
toF32 :: Float -> Val
toF32 = ValF32

-- | Create a new F64 'Val'
toF64 :: Double -> Val
toF64 = ValF64

-- | Get I32 'Val'
fromI32 :: Integral a => Val -> Maybe a
fromI32 (ValI32 x) = Just (fromIntegral x)
fromI32 _ = Nothing

-- | Get I64 'Val'
fromI64 :: Integral a => Val -> Maybe a
fromI64 (ValI64 x) = Just (fromIntegral x)
fromI64 _ = Nothing

-- | Get F32 'Val'
fromF32 :: Val -> Maybe Float
fromF32 (ValF32 x) = Just x
fromF32 _ = Nothing

-- | Get F64 'Val'
fromF64 :: Val -> Maybe Double
fromF64 (ValF64 x) = Just x
fromF64 _ = Nothing