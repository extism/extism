{-# LANGUAGE ForeignFunctionInterface #-}

module Extism (module Extism, module Extism.Manifest) where
import GHC.Int
import GHC.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Control.Monad (void)
import Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Text.JSON (JSON, toJSObject, encode)
import Extism.Manifest (Manifest, toString)

newtype ExtismContext = ExtismContext () deriving Show

foreign import ccall unsafe "extism.h extism_context_new" extism_context_new :: IO (Ptr ExtismContext)
foreign import ccall unsafe "extism.h &extism_context_free" extism_context_free :: FunPtr (Ptr ExtismContext -> IO ())
foreign import ccall unsafe "extism.h extism_plugin_new" extism_plugin_new :: Ptr ExtismContext -> Ptr Word8 -> Word64 -> CBool -> IO Int32
foreign import ccall unsafe "extism.h extism_plugin_update" extism_plugin_update :: Ptr ExtismContext -> Int32 -> Ptr Word8 -> Word64 -> CBool -> IO CBool
foreign import ccall unsafe "extism.h extism_plugin_call" extism_plugin_call :: Ptr ExtismContext -> Int32 -> CString -> Ptr Word8 -> Word64 -> IO Int32
foreign import ccall unsafe "extism.h extism_plugin_function_exists" extism_plugin_function_exists :: Ptr ExtismContext -> Int32 -> CString -> IO CBool
foreign import ccall unsafe "extism.h extism_error" extism_error :: Ptr ExtismContext -> Int32 -> IO CString
foreign import ccall unsafe "extism.h extism_plugin_output_length" extism_plugin_output_length :: Ptr ExtismContext -> Int32 -> IO Word64
foreign import ccall unsafe "extism.h extism_plugin_output_data" extism_plugin_output_data :: Ptr ExtismContext -> Int32 -> IO (Ptr Word8)
foreign import ccall unsafe "extism.h extism_log_file" extism_log_file :: CString -> CString -> IO CBool
foreign import ccall unsafe "extism.h extism_plugin_config" extism_plugin_config :: Ptr ExtismContext -> Int32 -> Ptr Word8 -> Int64 -> IO CBool
foreign import ccall unsafe "extism.h extism_plugin_free" extism_plugin_free :: Ptr ExtismContext -> Int32 -> IO ()
foreign import ccall unsafe "extism.h extism_context_reset" extism_context_reset :: Ptr ExtismContext -> IO ()

-- Context manages plugins
newtype Context = Context (ForeignPtr ExtismContext)

-- Plugins can be used to call WASM function
data Plugin = Plugin Context Int32

-- Extism error
newtype Error = Error String deriving Show

-- Helper function to convert a string to a bytestring
toByteString :: String -> ByteString
toByteString x = B.pack (Prelude.map c2w x)

-- Helper function to convert a bytestring to a string
fromByteString :: ByteString -> String
fromByteString bs = Prelude.map w2c $ B.unpack bs

-- Remove all registered plugins in a Context
reset :: Context -> IO ()
reset (Context ctx) =
  withForeignPtr ctx (\ctx ->
    extism_context_reset ctx)

-- Create a new context
newContext :: () -> IO Context
newContext () = do
  ptr <- extism_context_new
  fptr <- newForeignPtr extism_context_free ptr
  return (Context fptr)

-- Create a plugin from a WASM module, `useWasi` determines if WASI should
-- be linked
plugin :: Context -> B.ByteString -> Bool -> IO (Either Error Plugin)
plugin c wasm useWasi =
  let length = fromIntegral (B.length wasm) in
  let wasi = fromInteger (if useWasi then 1 else 0) in
  let Context ctx = c in
  do
    withForeignPtr ctx (\ctx -> do
      p <- unsafeUseAsCString wasm (\s ->
        extism_plugin_new ctx (castPtr s) length wasi)
      if p < 0 then do
        err <- extism_error ctx (-1)
        e <- peekCString err
        return $ Left (Error e)
      else
        return $ Right (Plugin c p))

-- Create a plugin from a Manifest
pluginFromManifest :: Context -> Manifest -> Bool -> IO (Either Error Plugin)
pluginFromManifest ctx manifest useWasi =
  let wasm = toByteString $ toString manifest in
  plugin ctx wasm useWasi

-- Update a plugin with a new WASM module
update :: Plugin -> B.ByteString -> Bool -> IO (Either Error ())
update (Plugin (Context ctx) id) wasm useWasi =
  let length = fromIntegral (B.length wasm) in
  let wasi = fromInteger (if useWasi then 1 else 0) in
  do
    withForeignPtr ctx (\ctx -> do
      b <- unsafeUseAsCString wasm (\s ->
        extism_plugin_update ctx id (castPtr s) length wasi)
      if b <= 0 then do
        err <- extism_error ctx (-1)
        e <- peekCString err
        return $ Left (Error e)
      else
        return (Right ()))

-- Update a plugin with a new Manifest
updateManifest :: Plugin -> Manifest -> Bool -> IO (Either Error ())
updateManifest plugin manifest useWasi =
  let wasm = toByteString $ toString manifest in
  update plugin wasm useWasi

-- Check if a plugin is value
isValid :: Plugin -> Bool
isValid (Plugin _ p) = p >= 0

-- Set configuration values for a plugin
setConfig :: Plugin -> [(String, Maybe String)] -> IO ()
setConfig (Plugin (Context ctx) plugin) x =
  if plugin < 0
    then return ()
  else
    let obj = toJSObject x in
    let bs = toByteString (encode obj) in
    let length = fromIntegral (B.length bs) in
    unsafeUseAsCString bs (\s -> do
      withForeignPtr ctx (\ctx ->
        void $ extism_plugin_config ctx plugin (castPtr s) length))

-- Set the log file and level, this is a global configuration
setLogFile :: String -> String -> IO ()
setLogFile filename level =
  withCString filename (\f ->
    withCString level (\l -> do
      void $ extism_log_file f l))

-- Check if a function exists in the given plugin
functionExists :: Plugin -> String -> IO Bool
functionExists (Plugin (Context ctx) plugin) name = do
  withForeignPtr ctx (\ctx -> do
    b <- withCString name (extism_plugin_function_exists ctx plugin)
    if b == 1 then return True else return False)

--- Call a function provided by the given plugin
call :: Plugin -> String -> B.ByteString -> IO (Either Error B.ByteString)
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
                return $ Left (Error e)
      else if rc == 0
        then do
          length <- extism_plugin_output_length ctx plugin
          ptr <- extism_plugin_output_data ctx plugin
          buf <- packCStringLen (castPtr ptr, fromIntegral length)
          return $ Right buf
      else return $ Left (Error "Call failed"))

-- Free a plugin
free :: Plugin -> IO ()
free (Plugin (Context ctx) plugin) =
  withForeignPtr ctx (\ctx ->
    extism_plugin_free ctx plugin)
