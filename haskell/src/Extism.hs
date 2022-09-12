{-# LANGUAGE ForeignFunctionInterface #-}

module Extism (module Extism, module Extism.Manifest) where
import GHC.Int
import GHC.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Control.Monad (void)
import Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Text.JSON (JSON, toJSObject, encode)
import Extism.Manifest (Manifest, toString)

foreign import ccall unsafe "extism.h extism_plugin_register" extism_plugin_register :: Ptr Word8 -> Word64 -> CBool -> IO Int32
foreign import ccall unsafe "extism.h extism_plugin_update" extism_plugin_update :: Int32 -> Ptr Word8 -> Word64 -> CBool -> IO CBool
foreign import ccall unsafe "extism.h extism_call" extism_call :: Int32 -> CString -> Ptr Word8 -> Word64 -> IO Int32
foreign import ccall unsafe "extism.h extism_function_exists" extism_function_exists :: Int32 -> CString -> IO CBool
foreign import ccall unsafe "extism.h extism_error" extism_error :: Int32 -> IO CString
foreign import ccall unsafe "extism.h extism_output_length" extism_output_length :: Int32 -> IO Word64
foreign import ccall unsafe "extism.h extism_output_get" extism_output_get :: Int32 -> IO (Ptr Word8)
foreign import ccall unsafe "extism.h extism_log_file" extism_log_file :: CString -> CString -> IO CBool
foreign import ccall unsafe "extism.h extism_plugin_config" extism_plugin_config :: Int32 -> Ptr Word8 -> Int64 -> IO CBool

newtype Plugin = Plugin Int32 deriving Show
newtype Error = Error String deriving Show

toByteString :: String -> ByteString
toByteString x = B.pack (Prelude.map c2w x)

fromByteString :: ByteString -> String
fromByteString bs = Prelude.map w2c $ B.unpack bs

register :: B.ByteString -> Bool -> IO Plugin
register wasm useWasi =
  let length = fromIntegral (B.length wasm) in
  let wasi = fromInteger (if useWasi then 1 else 0) in
  do
    p <- unsafeUseAsCString wasm (\s ->
      extism_plugin_register (castPtr s) length wasi)
    return $ Plugin p

registerManifest :: Manifest -> Bool -> IO Plugin
registerManifest manifest useWasi =
  let wasm = toByteString $ toString manifest in
  register wasm useWasi

update :: Plugin -> B.ByteString -> Bool -> IO Bool
update (Plugin id) wasm useWasi =
  let length = fromIntegral (B.length wasm) in
  let wasi = fromInteger (if useWasi then 1 else 0) in
  do
    b <- unsafeUseAsCString wasm (\s ->
      extism_plugin_update id (castPtr s) length wasi)
    return (b > 0)

updateManifest :: Plugin -> Manifest -> Bool -> IO Bool
updateManifest plugin manifest useWasi =
  let wasm = toByteString $ toString manifest in
  update plugin wasm useWasi

isValid :: Plugin -> Bool
isValid (Plugin p) = p >= 0

setConfig :: Plugin -> [(String, String)] -> IO ()
setConfig (Plugin plugin) x =
  if plugin < 0
    then return ()
  else
    let obj = toJSObject x in
    let bs = toByteString (encode obj) in
    let length = fromIntegral (B.length bs) in
    unsafeUseAsCString bs (\s -> do
      void $ extism_plugin_config plugin (castPtr s) length)

setLogFile :: String -> String -> IO ()
setLogFile filename level =
  withCString filename (\f ->
    withCString level (\l -> do
      void $ extism_log_file f l))

functionExists :: Plugin -> String -> IO Bool
functionExists (Plugin plugin) name = do
  b <- withCString name (extism_function_exists plugin)
  if b == 1 then return True else return False

call :: Plugin -> String -> B.ByteString -> IO (Either B.ByteString Error)
call (Plugin plugin) name input =
  let length = fromIntegral (B.length input) in
  do
    rc <- withCString name (\name ->
      unsafeUseAsCString input (\input ->
        extism_call plugin name (castPtr input) length))
    err <- extism_error plugin
    if err /= nullPtr
      then do e <- peekCString err
              return $ Right (Error e)
    else if rc == 0
      then do
        length <- extism_output_length plugin
        ptr <- extism_output_get plugin
        buf <- packCStringLen (castPtr ptr, fromIntegral length)
        return $ Left buf
    else return $ Right (Error "Call failed")
