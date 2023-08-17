module Extism.HostFunction where

import Extism
import Extism.Bindings
import Data.Word
import Data.ByteString as B
import Foreign.Ptr
import Foreign.Marshal.Array
import qualified Data.ByteString.Internal as BS (c2w)


-- | Allocate a new handle of the given size
memoryAlloc :: CurrentPlugin -> Word64 -> IO Word64
memoryAlloc = extism_current_plugin_memory_alloc

-- | Get the length of a handle, returns 0 if the handle is invalid
memoryLength :: CurrentPlugin -> Word64 -> IO Word64
memoryLength = extism_current_plugin_memory_length

-- | Free allocated memory
memoryFree :: CurrentPlugin -> Word64 -> IO ()
memoryFree = extism_current_plugin_memory_free

-- | Access a pointer to the entire memory region
memory :: CurrentPlugin -> IO (Ptr Word8)
memory = extism_current_plugin_memory

-- | Access a pointer the a specific offset in memory
memoryOffset :: CurrentPlugin -> Word64 -> IO (Ptr Word8)
memoryOffset plugin offs = do
  x <- extism_current_plugin_memory plugin
  return $ plusPtr x (fromIntegral offs)

-- | Access the data associated with a handle as a 'ByteString'
memoryBytes :: CurrentPlugin -> Word64 ->  IO B.ByteString
memoryBytes plugin offs = do
  ptr <- memoryOffset plugin offs
  len <- memoryLength plugin offs
  arr <- peekArray (fromIntegral len) ptr
  return $ B.pack arr

  
-- | Access the data associated with a handle as a 'String'
memoryString :: CurrentPlugin -> Word64 ->  IO String
memoryString plugin offs = do
  ptr <- memoryOffset plugin offs
  len <- memoryLength plugin offs
  arr <- peekArray (fromIntegral len) ptr
  return $ fromByteString $ B.pack arr

-- | Allocate memory and copy an existing 'ByteString' into it
allocBytes :: CurrentPlugin -> B.ByteString -> IO Word64
allocBytes plugin s = do
  let length = B.length s
  offs <- memoryAlloc plugin (fromIntegral length)
  ptr <- memoryOffset plugin offs
  pokeArray ptr (B.unpack s)
  return offs

  
-- | Allocate memory and copy an existing 'String' into it
allocString :: CurrentPlugin -> String -> IO Word64
allocString plugin s = do
  let length = Prelude.length s
  offs <- memoryAlloc plugin (fromIntegral length)
  ptr <- memoryOffset plugin offs
  pokeArray ptr (Prelude.map BS.c2w s)
  return offs

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
