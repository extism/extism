{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Extism.HostFunction(
  memoryAlloc,
  memoryLength,
  memoryFree,
  memory,
  memoryOffset,
  memoryBytes,
  memoryString,
  allocBytes,
  allocString,
  toI32,
  toI64,
  toF32,
  toF64,
  fromI32,
  fromI64,
  fromF32,
  fromF64,
  MemoryHandle
) where

import Extism
import Extism.Bindings
import Data.Word
import qualified Data.ByteString as B
import Foreign.Ptr
import Foreign.Marshal.Array
import qualified Data.ByteString.Internal as BS (c2w)

newtype MemoryHandle = MemoryHandle Word64 deriving (Num, Enum, Eq, Ord, Real, Integral)

-- | Allocate a new handle of the given size
memoryAlloc :: CurrentPlugin -> Word64 -> IO MemoryHandle
memoryAlloc p n = MemoryHandle <$> extism_current_plugin_memory_alloc p n

-- | Get the length of a handle, returns 0 if the handle is invalid
memoryLength :: CurrentPlugin -> MemoryHandle -> IO Word64
memoryLength p (MemoryHandle offs) = extism_current_plugin_memory_length p offs

-- | Free allocated memory
memoryFree :: CurrentPlugin -> MemoryHandle -> IO ()
memoryFree p (MemoryHandle offs) = extism_current_plugin_memory_free p offs

-- | Access a pointer to the entire memory region
memory :: CurrentPlugin -> IO (Ptr Word8)
memory = extism_current_plugin_memory

-- | Access the pointer for the given 'MemoryHandle'
memoryOffset :: CurrentPlugin -> MemoryHandle -> IO (Ptr Word8)
memoryOffset plugin (MemoryHandle offs) = do
  x <- extism_current_plugin_memory plugin
  return $ plusPtr x (fromIntegral offs)

-- | Access the data associated with a handle as a 'ByteString'
memoryBytes :: CurrentPlugin -> MemoryHandle ->  IO B.ByteString
memoryBytes plugin offs = do
  ptr <- memoryOffset plugin offs
  len <- memoryLength plugin offs
  arr <- peekArray (fromIntegral len) ptr
  return $ B.pack arr

  
-- | Access the data associated with a handle as a 'String'
memoryString :: CurrentPlugin -> MemoryHandle ->  IO String
memoryString plugin offs = do
  ptr <- memoryOffset plugin offs
  len <- memoryLength plugin offs
  arr <- peekArray (fromIntegral len) ptr
  return $ fromByteString $ B.pack arr

-- | Allocate memory and copy an existing 'ByteString' into it
allocBytes :: CurrentPlugin -> B.ByteString -> IO MemoryHandle
allocBytes plugin s = do
  let length = B.length s
  offs <- memoryAlloc plugin (fromIntegral length)
  ptr <- memoryOffset plugin offs
  pokeArray ptr (B.unpack s)
  return offs

  
-- | Allocate memory and copy an existing 'String' into it
allocString :: CurrentPlugin -> String -> IO MemoryHandle
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
