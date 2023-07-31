module Extism.CurrentPlugin where

import Extism
import Extism.Bindings
import Data.Word
import Data.ByteString as B
import Foreign.Ptr
import Foreign.Marshal.Array

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

-- | Allocate memory and copy an existing 'ByteString' into it
allocBytes :: CurrentPlugin -> B.ByteString -> IO Word64
allocBytes plugin s = do
  let length = B.length s
  offs <- memoryAlloc plugin (fromIntegral length)
  ptr <- memoryOffset plugin offs
  pokeArray ptr (B.unpack s)
  return offs

