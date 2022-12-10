{-# LANGUAGE ForeignFunctionInterface #-}

module Extism.Bindings where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Data.Int
import Data.Word

newtype ExtismContext = ExtismContext () deriving Show

foreign import ccall safe "extism.h extism_context_new" extism_context_new :: IO (Ptr ExtismContext)
foreign import ccall safe "extism.h &extism_context_free" extism_context_free :: FunPtr (Ptr ExtismContext -> IO ())
foreign import ccall safe "extism.h extism_plugin_new" extism_plugin_new :: Ptr ExtismContext -> Ptr Word8 -> Word64 -> CBool -> IO Int32
foreign import ccall safe "extism.h extism_plugin_update" extism_plugin_update :: Ptr ExtismContext -> Int32 -> Ptr Word8 -> Word64 -> CBool -> IO CBool
foreign import ccall safe "extism.h extism_plugin_call" extism_plugin_call :: Ptr ExtismContext -> Int32 -> CString -> Ptr Word8 -> Word64 -> IO Int32
foreign import ccall safe "extism.h extism_plugin_function_exists" extism_plugin_function_exists :: Ptr ExtismContext -> Int32 -> CString -> IO CBool
foreign import ccall safe "extism.h extism_error" extism_error :: Ptr ExtismContext -> Int32 -> IO CString
foreign import ccall safe "extism.h extism_plugin_output_length" extism_plugin_output_length :: Ptr ExtismContext -> Int32 -> IO Word64
foreign import ccall safe "extism.h extism_plugin_output_data" extism_plugin_output_data :: Ptr ExtismContext -> Int32 -> IO (Ptr Word8)
foreign import ccall safe "extism.h extism_log_file" extism_log_file :: CString -> CString -> IO CBool
foreign import ccall safe "extism.h extism_plugin_config" extism_plugin_config :: Ptr ExtismContext -> Int32 -> Ptr Word8 -> Int64 -> IO CBool
foreign import ccall safe "extism.h extism_plugin_free" extism_plugin_free :: Ptr ExtismContext -> Int32 -> IO ()
foreign import ccall safe "extism.h extism_context_reset" extism_context_reset :: Ptr ExtismContext -> IO ()
foreign import ccall safe "extism.h extism_version" extism_version :: IO CString
