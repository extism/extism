{-# LANGUAGE ForeignFunctionInterface #-}

module Extism.Bindings where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Data.Int
import Data.Word
import Foreign.Storable

type FreeCallback = Ptr () -> IO ()

newtype ExtismContext = ExtismContext () deriving Show
newtype ExtismFunction = ExtismFunction () deriving Show
newtype ExtismCancelHandle = ExtismCancelHandle () deriving Show
newtype ExtismCurrentPlugin = ExtismCurrentPlugin () deriving Show
data ValType = I32 | I64 | F32 | F64 | V128 | FuncRef | ExternRef deriving (Show, Eq)
data Val = ValI32 Int32 | ValI64 Int64 | ValF32 Float | ValF64 Double deriving (Show, Eq)

typeOfVal (ValI32 _) = I32
typeOfVal (ValI64 _) = I64
typeOfVal (ValF32 _) = F32
typeOfVal (ValF64 _) = F64

type CCallback = Ptr ExtismCurrentPlugin -> Ptr Val -> Word64 -> Ptr Val -> Word64 -> Ptr () -> IO ()

instance Storable Val where
  sizeOf _ =  16
  alignment _ = 1
  peek ptr = do
    t <- valTypeOfInt <$> peekByteOff ptr 0
    v <- case t of
         I32 -> ValI32 <$> (peekByteOff ptr 8)
         I64 -> ValI64 <$> (peekByteOff ptr 8)
         F32 -> ValF32 <$> (peekByteOff ptr 8)
         F64 -> ValF64 <$> (peekByteOff ptr 8)
    return $ v
  poke ptr x = do
    pokeByteOff ptr 0 (typeOfVal x)
    case x of
      ValI32 x -> pokeByteOff ptr 8 x
      ValI64 x -> pokeByteOff ptr 8 x
      ValF32 x -> pokeByteOff ptr 8 x
      ValF64 x -> pokeByteOff ptr 8 x


intOfValType :: ValType -> CInt
intOfValType I32 = 0
intOfValType I64 = 1
intOfValType F32 = 2
intOfValType F64 = 3
intOfValType V128 = 4
intOfValType FuncRef = 5
intOfValType ExternRef = 6

valTypeOfInt :: CInt -> ValType
valTypeOfInt 0 = I32
valTypeOfInt 1 = I64
valTypeOfInt 2 = F32
valTypeOfInt 3 = F64
valTypeOfInt 4 = V128
valTypeOfInt 5 = FuncRef
valTypeOfInt 6 = ExternRef
valTypeOfInt _ = error "Invalid ValType"

instance Storable ValType where
  sizeOf _ = 4
  alignment _ = 1
  peek ptr = do
    x <- peekByteOff ptr 0
    return $ valTypeOfInt (x :: CInt)
  poke ptr x = do
    pokeByteOff ptr 0 (intOfValType x)

foreign import ccall safe "extism.h extism_context_new" extism_context_new :: IO (Ptr ExtismContext)
foreign import ccall safe "extism.h &extism_context_free" extism_context_free :: FunPtr (Ptr ExtismContext -> IO ())
foreign import ccall safe "extism.h extism_plugin_new" extism_plugin_new :: Ptr ExtismContext -> Ptr Word8 -> Word64 -> Ptr (Ptr ExtismFunction) -> Word64 -> CBool -> IO Int32
foreign import ccall safe "extism.h extism_plugin_update" extism_plugin_update :: Ptr ExtismContext -> Int32 -> Ptr Word8 -> Word64 -> Ptr (Ptr ExtismFunction) -> Word64 -> CBool -> IO CBool
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
foreign import ccall safe "extism.h extism_plugin_cancel_handle" extism_plugin_cancel_handle :: Ptr ExtismContext -> Int32 -> IO (Ptr ExtismCancelHandle)
foreign import ccall safe "extism.h extism_plugin_cancel" extism_plugin_cancel :: Ptr ExtismCancelHandle -> IO Bool

foreign import ccall safe "extism.h extism_function_new" extism_function_new :: CString -> Ptr ValType -> Word64 -> Ptr ValType -> Word64 -> FunPtr CCallback -> Ptr () -> FunPtr FreeCallback -> IO (Ptr ExtismFunction)
foreign import ccall safe "extism.h extism_function_free" extism_function_free :: Ptr ExtismFunction -> IO ()
foreign import ccall safe "extism.h extism_current_plugin_memory" extism_current_plugin_memory :: Ptr ExtismCurrentPlugin -> IO (Ptr Word8)
foreign import ccall safe "extism.h extism_current_plugin_memory_alloc" extism_current_plugin_memory_alloc :: Ptr ExtismCurrentPlugin -> Word64 -> IO Word64
foreign import ccall safe "extism.h extism_current_plugin_memory_length" extism_current_plugin_memory_length :: Ptr ExtismCurrentPlugin -> Word64 -> IO Word64
foreign import ccall safe "extism.h extism_current_plugin_memory_free" extism_current_plugin_memory_free :: Ptr ExtismCurrentPlugin -> Word64 -> IO ()

