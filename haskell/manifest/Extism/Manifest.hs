{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Extism.Manifest where

import Text.JSON
  (
    JSON,
    JSValue(JSNull, JSString, JSArray),
    toJSString, showJSON, makeObj, encode
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS (unpack)

makeArray x = JSArray [toJSONValue a | a <- x]
isNull JSNull = True
isNull _ = False
filterNulls obj = [(a, b) | (a, b) <- obj, not (isNull b)]
object x = makeObj $ filterNulls x
(.=) a b = (a, toJSONValue b)

newtype Memory = Memory
  {
    memoryMax :: Maybe Int
  }

class JSONValue a where
  toJSONValue :: a -> JSValue
  
instance {-# OVERLAPS #-} (JSON a) => (JSONValue a) where
  toJSONValue = showJSON

instance {-# OVERLAPS #-} (JSONValue a) => (JSONValue (Maybe a)) where
  toJSONValue Nothing = JSNull
  toJSONValue (Just x) = toJSONValue x

instance JSONValue Memory where
  toJSONValue (Memory max) =
    object [
      "max" .= max
    ]

data HTTPRequest = HTTPRequest
  {
    url :: String
  , headers :: Maybe [(String, String)]
  , method :: Maybe String
  }

requestObj (HTTPRequest url headers method) =
  [
    "url" .= url ,
    "headers" .= headers,
    "method" .= method
  ]

instance JSONValue HTTPRequest where
  toJSONValue x =
    object $ requestObj x

data WasmFile = WasmFile
  {
    filePath :: String
  , fileName :: Maybe String
  , fileHash :: Maybe String
  }

instance JSONValue WasmFile where
  toJSONValue (WasmFile path name hash) =
    object [
      "path" .= path,
      "name" .= name,
      "hash" .= hash
    ]

data WasmCode = WasmCode
  {
    codeBytes :: B.ByteString
  , codeName :: Maybe String
  , codeHash :: Maybe String
  }


instance JSONValue WasmCode where
  toJSONValue (WasmCode x name hash) =
    let bytes = BS.unpack $ B64.encode x in
    object [
      "data" .= bytes,
      "name" .= name,
      "hash" .= hash
    ]

data WasmURL = WasmURL
  {
    req :: HTTPRequest
  , urlName :: Maybe String
  , urlHash :: Maybe String
  }


instance JSONValue WasmURL where
  toJSONValue (WasmURL req name hash) =
    let request = requestObj req in
    object $ "name" .= name : "hash" .= hash : request

data Wasm = File WasmFile | Code WasmCode | URL WasmURL

instance JSONValue Wasm where
  toJSONValue x =
    case x of
      File f -> toJSONValue f
      Code d -> toJSONValue d
      URL u -> toJSONValue u

wasmFile :: String -> Wasm
wasmFile path =
  File WasmFile { filePath = path, fileName = Nothing, fileHash = Nothing}

wasmURL :: String -> String -> Wasm
wasmURL method url =
  let r = HTTPRequest { url = url, headers = Nothing, method = Just method } in
  URL WasmURL { req = r, urlName = Nothing, urlHash = Nothing }

wasmCode :: B.ByteString -> Wasm
wasmCode code =
  Code WasmCode { codeBytes = code, codeName = Nothing, codeHash = Nothing }

withName :: Wasm -> String -> Wasm
withName (Code code) name = Code code { codeName = Just name }
withName (URL url) name =  URL url { urlName = Just name }
withName (File f) name = File  f { fileName = Just name }


withHash :: Wasm -> String -> Wasm
withHash (Code code) hash = Code code { codeHash = Just hash }
withHash (URL url) hash =  URL url { urlHash = Just hash }
withHash (File f) hash = File  f { fileHash = Just hash }

data Manifest = Manifest
  {
    wasm :: [Wasm]
  , memory :: Maybe Memory
  , config :: Maybe [(String, String)]
  , allowedHosts :: Maybe [String]
  }

manifest :: [Wasm] -> Manifest
manifest wasm =
  Manifest {
    wasm = wasm,
    memory = Nothing,
    config = Nothing,
    allowedHosts = Nothing
  }

withConfig :: Manifest -> [(String, String)] -> Manifest
withConfig m config =
  m { config = Just config }


withHosts :: Manifest -> [String] -> Manifest
withHosts m hosts =
  m { allowedHosts = Just hosts }

instance JSONValue Manifest where
  toJSONValue (Manifest wasm memory config hosts) =
    let w = makeArray wasm in
    object [
      "wasm" .= w,
      "memory" .= memory,
      "config" .= config,
      "allowed_hosts" .= hosts
    ]

toString :: (JSONValue a) => a -> String
toString v =
  encode (toJSONValue v)
