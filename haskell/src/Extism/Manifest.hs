module Extism.Manifest where

import Text.JSON
  (
    JSValue(JSNull, JSString, JSArray),
    toJSString, showJSON, makeObj, encode
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS (unpack)

valueOrNull f Nothing = JSNull
valueOrNull f (Just x) = f x
makeString s = JSString (toJSString s)
stringOrNull = valueOrNull makeString
makeArray f [] = JSNull
makeArray f x = JSArray [f a | a <- x]
filterNulls obj = [(a, b) | (a, b) <- obj, not (isNull b)]
mapObj f x = makeObj (filterNulls [(a, f b) | (a, b) <- x])
isNull JSNull = True
isNull _ = False

newtype Memory = Memory
  {
    memoryMax :: Maybe Int
  }

class JSONValue a where
  toJSONValue :: a -> JSValue

instance JSONValue Memory where
  toJSONValue x =
    case memoryMax x of
      Nothing -> makeObj []
      Just max -> makeObj [("max", showJSON max)]

data HttpRequest = HttpRequest
  {
    url :: String
  , header :: [(String, String)]
  , method :: Maybe String
  }

requestObj x =
  let meth = stringOrNull $ method x in
  let h = mapObj makeString $ header x in
  filterNulls [
    ("url", makeString $ url x),
    ("header", h),
    ("method", meth)
  ]

instance JSONValue HttpRequest where
  toJSONValue x =
    makeObj $ requestObj x

data WasmFile = WasmFile
  {
    filePath :: String
  , fileName :: Maybe String
  , fileHash :: Maybe String
  }

instance JSONValue WasmFile where
  toJSONValue x =
    let path = makeString $ filePath x in
    let name = stringOrNull $ fileName x in
    let hash = stringOrNull $ fileHash x in
    makeObj $ filterNulls [
      ("path", path),
      ("name", name),
      ("hash", hash)
    ]

data WasmCode = WasmCode
  {
    codeBytes :: B.ByteString
  , codeName :: Maybe String
  , codeHash :: Maybe String
  }


instance JSONValue WasmCode where
  toJSONValue x =
    let bytes = makeString $ BS.unpack $ B64.encode $ codeBytes x in
    let name = stringOrNull $ codeName x in
    let hash = stringOrNull $ codeHash x in
    makeObj $ filterNulls [
      ("data", bytes),
      ("name", name),
      ("hash", hash)
    ]

data WasmURL = WasmURL
  {
    req :: HttpRequest
  , urlName :: Maybe String
  , urlHash :: Maybe String
  }


instance JSONValue WasmURL where
  toJSONValue x =
    let request = requestObj $ req x in
    let name = stringOrNull $ urlName x in
    let hash = stringOrNull $ urlHash x in
    makeObj $ filterNulls $ ("name", name) : ("hash", hash) : request

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
  let r = HttpRequest { url = url, header = [], method = Just method } in
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
  , config :: [(String, String)]
  , allowed_hosts :: [String]
  }

manifest :: [Wasm] -> Manifest
manifest wasm =
  Manifest {
    wasm = wasm,
    memory = Nothing,
    config = [],
    allowed_hosts = []
  }

withConfig :: Manifest -> [(String, String)] -> Manifest
withConfig m config =
  m { config = config }


withHosts :: Manifest -> [String] -> Manifest
withHosts m hosts =
  m { allowed_hosts = hosts }

instance JSONValue Manifest where
  toJSONValue x =
    let w = makeArray toJSONValue $ wasm x in
    let mem = valueOrNull toJSONValue $ memory x in
    let c = mapObj makeString $ config x in
    let hosts = makeArray makeString $ allowed_hosts x in
    makeObj $ filterNulls [
      ("wasm", w),
      ("memory", mem),
      ("config", c),
      ("allowed_hosts", hosts)
    ]

toString :: Manifest -> String
toString manifest =
  encode (toJSONValue manifest)
