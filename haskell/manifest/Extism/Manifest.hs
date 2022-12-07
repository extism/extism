{-# LANGUAGE FlexibleInstances, UndecidableInstances, DeriveDataTypeable, DeriveAnyClass #-}

module Extism.Manifest(module Extism.Manifest, module Text.JSON) where

import Text.JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS (unpack)

data Nullable a = Null | NotNull a

makeArray x = JSArray [showJSON a | a <- x]
isNull JSNull = True
isNull _ = False
filterNulls obj = [(a, b) | (a, b) <- obj, not (isNull b)]
object x = makeObj $ filterNulls x
nonNull x = NotNull x
null' = Null
(.=) a b = (a, showJSON b)
toNullable (Just x) = NotNull x
toNullable Nothing = Null
fromNullable (NotNull x) = Just x
fromNullable Null = Nothing

(.?) a k =
  case valFromObj k a of
    Ok x -> NotNull x
    Error _ -> Null
(.??) a k = toNullable $ lookup k a

instance JSON a => JSON (Nullable a) where
  showJSON (NotNull x) = showJSON x
  showJSON Null = JSNull
  readJSON JSNull = Ok Null
  readJSON x = readJSON x

-- | Memory options
newtype Memory = Memory
  {
    memoryMaxPages :: Nullable Int
  }

instance JSON Memory where
  showJSON (Memory max) =
    object [
      "max_pages" .= max
    ]
  readJSON (JSObject obj) =
    valFromObj "max_pages" obj

-- | HTTP request
data HTTPRequest = HTTPRequest
  {
    url :: String
  , headers :: Nullable [(String, String)]
  , method :: Nullable String
  }

requestObj (HTTPRequest url headers method) =
  [
    "url" .= url,
    "headers" .= headers,
    "method" .= method
  ]

instance JSON HTTPRequest where
  showJSON req =  object $ requestObj req
  readJSON (JSObject x) =
    let url = x .? "url" in
    let headers =  x .? "headers" in
    let method =  x .? "method" in
    case url of
      Null -> Error "Missing 'url' field"
      NotNull url -> Ok (HTTPRequest url headers method)


-- | WASM from file
data WasmFile = WasmFile
  {
    filePath :: String
  , fileName :: Nullable String
  , fileHash :: Nullable String
  }

instance JSON WasmFile where
  showJSON (WasmFile path name hash) =
    object [
      "path" .= path,
      "name" .= name,
      "hash" .= hash
    ]
  readJSON (JSObject x) =
    let path = x .? "url" in
    let name = x .? "name" in
    let hash = x .? "hash" in
    case path of
      Null -> Error "Missing 'path' field"
      NotNull path -> Ok (WasmFile path name hash)

-- | WASM from raw bytes
data WasmData = WasmData
  {
    dataBytes :: B.ByteString
  , dataName :: Nullable String
  , dataHash :: Nullable String
  }


instance JSON WasmData where
  showJSON (WasmData x name hash) =
    let bytes = BS.unpack $ B64.encode x in
    object [
      "data" .= bytes,
      "name" .= name,
      "hash" .= hash
    ]
  readJSON (JSObject x) =
    let d = x .? "data" in
    let name = x .? "name" in
    let hash = x .? "hash" in
    case d of
      Null -> Error "Missing 'path' field"
      NotNull d ->
        case B64.decode d of
          Left msg -> Error msg
          Right d' -> Ok (WasmData d' name hash)


-- | WASM from a URL
data WasmURL = WasmURL
  {
    req :: HTTPRequest
  , urlName :: Nullable String
  , urlHash :: Nullable String
  }


instance JSON WasmURL where
  showJSON (WasmURL req name hash) =
    object (
      "name" .= name :
      "hash" .= hash :
      requestObj req)
  readJSON (JSObject x) =
    let req = x .? "req" in
    let name = x .? "name" in
    let hash = x .? "hash" in
    case fromNullable req of
      Nothing -> Error "Missing 'req' field"
      Just req -> Ok (WasmURL req name hash)

-- | Specifies where to get WASM module data
data Wasm = File WasmFile | Data WasmData | URL WasmURL

instance JSON Wasm where
  showJSON x =
    case x of
      File f -> showJSON f
      Data d -> showJSON d
      URL u -> showJSON u
  readJSON x =
    let file = (readJSON x :: Result WasmFile) in
    case file of
      Ok x -> Ok (File x)
      Error _ ->
        let data' = (readJSON x :: Result WasmData) in
        case data' of
        Ok x -> Ok (Data x)
        Error _ ->
          let url = (readJSON x :: Result WasmURL) in
          case url of
          Ok x -> Ok (URL x)
          Error _ -> Error "JSON does not match any of the Wasm types"

wasmFile :: String -> Wasm
wasmFile path =
  File WasmFile { filePath = path, fileName = null', fileHash = null'}

wasmURL :: String -> String -> Wasm
wasmURL method url =
  let r = HTTPRequest { url = url, headers = null', method = nonNull method } in
  URL WasmURL { req = r, urlName = null', urlHash = null' }

wasmData :: B.ByteString -> Wasm
wasmData d =
  Data WasmData { dataBytes = d, dataName = null', dataHash = null' }

withName :: Wasm -> String -> Wasm
withName (Data d) name = Data d { dataName = nonNull name }
withName (URL url) name =  URL url { urlName = nonNull name }
withName (File f) name = File  f { fileName = nonNull name }


withHash :: Wasm -> String -> Wasm
withHash (Data d) hash = Data d { dataHash = nonNull hash }
withHash (URL url) hash =  URL url { urlHash = nonNull hash }
withHash (File f) hash = File  f { fileHash = nonNull hash }

-- | The 'Manifest' type is used to provide WASM data and configuration to the
-- | Extism runtime
data Manifest = Manifest
  {
    wasm :: [Wasm]
  , memory :: Nullable Memory
  , config :: Nullable [(String, String)]
  , allowedHosts :: Nullable [String]
  , allowedPaths :: Nullable [(String, String)]
  }


instance JSON Manifest where
  showJSON (Manifest wasm memory config hosts paths) =
    let w = makeArray wasm in
    object [
      "wasm" .= w,
      "memory" .= memory,
      "config" .= config,
      "allowed_hosts" .= hosts,
      "allowed_paths" .= paths
    ]
  readJSON (JSObject x) =
    let wasm = x .? "wasm" in
    let memory = x .? "memory" in
    let config = x .? "config" in
    let hosts = x .? "allowed_hosts" in
    let paths = x .? "allowed_paths" in
    case fromNullable wasm of
      Nothing -> Error "Missing 'wasm' field"
      Just wasm -> Ok (Manifest wasm memory config hosts paths)

-- | Create a new 'Manifest' from a list of 'Wasm'
manifest :: [Wasm] -> Manifest
manifest wasm =
  Manifest {
    wasm = wasm,
    memory = null',
    config = null',
    allowedHosts = null',
    allowedPaths = null'
  }

-- | Update the config values
withConfig :: Manifest -> [(String, String)] -> Manifest
withConfig m config =
  m { config = nonNull config }


-- | Update allowed hosts for `extism_http_request`
withHosts :: Manifest -> [String] -> Manifest
withHosts m hosts =
  m { allowedHosts = nonNull hosts }


-- | Update allowed paths
withPaths :: Manifest -> [(String, String)] -> Manifest
withPaths m p =
  m { allowedPaths = nonNull p }

toString :: (JSON a) => a -> String
toString v =
  encode (showJSON v)
