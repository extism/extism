{-# LANGUAGE AllowAmbiguousTypes #-}

module Extism.JSON (
  module Extism.JSON,
  module Text.JSON
) where

import Text.JSON
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS (unpack)

data Nullable a = Null | NotNull a

makeArray x = JSArray [showJSON a | a <- x]
isNull JSNull = True
isNull _ = False
filterNulls obj = [(a, b) | (a, b) <- obj, not (isNull b)]
object x = makeObj $ filterNulls x
objectWithNulls x = makeObj x
nonNull x = NotNull x
null' = Null
(.=) a b = (a, showJSON b)
toNullable (Just x) = NotNull x
toNullable Nothing = Null
fromNullable (NotNull x) = Just x
fromNullable Null = Nothing

(.?) (JSObject a) k =
  case valFromObj k a of
    Ok x -> NotNull x
    Error _ -> Null
(.?) _ _ = Null
(.??) a k = toNullable $ lookup k a

find :: JSON a => String -> JSValue -> Nullable JSValue
find k obj = obj .? k

update :: JSON a => String -> a -> JSValue -> JSValue
update k v (JSObject obj) = object $ (fromJSObject obj) ++ [k .= v]

instance JSON a => JSON (Nullable a) where
  showJSON (NotNull x) = showJSON x
  showJSON Null = JSNull
  readJSON JSNull = Ok Null
  readJSON x = readJSON x


newtype Base64 = Base64 B.ByteString

instance JSON Base64 where
  showJSON (Base64 bs) = showJSON (BS.unpack $ B64.encode bs)
  readJSON (JSString s) =
    let toByteString x = B.pack (Prelude.map c2w x) in
    case B64.decode (toByteString (fromJSString s)) of
    Left msg -> Error msg
    Right d -> Ok (Base64 d)
