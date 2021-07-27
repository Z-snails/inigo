module Inigo.Package.ParseHelpers

import Toml

export
maybe : Either String a -> Either String (Maybe a)
maybe (Left err) = Right Nothing
maybe (Right val) = Right (Just val)

export
withDefault : a -> Either String a -> Either String a
withDefault x (Left err) = Right x
withDefault _ els = els

export
string : List String -> Toml -> Either String String
string key toml =
  case get key toml of
    Just (Str x) =>
      Right x

    _ =>
      Left ("Missing or invalid key: " ++ (show key))

export
getStrings : List String -> Value -> Either String (List String)
getStrings key (Lst vals) = foldlM fold [] vals
  where
    fold : List String -> Value -> Either String (List String)
    fold acc (Str s) = Right $ s :: acc
    fold acc val = Left $ "Invalid value type for " ++ showKey key
getStrings key val = Left $ "Invalid value type for " ++ showKey key

export
listStr : List String -> Toml -> Either String (List String)
listStr key toml =
  case get key toml of
    Just val => getStrings key val
    _ =>
      Left ("Missing or invalid key: " ++ (show key))
