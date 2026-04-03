-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for extracting values from JSON objects

module Hydra.Extract.Json where

import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Extract an array from a JSON value, failing if the value is not an array
expectArray :: Model.Value -> Either String [Model.Value]
expectArray value =
    case value of
      Model.ValueArray v0 -> Right v0
      _ -> Left (Strings.cat2 (Strings.cat2 "expected " "JSON array") (Strings.cat2 " but found " (showValue value)))

-- | Extract a number from a JSON value, failing if the value is not a number
expectNumber :: Model.Value -> Either String Double
expectNumber value =
    case value of
      Model.ValueNumber v0 -> Right v0
      _ -> Left (Strings.cat2 (Strings.cat2 "expected " "JSON number") (Strings.cat2 " but found " (showValue value)))

-- | Extract an object from a JSON value, failing if the value is not an object
expectObject :: Model.Value -> Either String (M.Map String Model.Value)
expectObject value =
    case value of
      Model.ValueObject v0 -> Right v0
      _ -> Left (Strings.cat2 (Strings.cat2 "expected " "JSON object") (Strings.cat2 " but found " (showValue value)))

-- | Extract a string from a JSON value, failing if the value is not a string
expectString :: Model.Value -> Either String String
expectString value =
    case value of
      Model.ValueString v0 -> Right v0
      _ -> Left (Strings.cat2 (Strings.cat2 "expected " "JSON string") (Strings.cat2 " but found " (showValue value)))

-- | Look up an optional field in a JSON object
opt :: Ord t0 => (t0 -> M.Map t0 t1 -> Maybe t1)
opt fname m = Maps.lookup fname m

-- | Look up an optional array field in a JSON object
optArray :: Ord t0 => (t0 -> M.Map t0 Model.Value -> Either String (Maybe [Model.Value]))
optArray fname m = Maybes.maybe (Right Nothing) (\a -> Eithers.map (\x -> Just x) (expectArray a)) (opt fname m)

-- | Look up an optional string field in a JSON object
optString :: Ord t0 => (t0 -> M.Map t0 Model.Value -> Either String (Maybe String))
optString fname m = Maybes.maybe (Right Nothing) (\s -> Eithers.map (\x -> Just x) (expectString s)) (opt fname m)

-- | Look up a required field in a JSON object, failing if not found
require :: Ord t0 => (t0 -> M.Map t0 t1 -> Either String t1)
require fname m =
    Maybes.maybe (Left (Strings.cat [
      "required attribute ",
      (showValue fname),
      " not found"])) (\value -> Right value) (Maps.lookup fname m)

-- | Look up a required array field in a JSON object
requireArray :: Ord t0 => (t0 -> M.Map t0 Model.Value -> Either String [Model.Value])
requireArray fname m = Eithers.bind (require fname m) expectArray

-- | Look up a required number field in a JSON object
requireNumber :: Ord t0 => (t0 -> M.Map t0 Model.Value -> Either String Double)
requireNumber fname m = Eithers.bind (require fname m) expectNumber

-- | Look up a required string field in a JSON object
requireString :: Ord t0 => (t0 -> M.Map t0 Model.Value -> Either String String)
requireString fname m = Eithers.bind (require fname m) expectString

-- | Show a JSON value as a string (placeholder implementation)
showValue :: t0 -> String
showValue value = "TODO: implement showValue"
