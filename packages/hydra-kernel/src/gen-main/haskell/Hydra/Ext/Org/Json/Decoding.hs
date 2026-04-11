-- Note: this is an automatically generated file. Do not edit.

-- | Decoding functions for JSON data

module Hydra.Ext.Org.Json.Decoding where

import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Decode a JSON array using a decoder for elements
decodeArray :: (Model.Value -> Either String t0) -> Model.Value -> Either String [t0]
decodeArray decodeElem x =
    case x of
      Model.ValueArray v0 -> Eithers.mapList decodeElem v0
      _ -> Left "expected an array"

-- | Decode a JSON boolean value
decodeBoolean :: Model.Value -> Either String Bool
decodeBoolean x =
    case x of
      Model.ValueBoolean v0 -> Right v0
      _ -> Left "expected a boolean"

-- | Decode a required field from a JSON object
decodeField :: (t0 -> Either String t1) -> String -> M.Map String t0 -> Either String t1
decodeField decodeValue name m =
    Eithers.bind (decodeOptionalField decodeValue name m) (Maybes.maybe (Left (Strings.cat2 "missing field: " name)) (\f -> Right f))

-- | Decode a JSON object value
decodeObject :: Model.Value -> Either String (M.Map String Model.Value)
decodeObject x =
    case x of
      Model.ValueObject v0 -> Right v0
      _ -> Left "expected an object"

-- | Decode an optional field from a JSON object
decodeOptionalField :: Ord t3 => ((t0 -> Either t1 t2) -> t3 -> M.Map t3 t0 -> Either t1 (Maybe t2))
decodeOptionalField decodeValue name m =
    Maybes.maybe (Right Nothing) (\v -> Eithers.map (\x -> Just x) (decodeValue v)) (Maps.lookup name m)

-- | Decode a JSON string value
decodeString :: Model.Value -> Either String String
decodeString x =
    case x of
      Model.ValueString v0 -> Right v0
      _ -> Left "expected a string"
