-- | Decoding functions for JSON data

module Hydra.Langs.Json.Decoding where

import qualified Hydra.Compute as Compute
import qualified Hydra.Json as Json
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Strings as Strings
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

decodeString :: (Json.Value -> Compute.Flow s String)
decodeString x = case x of
  Json.ValueString v282 -> (Flows.pure v282)
  _ -> (Flows.fail "expected a string")

decodeNumber :: (Json.Value -> Compute.Flow s Double)
decodeNumber x = case x of
  Json.ValueNumber v283 -> (Flows.pure v283)
  _ -> (Flows.fail "expected a number")

decodeBoolean :: (Json.Value -> Compute.Flow s Bool)
decodeBoolean x = case x of
  Json.ValueBoolean v284 -> (Flows.pure v284)
  _ -> (Flows.fail "expected a boolean")

decodeArray :: ((Json.Value -> Compute.Flow s a) -> Json.Value -> Compute.Flow s [a])
decodeArray decodeElem x = case x of
  Json.ValueArray v285 -> (Flows.mapList decodeElem v285)
  _ -> (Flows.fail "expected an array")

decodeObject :: (Json.Value -> Compute.Flow s (Map String Json.Value))
decodeObject x = case x of
  Json.ValueObject v286 -> (Flows.pure v286)
  _ -> (Flows.fail "expected an object")

decodeField :: ((Json.Value -> Compute.Flow s a) -> String -> Map String Json.Value -> Compute.Flow s a)
decodeField decodeValue name m = (Flows.bind (decodeOptionalField decodeValue name m) (\x -> case x of
  Nothing -> (Flows.fail (Strings.cat [
    "missing field: ",
    name]))
  Just v287 -> (Flows.pure v287)))

decodeOptionalField :: ((Json.Value -> Compute.Flow s a) -> String -> Map String Json.Value -> Compute.Flow s (Maybe a))
decodeOptionalField decodeValue name m = ((\x -> case x of
  Nothing -> (Flows.pure Nothing)
  Just v288 -> (Flows.map (\x -> Just x) (decodeValue v288))) (Maps.lookup name m))