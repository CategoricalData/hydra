-- | Decoding functions for JSON data

module Hydra.Ext.Org.Json.Decoding where

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
  Json.ValueString v0 -> (Flows.pure v0)
  _ -> (Flows.fail "expected a string")

decodeNumber :: (Json.Value -> Compute.Flow s Double)
decodeNumber x = case x of
  Json.ValueNumber v1 -> (Flows.pure v1)
  _ -> (Flows.fail "expected a number")

decodeBoolean :: (Json.Value -> Compute.Flow s Bool)
decodeBoolean x = case x of
  Json.ValueBoolean v2 -> (Flows.pure v2)
  _ -> (Flows.fail "expected a boolean")

decodeArray :: ((Json.Value -> Compute.Flow s a) -> Json.Value -> Compute.Flow s [a])
decodeArray decodeElem x = case x of
  Json.ValueArray v3 -> (Flows.mapList decodeElem v3)
  _ -> (Flows.fail "expected an array")

decodeObject :: (Json.Value -> Compute.Flow s (Map String Json.Value))
decodeObject x = case x of
  Json.ValueObject v4 -> (Flows.pure v4)
  _ -> (Flows.fail "expected an object")

decodeField :: ((Json.Value -> Compute.Flow s a) -> String -> Map String Json.Value -> Compute.Flow s a)
decodeField decodeValue name m = (Flows.bind (decodeOptionalField decodeValue name m) (\x -> case x of
  Nothing -> (Flows.fail (Strings.cat [
    "missing field: ",
    name]))
  Just v5 -> (Flows.pure v5)))

decodeOptionalField :: ((Json.Value -> Compute.Flow s a) -> String -> Map String Json.Value -> Compute.Flow s (Maybe a))
decodeOptionalField decodeValue name m = ((\x -> case x of
  Nothing -> (Flows.pure Nothing)
  Just v6 -> (Flows.map (\x -> Just x) (decodeValue v6))) (Maps.lookup name m))