-- | Decoding functions for JSON data

module Hydra.Ext.Org.Json.Decoding where

import qualified Hydra.Compute as Compute
import qualified Hydra.Json as Json
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

decodeArray :: ((Json.Value -> Compute.Flow t0 t1) -> Json.Value -> Compute.Flow t0 [t1])
decodeArray decodeElem x = case x of
  Json.ValueArray v1 -> (Flows.mapList decodeElem v1)
  _ -> (Flows.fail "expected an array")

decodeBoolean :: (Json.Value -> Compute.Flow t0 Bool)
decodeBoolean x = case x of
  Json.ValueBoolean v1 -> (Flows.pure v1)
  _ -> (Flows.fail "expected a boolean")

decodeField :: ((t0 -> Compute.Flow t1 t2) -> String -> M.Map String t0 -> Compute.Flow t1 t2)
decodeField decodeValue name m = (Flows.bind (decodeOptionalField decodeValue name m) (Optionals.maybe (Flows.fail (Strings.cat2 "missing field: " name)) (\f -> Flows.pure f)))

decodeObject :: (Json.Value -> Compute.Flow t0 (M.Map String Json.Value))
decodeObject x = case x of
  Json.ValueObject v1 -> (Flows.pure v1)
  _ -> (Flows.fail "expected an object")

decodeOptionalField :: (Ord t3) => ((t0 -> Compute.Flow t1 t2) -> t3 -> M.Map t3 t0 -> Compute.Flow t1 (Maybe t2))
decodeOptionalField decodeValue name m = (Optionals.maybe (Flows.pure Nothing) (\v -> Flows.map (\x -> Just x) (decodeValue v)) (Maps.lookup name m))

decodeString :: (Json.Value -> Compute.Flow t0 String)
decodeString x = case x of
  Json.ValueString v1 -> (Flows.pure v1)
  _ -> (Flows.fail "expected a string")
