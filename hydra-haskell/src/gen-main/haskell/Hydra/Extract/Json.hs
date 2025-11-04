-- | Utilities for extracting values from JSON objects

module Hydra.Extract.Json where

import qualified Hydra.Compute as Compute
import qualified Hydra.Json as Json
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Monads as Monads
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

expectArray :: (Json.Value -> Compute.Flow t0 [Json.Value])
expectArray value = ((\x -> case x of
  Json.ValueArray v1 -> (Flows.pure v1)
  _ -> (Monads.unexpected "JSON array" (showValue value))) value)

expectNumber :: (Json.Value -> Compute.Flow t0 Double)
expectNumber value = ((\x -> case x of
  Json.ValueNumber v1 -> (Flows.pure v1)
  _ -> (Monads.unexpected "JSON number" (showValue value))) value)

expectObject :: (Json.Value -> Compute.Flow t0 (M.Map String Json.Value))
expectObject value = ((\x -> case x of
  Json.ValueObject v1 -> (Flows.pure v1)
  _ -> (Monads.unexpected "JSON object" (showValue value))) value)

expectString :: (Json.Value -> Compute.Flow t0 String)
expectString value = ((\x -> case x of
  Json.ValueString v1 -> (Flows.pure v1)
  _ -> (Monads.unexpected "JSON string" (showValue value))) value)

opt :: (Ord t0) => (t0 -> M.Map t0 t1 -> Maybe t1)
opt fname m = (Maps.lookup fname m)

optArray :: (Ord t0) => (t0 -> M.Map t0 Json.Value -> Compute.Flow t1 (Maybe [Json.Value]))
optArray fname m = (Maybes.maybe (Flows.pure Nothing) (\a -> Flows.map Maybes.pure (expectArray a)) (opt fname m))

optString :: (Ord t0) => (t0 -> M.Map t0 Json.Value -> Compute.Flow t1 (Maybe String))
optString fname m = (Maybes.maybe (Flows.pure Nothing) (\s -> Flows.map Maybes.pure (expectString s)) (opt fname m))

require :: (Ord t0) => (t0 -> M.Map t0 t1 -> Compute.Flow t2 t1)
require fname m = (Maybes.maybe (Flows.fail (Strings.cat [
  "required attribute ",
  showValue fname,
  " not found"])) (\value -> Flows.pure value) (Maps.lookup fname m))

requireArray :: (Ord t0) => (t0 -> M.Map t0 Json.Value -> Compute.Flow t1 [Json.Value])
requireArray fname m = (Flows.bind (require fname m) expectArray)

requireNumber :: (Ord t0) => (t0 -> M.Map t0 Json.Value -> Compute.Flow t1 Double)
requireNumber fname m = (Flows.bind (require fname m) expectNumber)

requireString :: (Ord t0) => (t0 -> M.Map t0 Json.Value -> Compute.Flow t1 String)
requireString fname m = (Flows.bind (require fname m) expectString)

showValue :: (t0 -> String)
showValue value = "TODO: implement showValue"
