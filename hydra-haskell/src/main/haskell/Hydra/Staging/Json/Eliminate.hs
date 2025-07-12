module Hydra.Staging.Json.Eliminate where

import Hydra.Tools.Monads
import qualified Hydra.Compute as Compute
import qualified Hydra.Monads as Monads
import qualified Hydra.Json as Json

import qualified Data.Map as M


expectArray :: Json.Value -> Compute.Flow s [Json.Value]
expectArray value = case value of
  Json.ValueArray els -> Monads.pure els
  _ -> Monads.unexpected "JSON array" $ show value

expectNumber :: Json.Value -> Compute.Flow s Double
expectNumber value = case value of
  Json.ValueNumber d -> Monads.pure d
  _ -> Monads.unexpected "JSON number" $ show value

expectObject :: Json.Value -> Compute.Flow s (M.Map String Json.Value)
expectObject value = case value of
  Json.ValueObject m -> Monads.pure m
  _ -> Monads.unexpected "JSON object" $ show value

expectString :: Json.Value -> Compute.Flow s String
expectString value = case value of
  Json.ValueString s -> Monads.pure s
  _ -> Monads.unexpected "JSON string" $ show value

opt :: String -> M.Map String Json.Value -> Maybe Json.Value
opt = M.lookup

optArray :: String -> M.Map String Json.Value -> Compute.Flow s (Maybe [Json.Value])
optArray fname m = case opt fname m of
  Nothing -> Monads.pure Nothing
  Just a -> Just <$> expectArray a

optString :: String -> M.Map String Json.Value -> Compute.Flow s (Maybe String)
optString fname m = case opt fname m of
  Nothing -> Monads.pure Nothing
  Just s -> Just <$> expectString s

require :: String -> M.Map String Json.Value -> Compute.Flow s Json.Value
require fname m = case M.lookup fname m of
  Nothing -> Monads.fail $ "required attribute " ++ show fname ++ " not found"
  Just value -> Monads.pure value

requireArray :: String -> M.Map String Json.Value -> Compute.Flow s [Json.Value]
requireArray fname m = require fname m >>= expectArray

requireNumber :: String -> M.Map String Json.Value -> Compute.Flow s Double
requireNumber fname m = require fname m >>= expectNumber

requireString :: String -> M.Map String Json.Value -> Compute.Flow s String
requireString fname m = require fname m >>= expectString
