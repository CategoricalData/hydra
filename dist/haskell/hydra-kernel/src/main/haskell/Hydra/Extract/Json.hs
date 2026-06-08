-- Note: this is an automatically generated file. Do not edit.
-- | Utilities for extracting values from JSON objects

module Hydra.Extract.Json where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Extract an array from a JSON value, failing if the value is not an array
expectArray :: Model.Value -> Either String [Model.Value]
expectArray value =
    case value of
      Model.ValueArray v0 -> Right v0
      _ -> Left (Strings.cat2 (Strings.cat2 "expected " "JSON array") (Strings.cat2 " but found " (showValue value)))
-- | Extract a number from a JSON value, failing if the value is not a number
expectNumber :: Model.Value -> Either String Sci.Scientific
expectNumber value =
    case value of
      Model.ValueNumber v0 -> Right v0
      _ -> Left (Strings.cat2 (Strings.cat2 "expected " "JSON number") (Strings.cat2 " but found " (showValue value)))
-- | Extract an object from a JSON value as a name-keyed map, failing if the value is not an object. Field order is not preserved; lookups are by name.
expectObject :: Model.Value -> Either String (M.Map String Model.Value)
expectObject value =
    case value of
      Model.ValueObject v0 -> Right (Maps.fromList v0)
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
optArray fname m = Optionals.cases (opt fname m) (Right Nothing) (\a -> Eithers.map (\x -> Just x) (expectArray a))
-- | Look up an optional string field in a JSON object
optString :: Ord t0 => (t0 -> M.Map t0 Model.Value -> Either String (Maybe String))
optString fname m = Optionals.cases (opt fname m) (Right Nothing) (\s -> Eithers.map (\x -> Just x) (expectString s))
-- | Look up a required field in a JSON object, failing if not found
require :: Ord t0 => (t0 -> M.Map t0 t1 -> Either String t1)
require fname m =
    Optionals.cases (Maps.lookup fname m) (Left (Strings.cat [
      "required attribute ",
      (showValue fname),
      " not found"])) (\value -> Right value)
-- | Look up a required array field in a JSON object
requireArray :: Ord t0 => (t0 -> M.Map t0 Model.Value -> Either String [Model.Value])
requireArray fname m = Eithers.bind (require fname m) expectArray
-- | Look up a required number field in a JSON object
requireNumber :: Ord t0 => (t0 -> M.Map t0 Model.Value -> Either String Sci.Scientific)
requireNumber fname m = Eithers.bind (require fname m) expectNumber
-- | Look up a required string field in a JSON object
requireString :: Ord t0 => (t0 -> M.Map t0 Model.Value -> Either String String)
requireString fname m = Eithers.bind (require fname m) expectString
-- | Show a JSON value as a string (placeholder implementation)
showValue :: t0 -> String
showValue value = "TODO: implement showValue"
