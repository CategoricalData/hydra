-- Note: this is an automatically generated file. Do not edit.

-- | JSON-to-YAML encoding. Converts JSON Values to YAML Nodes (always succeeds), and Hydra Terms to YAML Nodes via JSON.

module Hydra.Json.Yaml.Encode where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Encode as Encode
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Yaml.Model as Model_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Convert a JSON value to a YAML node. Always succeeds since YAML is a superset of JSON.
jsonToYaml :: Model.Value -> Model_.Node
jsonToYaml value =
    case value of
      Model.ValueArray v0 -> Model_.NodeSequence (Lists.map (\v -> jsonToYaml v) v0)
      Model.ValueBoolean v0 -> Model_.NodeScalar (Model_.ScalarBool v0)
      Model.ValueNull -> Model_.NodeScalar Model_.ScalarNull
      Model.ValueNumber v0 -> Model_.NodeScalar (Model_.ScalarFloat v0)
      Model.ValueObject v0 -> Model_.NodeMapping (Maps.fromList (Lists.map (\kv -> (Model_.NodeScalar (Model_.ScalarStr (Pairs.first kv)), (jsonToYaml (Pairs.second kv)))) (Maps.toList v0)))
      Model.ValueString v0 -> Model_.NodeScalar (Model_.ScalarStr v0)

-- | Encode a Hydra term to a YAML node via JSON encoding.
toYaml :: M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> Core.Term -> Either String Model_.Node
toYaml types tname typ term = Eithers.map (\v -> jsonToYaml v) (Encode.toJson types tname typ term)
