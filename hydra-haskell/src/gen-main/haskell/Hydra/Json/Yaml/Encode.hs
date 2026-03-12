-- Note: this is an automatically generated file. Do not edit.

-- | JSON-to-YAML encoding. Converts JSON Values to YAML Nodes (always succeeds), and Hydra Terms to YAML Nodes via JSON.

module Hydra.Json.Yaml.Encode where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Yaml.Model as Model
import qualified Hydra.Json.Encode as Encode
import qualified Hydra.Json.Model as Model_
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a JSON value to a YAML node. Always succeeds since YAML is a superset of JSON.
jsonToYaml :: (Model_.Value -> Model.Node)
jsonToYaml value = ((\x -> case x of
  Model_.ValueArray v0 -> (Model.NodeSequence (Lists.map (\v -> jsonToYaml v) v0))
  Model_.ValueBoolean v0 -> (Model.NodeScalar (Model.ScalarBool v0))
  Model_.ValueNull -> (Model.NodeScalar Model.ScalarNull)
  Model_.ValueNumber v0 -> (Model.NodeScalar (Model.ScalarFloat v0))
  Model_.ValueObject v0 -> (Model.NodeMapping (Maps.fromList (Lists.map (\kv -> (Model.NodeScalar (Model.ScalarStr (Pairs.first kv)), (jsonToYaml (Pairs.second kv)))) (Maps.toList v0))))
  Model_.ValueString v0 -> (Model.NodeScalar (Model.ScalarStr v0))) value)

-- | Encode a Hydra term to a YAML node via JSON encoding.
toYaml :: (Core.Term -> Either String Model.Node)
toYaml term = (Eithers.map (\v -> jsonToYaml v) (Encode.toJson term))
