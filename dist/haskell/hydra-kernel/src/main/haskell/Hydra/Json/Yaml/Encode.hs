-- Note: this is an automatically generated file. Do not edit.
-- | JSON-to-YAML encoding. Converts JSON Values to YAML Nodes (always succeeds), and Hydra Terms to YAML Nodes via JSON.

module Hydra.Json.Yaml.Encode where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Encode as Encode
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Pairs as Pairs
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
import qualified Hydra.Yaml.Model as YamlModel
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Convert a JSON value to a YAML node. Always succeeds since YAML is a superset of JSON.
jsonToYaml :: JsonModel.Value -> YamlModel.Node
jsonToYaml value =
    case value of
      JsonModel.ValueArray v0 -> YamlModel.NodeSequence (Lists.map (\v -> jsonToYaml v) v0)
      JsonModel.ValueBoolean v0 -> YamlModel.NodeScalar (YamlModel.ScalarBool v0)
      JsonModel.ValueNull -> YamlModel.NodeScalar YamlModel.ScalarNull
      JsonModel.ValueNumber v0 -> YamlModel.NodeScalar (YamlModel.ScalarDecimal v0)
      JsonModel.ValueObject v0 -> YamlModel.NodeMapping (Maps.fromList (Lists.map (\kv -> (YamlModel.NodeScalar (YamlModel.ScalarStr (Pairs.first kv)), (jsonToYaml (Pairs.second kv)))) (Maps.toList v0)))
      JsonModel.ValueString v0 -> YamlModel.NodeScalar (YamlModel.ScalarStr v0)
-- | Encode a Hydra term to a YAML node via JSON encoding.
toYaml :: M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> Core.Term -> Either String YamlModel.Node
toYaml types tname typ term = Eithers.map (\v -> jsonToYaml v) (Encode.toJson types tname typ term)
