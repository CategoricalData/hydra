-- Note: this is an automatically generated file. Do not edit.
-- | YAML-to-JSON decoding. Converts YAML Nodes to JSON Values (may fail for non-JSON YAML), and YAML Nodes to Hydra Terms via JSON.

module Hydra.Json.Yaml.Decode where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Decode as Decode
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import qualified Hydra.Yaml.Model as YamlModel
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Decode a YAML node to a Hydra term via JSON decoding.
fromYaml :: M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> YamlModel.Node -> Either String Core.Term
fromYaml types tname typ node =

      let jsonResult = yamlToJson node
      in (Eithers.either (\err -> Left err) (\json -> Decode.fromJson types tname typ json) jsonResult)
-- | Convert a YAML node to a JSON value. Fails for non-JSON YAML features (e.g. non-string mapping keys).
yamlToJson :: YamlModel.Node -> Either String JsonModel.Value
yamlToJson node =
    case node of
      YamlModel.NodeMapping v0 ->
        let convertEntry =
                \kv ->
                  let keyNode = Pairs.first kv
                      valNode = Pairs.second kv
                      keyResult =
                              case keyNode of
                                YamlModel.NodeScalar v1 -> case v1 of
                                  YamlModel.ScalarStr v2 -> Right v2
                                  _ -> Left "non-string YAML mapping key"
                                _ -> Left "non-scalar YAML mapping key"
                  in (Eithers.either (\err -> Left err) (\key ->
                    let valResult = yamlToJson valNode
                    in (Eithers.map (\v -> (key, v)) valResult)) keyResult)
            entries = Eithers.mapList convertEntry (Maps.toList v0)
        in (Eithers.map (\es -> JsonModel.ValueObject (Maps.fromList es)) entries)
      YamlModel.NodeScalar v0 -> case v0 of
        YamlModel.ScalarBool v1 -> Right (JsonModel.ValueBoolean v1)
        YamlModel.ScalarDecimal v1 -> Right (JsonModel.ValueNumber v1)
        YamlModel.ScalarFloat v1 -> Right (JsonModel.ValueNumber (Literals.float64ToDecimal v1))
        YamlModel.ScalarInt v1 -> Right (JsonModel.ValueNumber (Literals.bigintToDecimal v1))
        YamlModel.ScalarNull -> Right JsonModel.ValueNull
        YamlModel.ScalarStr v1 -> Right (JsonModel.ValueString v1)
      YamlModel.NodeSequence v0 ->
        let results = Eithers.mapList (\n -> yamlToJson n) v0
        in (Eithers.map (\vs -> JsonModel.ValueArray vs) results)
