-- Note: this is an automatically generated file. Do not edit.

-- | YAML-to-JSON decoding. Converts YAML Nodes to JSON Values (may fail for non-JSON YAML), and YAML Nodes to Hydra Terms via JSON.

module Hydra.Json.Yaml.Decode where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Decode as Decode
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Yaml.Model as YamlModel
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
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
        YamlModel.ScalarFloat v1 -> Right (JsonModel.ValueNumber v1)
        YamlModel.ScalarInt v1 -> Right (JsonModel.ValueNumber (Literals.bigintToBigfloat v1))
        YamlModel.ScalarNull -> Right JsonModel.ValueNull
        YamlModel.ScalarStr v1 -> Right (JsonModel.ValueString v1)
      YamlModel.NodeSequence v0 ->
        let results = Eithers.mapList (\n -> yamlToJson n) v0
        in (Eithers.map (\vs -> JsonModel.ValueArray vs) results)
