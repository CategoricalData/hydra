-- Note: this is an automatically generated file. Do not edit.

-- | YAML-to-JSON decoding. Converts YAML Nodes to JSON Values (may fail for non-JSON YAML), and YAML Nodes to Hydra Terms via JSON.

module Hydra.Json.Yaml.Decode where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Decode as Decode
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Yaml.Model as Model_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Decode a YAML node to a Hydra term via JSON decoding.
fromYaml :: M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> Model_.Node -> Either String Core.Term
fromYaml types tname typ node =

      let jsonResult = yamlToJson node
      in (Eithers.either (\err -> Left err) (\json -> Decode.fromJson types tname typ json) jsonResult)

-- | Convert a YAML node to a JSON value. Fails for non-JSON YAML features (e.g. non-string mapping keys).
yamlToJson :: Model_.Node -> Either String Model.Value
yamlToJson node =
    case node of
      Model_.NodeMapping v0 ->
        let convertEntry =
                \kv ->
                  let keyNode = Pairs.first kv
                      valNode = Pairs.second kv
                      keyResult =
                              case keyNode of
                                Model_.NodeScalar v1 -> case v1 of
                                  Model_.ScalarStr v2 -> Right v2
                                  _ -> Left "non-string YAML mapping key"
                                _ -> Left "non-scalar YAML mapping key"
                  in (Eithers.either (\err -> Left err) (\key ->
                    let valResult = yamlToJson valNode
                    in (Eithers.map (\v -> (key, v)) valResult)) keyResult)
            entries = Eithers.mapList convertEntry (Maps.toList v0)
        in (Eithers.map (\es -> Model.ValueObject (Maps.fromList es)) entries)
      Model_.NodeScalar v0 -> case v0 of
        Model_.ScalarBool v1 -> Right (Model.ValueBoolean v1)
        Model_.ScalarFloat v1 -> Right (Model.ValueNumber v1)
        Model_.ScalarInt v1 -> Right (Model.ValueNumber (Literals.bigintToBigfloat v1))
        Model_.ScalarNull -> Right Model.ValueNull
        Model_.ScalarStr v1 -> Right (Model.ValueString v1)
      Model_.NodeSequence v0 ->
        let results = Eithers.mapList (\n -> yamlToJson n) v0
        in (Eithers.map (\vs -> Model.ValueArray vs) results)
