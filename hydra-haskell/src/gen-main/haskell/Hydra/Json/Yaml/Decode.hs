-- Note: this is an automatically generated file. Do not edit.

-- | YAML-to-JSON decoding. Converts YAML Nodes to JSON Values (may fail for non-JSON YAML), and YAML Nodes to Hydra Terms via JSON.

module Hydra.Json.Yaml.Decode where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Yaml.Model as Model
import qualified Hydra.Json.Decode as Decode
import qualified Hydra.Json.Model as Model_
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a YAML node to a JSON value. Fails for non-JSON YAML features (e.g. non-string mapping keys).
yamlToJson :: (Model.Node -> Either String Model_.Value)
yamlToJson node = ((\x -> case x of
  Model.NodeMapping v0 ->  
    let convertEntry = (\kv ->  
            let keyNode = (Pairs.first kv)
            in  
              let valNode = (Pairs.second kv)
              in  
                let keyResult = ((\x -> case x of
                        Model.NodeScalar v1 -> ((\x -> case x of
                          Model.ScalarStr v2 -> (Right v2)
                          _ -> (Left "non-string YAML mapping key")) v1)
                        _ -> (Left "non-scalar YAML mapping key")) keyNode)
                in (Eithers.either (\err -> Left err) (\key ->  
                  let valResult = (yamlToJson valNode)
                  in (Eithers.map (\v -> (key, v)) valResult)) keyResult))
    in  
      let entries = (Eithers.mapList convertEntry (Maps.toList v0))
      in (Eithers.map (\es -> Model_.ValueObject (Maps.fromList es)) entries)
  Model.NodeScalar v0 -> ((\x -> case x of
    Model.ScalarBool v1 -> (Right (Model_.ValueBoolean v1))
    Model.ScalarFloat v1 -> (Right (Model_.ValueNumber v1))
    Model.ScalarInt v1 -> (Right (Model_.ValueNumber (Literals.bigintToBigfloat v1)))
    Model.ScalarNull -> (Right Model_.ValueNull)
    Model.ScalarStr v1 -> (Right (Model_.ValueString v1))) v0)
  Model.NodeSequence v0 ->  
    let results = (Eithers.mapList (\n -> yamlToJson n) v0)
    in (Eithers.map (\vs -> Model_.ValueArray vs) results)) node)

-- | Decode a YAML node to a Hydra term via JSON decoding.
fromYaml :: (M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> Model.Node -> Either String Core.Term)
fromYaml types tname typ node =  
  let jsonResult = (yamlToJson node)
  in (Eithers.either (\err -> Left err) (\json -> Decode.fromJson types tname typ json) jsonResult)
