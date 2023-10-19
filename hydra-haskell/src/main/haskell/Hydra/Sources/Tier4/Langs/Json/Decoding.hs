{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Json.Decoding where

-- TODO: standardized Tier-4 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier2.All

import qualified Hydra.Json as Json
import Hydra.Sources.Tier0.Json


jsonDecodingModule :: Module Kv
jsonDecodingModule = Module (Namespace "hydra/langs/json/decoding") elements [jsonModelModule, hydraCoreModule] [jsonModelModule] $
    Just "Decoding functions for JSON data"
  where
   elements = [
     Base.el decodeStringDef,
     Base.el decodeNumberDef,
     Base.el decodeBooleanDef,
     Base.el decodeArrayDef,
     Base.el decodeObjectDef,
     Base.el decodeFieldDef,
     Base.el decodeOptionalFieldDef]

jsonDecodingDefinition :: String -> Datum a -> Definition a
jsonDecodingDefinition label = definitionInModule jsonDecodingModule ("decode" <> label)

valueT = TypeVariable Json._Value

decodeStringDef :: Definition (Json.Value -> Flow s String)
decodeStringDef  = jsonDecodingDefinition "String" $
  function valueT (flowT sT stringT) $
  match Json._Value (Just $ Flows.fail @@ "expected a string") [
    Json._Value_string>>: Flows.pure]

decodeNumberDef :: Definition (Json.Value -> Flow s Double)
decodeNumberDef  = jsonDecodingDefinition "Number" $
  function valueT (flowT sT Types.bigfloat) $
  match Json._Value (Just $ Flows.fail @@ "expected a number") [
    Json._Value_number>>: Flows.pure]

decodeBooleanDef :: Definition (Json.Value -> Flow s Bool)
decodeBooleanDef  = jsonDecodingDefinition "Boolean" $
  function valueT (flowT sT booleanT) $
  match Json._Value (Just $ Flows.fail @@ "expected a boolean") [
    Json._Value_boolean>>: Flows.pure]

decodeArrayDef :: Definition ((Json.Value -> Flow s a) -> Json.Value -> Flow s [a])
decodeArrayDef  = jsonDecodingDefinition "Array" $
  function (functionT valueT (flowT sT aT)) (functionT valueT (flowT sT (listT aT))) $
  lambda "decodeElem" $ match Json._Value (Just $ Flows.fail @@ "expected an array") [
    Json._Value_array>>: Flows.mapList @@ (var "decodeElem")]

decodeObjectDef :: Definition (Json.Value -> Flow s (M.Map String Json.Value))
decodeObjectDef  = jsonDecodingDefinition "Object" $
  function valueT (flowT sT (mapT stringT valueT)) $
  match Json._Value (Just $ Flows.fail @@ "expected an object") [
    Json._Value_object>>: Flows.pure]

decodeFieldDef :: Definition ((Json.Value -> Flow s a) -> String -> (M.Map String Json.Value) -> Flow s a)
decodeFieldDef  = jsonDecodingDefinition "Field" $
  function (functionT valueT (flowT sT aT)) (functionT stringT (functionT (mapT stringT valueT) (flowT sT aT))) $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    Flows.bind
      @@ (ref decodeOptionalFieldDef @@ var "decodeValue" @@ var "name" @@ var "m")
      @@ (matchOpt (Flows.fail @@ ("missing field: " ++ var "name")) Flows.pure)

decodeOptionalFieldDef :: Definition ((Json.Value -> Flow s a) -> String -> (M.Map String Json.Value) -> Flow s (Maybe a))
decodeOptionalFieldDef  = jsonDecodingDefinition "OptionalField" $
  function (functionT valueT (flowT sT aT)) (functionT stringT (functionT (mapT stringT valueT) (flowT sT (Types.optional aT)))) $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    (matchOpt (Flows.pure @@ nothing) (lambda "v" (Flows.map @@ (lambda "x" (just $ var "x")) @@ (var "decodeValue" @@ var "v"))))
      @@ (Maps.lookup @@ var "name" @@ var "m")
