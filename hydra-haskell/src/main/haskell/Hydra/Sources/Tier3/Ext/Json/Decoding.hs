{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Json.Decoding where

-- TODO: standardized Tier-3 imports
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
import Hydra.Sources.Tier1.Json


jsonDecodingModule :: Module
jsonDecodingModule = Module (Namespace "hydra/ext/org/json/decoding") elements
    [jsonModelModule, hydraCoreModule] (jsonModelModule:[hydraCoreModule]) $
    Just "Decoding functions for JSON data"
  where
   elements = [
     Base.el decodeArrayDef,
     Base.el decodeBooleanDef,
     Base.el decodeFieldDef,
     Base.el decodeNumberDef,
     Base.el decodeObjectDef,
     Base.el decodeOptionalFieldDef,
     Base.el decodeStringDef]

jsonDecodingDefinition :: String -> TTerm a -> TElement a
jsonDecodingDefinition label = definitionInModule jsonDecodingModule ("decode" <> label)

valueT = TypeVariable Json._Value

decodeArrayDef :: TElement ((Json.Value -> Flow s a) -> Json.Value -> Flow s [a])
decodeArrayDef  = jsonDecodingDefinition "Array" $
  function (tFun valueT (tFlow tS tA)) (tFun valueT (tFlow tS (tList tA))) $
  lambda "decodeElem" $ match Json._Value (Just $ Flows.fail @@ "expected an array") [
    Json._Value_array>>: Flows.mapList @@ (var "decodeElem")]

decodeBooleanDef :: TElement (Json.Value -> Flow s Bool)
decodeBooleanDef  = jsonDecodingDefinition "Boolean" $
  function valueT (tFlow tS tBoolean) $
  match Json._Value (Just $ Flows.fail @@ "expected a boolean") [
    Json._Value_boolean>>: Flows.pure]

decodeFieldDef :: TElement ((Json.Value -> Flow s a) -> String -> (M.Map String Json.Value) -> Flow s a)
decodeFieldDef  = jsonDecodingDefinition "Field" $
  function (tFun valueT (tFlow tS tA)) (tFun tString (tFun (tMap tString valueT) (tFlow tS tA))) $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    Flows.bind
      @@ (ref decodeOptionalFieldDef @@ var "decodeValue" @@ var "name" @@ var "m")
      @@ (matchOpt (Flows.fail @@ ("missing field: " ++ var "name")) Flows.pure)

decodeNumberDef :: TElement (Json.Value -> Flow s Double)
decodeNumberDef  = jsonDecodingDefinition "Number" $
  function valueT (tFlow tS Types.bigfloat) $
  match Json._Value (Just $ Flows.fail @@ "expected a number") [
    Json._Value_number>>: Flows.pure]

decodeObjectDef :: TElement (Json.Value -> Flow s (M.Map String Json.Value))
decodeObjectDef  = jsonDecodingDefinition "Object" $
  function valueT (tFlow tS (tMap tString valueT)) $
  match Json._Value (Just $ Flows.fail @@ "expected an object") [
    Json._Value_object>>: Flows.pure]

decodeOptionalFieldDef :: TElement ((Json.Value -> Flow s a) -> String -> (M.Map String Json.Value) -> Flow s (Maybe a))
decodeOptionalFieldDef  = jsonDecodingDefinition "OptionalField" $
  function (tFun valueT (tFlow tS tA)) (tFun tString (tFun (tMap tString valueT) (tFlow tS (Types.optional tA)))) $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    (matchOpt (Flows.pure @@ nothing) (lambda "v" (Flows.map @@ (lambda "x" (just $ var "x")) @@ (var "decodeValue" @@ var "v"))))
      @@ (Maps.lookup @@ var "name" @@ var "m")

decodeStringDef :: TElement (Json.Value -> Flow s String)
decodeStringDef  = jsonDecodingDefinition "String" $
  function valueT (tFlow tS tString) $
  match Json._Value (Just $ Flows.fail @@ "expected a string") [
    Json._Value_string>>: Flows.pure]
