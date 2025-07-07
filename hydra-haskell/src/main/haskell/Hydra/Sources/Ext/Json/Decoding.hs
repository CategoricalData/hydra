{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Ext.Json.Decoding where

-- TODO: standardized Tier-3 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Dsl.Phantoms        as Phantoms
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Kernel

import Hydra.Kernel
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.All as Tier2
import qualified Hydra.Json as Json
import Hydra.Sources.Kernel.Types.Json
import Hydra.Sources.Libraries


jsonDecodingModule :: Module
jsonDecodingModule = Module (Namespace "hydra.ext.org.json.decoding") elements
    [KernelTypes.jsonModelModule, KernelTypes.hydraCoreModule]
    [KernelTypes.jsonModelModule, KernelTypes.hydraComputeModule] $
    Just "Decoding functions for JSON data"
  where
   elements = [
     Phantoms.el decodeArrayDef,
     Phantoms.el decodeBooleanDef,
     Phantoms.el decodeFieldDef,
--     Phantoms.el decodeNumberDef, TODO: restore
     Phantoms.el decodeObjectDef,
     Phantoms.el decodeOptionalFieldDef,
     Phantoms.el decodeStringDef]

jsonDecodingDefinition :: String -> TTerm a -> TElement a
jsonDecodingDefinition label = definitionInModule jsonDecodingModule ("decode" <> label)

valueT = TypeVariable Json._Value

decodeArrayDef :: TElement ((Json.Value -> Flow s a) -> Json.Value -> Flow s [a])
decodeArrayDef  = jsonDecodingDefinition "Array" $
  lambda "decodeElem" $ match Json._Value (Just $ Flows.fail "expected an array") [
    Json._Value_array>>: lambda "a" $ Flows.mapList (var "decodeElem") $ var "a"]

decodeBooleanDef :: TElement (Json.Value -> Flow s Bool)
decodeBooleanDef  = jsonDecodingDefinition "Boolean" $
  match Json._Value (Just $ Flows.fail $ "expected a boolean") [
    Json._Value_boolean>>: lambda "b" $ Flows.pure $ var "b"]

decodeFieldDef :: TElement ((Json.Value -> Flow s a) -> String -> (M.Map String Json.Value) -> Flow s a)
decodeFieldDef  = jsonDecodingDefinition "Field" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    Flows.bind
      (ref decodeOptionalFieldDef @@ var "decodeValue" @@ var "name" @@ var "m")
      (primitive _optionals_maybe
        @@ (Flows.fail ("missing field: " ++ var "name"))
        @@ (lambda "f" $ Flows.pure $ var "f"))

decodeNumberDef :: TElement (Json.Value -> Flow s Double)
decodeNumberDef  = jsonDecodingDefinition "Number" $
  match Json._Value (Just $ Flows.fail "expected a number") [
    Json._Value_number>>: lambda "n" $ Flows.pure $ var "n"]

decodeObjectDef :: TElement (Json.Value -> Flow s (M.Map String Json.Value))
decodeObjectDef  = jsonDecodingDefinition "Object" $
  match Json._Value (Just $ Flows.fail "expected an object") [
    Json._Value_object>>: lambda "o" $ Flows.pure $ var "o"]

decodeOptionalFieldDef :: TElement ((Json.Value -> Flow s a) -> String -> (M.Map String Json.Value) -> Flow s (Maybe a))
decodeOptionalFieldDef  = jsonDecodingDefinition "OptionalField" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    (primitive _optionals_maybe
        @@ (Flows.pure nothing)
        @@ (lambda "v" (Flows.map (lambda "x" (just $ var "x")) (var "decodeValue" @@ var "v"))))
      @@ (Maps.lookup (var "name") (var "m"))

decodeStringDef :: TElement (Json.Value -> Flow s String)
decodeStringDef  = jsonDecodingDefinition "String" $
  match Json._Value (Just $ Flows.fail "expected a string") [
    Json._Value_string>>: lambda "s" $ Flows.pure $ var "s"]
