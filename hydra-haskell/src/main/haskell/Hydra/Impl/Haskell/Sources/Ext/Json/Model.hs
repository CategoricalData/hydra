module Hydra.Impl.Haskell.Sources.Ext.Json.Model where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


jsonModelModule :: Module Meta
jsonModelModule = Module jsonModel []

jsonModelName :: GraphName
jsonModelName = GraphName "hydra/ext/json/model"

jsonModel :: Graph Meta
jsonModel = Graph jsonModelName elements (const True) hydraCoreName
  where
    def = datatype jsonModelName
    json = nominal . qualify jsonModelName . Name

    elements = [

      def "Number" $
        doc "A numeric value" $
        record [
          field "integer" bigint,
          field "fraction" bigint,
          field "exponent" bigint],

      def "Value" $
        doc "A JSON value" $
        union [
          field "array" $ list $ json "Value",
          field "boolean" boolean,
          field "null" unit,
          field "number" $ json "Number",
          field "object" $ Types.map string (json "Value"),
          field "string" string]]
