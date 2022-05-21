module Hydra.Impl.Haskell.Sources.Ext.Json.Json where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


jsonJsonModule :: Module Meta
jsonJsonModule = Module jsonJson []

jsonJsonName :: GraphName
jsonJsonName = GraphName "hydra/ext/json/json"

jsonJson :: Graph Meta
jsonJson = Graph jsonJsonName elements (const True) hydraCoreName
  where
    def = datatype jsonJsonName
    json = nominal . qualify jsonJsonName . Name

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
