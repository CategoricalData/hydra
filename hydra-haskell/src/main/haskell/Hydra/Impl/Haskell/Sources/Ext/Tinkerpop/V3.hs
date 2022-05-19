module Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.V3 where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


tinkerpopV3Module :: Module Meta
tinkerpopV3Module = Module tinkerpopV3 [hydraCoreModule]

tinkerpopV3Name :: String
tinkerpopV3Name = "hydra/ext/tinkerpop/v3"

tinkerpopV3 :: Graph Meta
tinkerpopV3 = Graph tinkerpopV3Name elements (const True) hydraCoreName
  where
    core = nominal . qualify hydraCoreName
    v3 = nominal . qualify tinkerpopV3Name
    def = datatype tinkerpopV3Name
    elements = [

      def "Edge" $
        doc "An edge" $
        record [
          field "id" $ v3 "Id",
          field "properties" $ v3 "Properties",
          field "out" $ v3 "Id",
          field "in" $ v3 "Id"],
          
      def "Id" $
        doc "A vertex or edge id" $
        core "Literal",

      def "Properties" $
        doc "A map of property keys to property values" $
        Types.map (v3 "PropertyKey") (core "Literal"),

      def "PropertyKey" $
        doc "A property key"
        string,

      def "PropertyValue" $
        doc "A property value" $
        union [
          field "literal" $ core "Literal",
          field "list" $ list $ v3 "PropertyValue"],

      def "Vertex" $
        doc "A vertex" $
        record [
          field "id" $ v3 "Id",
          field "properties" $ v3 "Properties"],

      def "VertexId" $
        doc "A vertex id" $
        core "Literal"]
