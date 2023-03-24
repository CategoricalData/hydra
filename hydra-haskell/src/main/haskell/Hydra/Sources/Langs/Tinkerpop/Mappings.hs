{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Langs.Tinkerpop.Mappings where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types
import Hydra.Sources.Langs.Tinkerpop.V3


tinkerpopMappingsModule :: Module Kv
tinkerpopMappingsModule = Module ns elements [tinkerpopV3Module] $
    Just "A model for property graph mapping specifications"
  where
    ns = Namespace "hydra/langs/tinkerpop/mappings"
    mappings = nsref ns
    v3 = nsref $ moduleNamespace tinkerpopV3Module
    def = datatype ns

    elements = [

      def "EdgeSpec" $
        doc "A mapping specification producing edges of a specified label." $
        record [
          "label">:
            doc "The label of the target edges, which must conform to the edge type associated with that label." $
            v3 "EdgeLabel",
          "id">:
            doc "A specification of the id of each target edge" $
            mappings "ValueSpec",
          "out">:
            doc "A specification of the out-vertex reference of each target edge" $
            mappings "ValueSpec",
          "in">:
            doc "A specification of the in-vertex reference of each target edge" $
            mappings "ValueSpec",
          "properties">:
            doc "Zero or more property specifications for each target edge" $
            mappings "PropertySpec"],

      def "PropertySpec" $
        doc "A mapping specification producing properties of a specified key, and values of the appropriate type." $
        record [
          "key">:
            doc "The key of the target properties" $
            v3 "PropertyKey",
          "value">:
            doc "A specification of the value of each target property, which must conform to the type associated with the property key" $
            mappings "ValueSpec"],
            
      def "ValueSpec" $
        doc "A mapping specification producing values (usually literal values) whose type is understood in context" $
        union [
          "pattern">:
            doc "A compact path representing the function, e.g. ${}/engineInfo/model/name"
            string],

      def "VertexSpec" $
        doc "A mapping specification producing vertices of a specified label" $
        record [
          "label">:
            doc "The label of the target vertices, which must conform to the vertex type associated with that label." $
            v3 "VertexLabel",
          "id">:
            doc "A specification of the id of each target vertex" $
            mappings "ValueSpec",
          "properties">:
            doc "Zero or more property specifications for each target vertex" $
            list $ mappings "PropertySpec"]]
