{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Langs.Tinkerpop.Mappings where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Compute
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types
import Hydra.Sources.Langs.Tinkerpop.PropertyGraph


tinkerpopMappingsModule :: Module Kv
tinkerpopMappingsModule = Module ns elements [tinkerpopPropertyGraphModule, hydraCoreModule, hydraComputeModule] $
    Just "A model for property graph mapping specifications"
  where
    ns = Namespace "hydra/langs/tinkerpop/mappings"
    mappings = nsref ns
    compute = nsref $ moduleNamespace hydraComputeModule
    core = nsref $ moduleNamespace hydraCoreModule
    v3 = nsref $ moduleNamespace tinkerpopPropertyGraphModule
    def = datatype ns

    elements = [

--      def "EdgeSchema" $
--        doc "A schema enabling encoding and decoding of edges as terms" $
--        lambda "s" $ lambda "a" $ lambda "v" $ lambda "e" $ lambda "p" $
--          record [
--            "label">: v3 "EdgeLabel",
--            "id">: mappings "ValueSchema" @@ "s" @@ "a" @@ "e",
--            "out">: mappings "ValueSchema" @@ "s" @@ "a" @@ "v",
--            "in">: mappings "ValueSchema" @@ "s" @@ "a" @@ "v",
--            "properties">: list $ mappings "PropertySchema" @@ "s" @@ "a" @@ "p"],

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
            list $ mappings "PropertySpec"],

      def "ElementSpec" $
        doc "Either a vertex specification or an edge specification" $
        union [
          "vertex">: mappings "VertexSpec",
          "edge">: mappings "EdgeSpec"],

--      def "PropertySchema" $
--        doc "A schema enabling encoding and decoding of property values as terms" $
--        lambda "s" $ lambda "a" $ lambda "p" $
--          record [
--            "key">: v3 "PropertyKey",
--            "value">: mappings "ValueSchema" @@ "s" @@ "a" @@ "p"],

      def "PropertySpec" $
        doc "A mapping specification producing properties of a specified key, and values of the appropriate type." $
        record [
          "key">:
            doc "The key of the target properties" $
            v3 "PropertyKey",
          "value">:
            doc "A specification of the value of each target property, which must conform to the type associated with the property key" $
            mappings "ValueSpec"],

      def "Schema" $
        lambda "s" $ lambda "a" $ lambda "t" $ lambda "v" $ lambda "e" $ lambda "p" $
          record [
            "vertexIds">: compute "Coder" @@ "s" @@ "s" @@ (core "Term" @@ "a") @@ "v",
            "edgeIds">: compute "Coder" @@ "s" @@ "s" @@ (core "Term" @@ "a") @@ "e",
            "propertyTypes">: compute "Coder" @@ "s" @@ "s" @@ (core "Type" @@ "a") @@ "t",
            "propertyValues">: compute "Coder" @@ "s" @@ "s" @@ (core "Term" @@ "a") @@ "p"],

--      def "Schema" $
--        doc "A set of vertex and edge schemas" $
--        lambda "s" $ lambda "a" $ lambda "v" $ lambda "e" $ lambda "p" $
--        record [
--          "vertices">: list $ mappings "VertexSchema" @@ "s" @@ "a" @@ "v" @@ "p",
--          "edges">: list $ mappings "EdgeSchema" @@ "s" @@ "a" @@ "v" @@ "e" @@ "p"],

--      def "ValueSchema" $
--        doc "A schema enabling encoding and decoding of ids or property values" $
--        lambda "s" $ lambda "a" $ lambda "p" $
--          compute "Coder" @@ "s" @@ "s" @@ "p" @@ (core "Term" @@ "a"),

      def "ValueSpec" $
        doc "A mapping specification producing values (usually literal values) whose type is understood in context" $
        union [
          "value">:
            doc "A trivial no-op specification which passes the entire value"
            unit,
          "pattern">:
            doc "A compact path representing the function, e.g. ${}/engineInfo/model/name"
            string],

--      def "VertexSchema" $
--        doc "A schema enabling encoding and decoding of vertices as terms" $
--        lambda "s" $ lambda "a" $ lambda "v" $ lambda "p" $
--          record [
--            "label">: v3 "VertexLabel",
--            "id">: mappings "ValueSchema" @@ "s" @@ "a" @@ "v",
--            "properties">: list $ mappings "PropertySchema" @@ "s" @@ "a" @@ "p"],

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
