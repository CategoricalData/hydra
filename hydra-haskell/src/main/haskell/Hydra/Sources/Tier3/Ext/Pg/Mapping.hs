{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Pg.Mapping where

import Hydra.Sources.Tier2.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Pg.Mapping
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Ext.Pg.Model


pgMappingModule :: Module
pgMappingModule = Module ns elements
    [pgModelModule, hydraCoreModule, hydraComputeModule] [hydraCoreModule] $
    Just "A model for property graph mapping specifications. See https://github.com/CategoricalData/hydra/wiki/Property-graphs"
  where
    ns = Namespace "hydra.pg.mapping"
    mappings = typeref ns
    compute = typeref $ moduleNamespace hydraComputeModule
    core = typeref $ moduleNamespace hydraCoreModule
    v3 = typeref $ moduleNamespace pgModelModule
    def = datatype ns
    toField (k, v) = Field k $ Terms.string v

    elements = [

      def "AnnotationSchema" $
        doc "Configurable annotation keys for property graph mapping specifications" $
        record [
          "vertexLabel">: string,
          "edgeLabel">: string,
          "vertexId">: string,
          "edgeId">: string,
          "propertyKey">: string,
          "propertyValue">: string,
          "outVertex">: string,
          "outVertexLabel">: string,
          "inVertex">: string,
          "inVertexLabel">: string,
          "outEdge">: string,
          "outEdgeLabel">: string,
          "inEdge">: string,
          "inEdgeLabel">: string,
          "ignore">: string],

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
        doc "A set of mappings which translates between Hydra terms and annotations, and application-specific property graph types" $
        forAlls ["s", "t", "v"] $
          record [
            "vertexIdTypes">: compute "Coder" @@ "s" @@ "s" @@ core "Type" @@ "t",
            "vertexIds">: compute "Coder" @@ "s" @@ "s" @@ core "Term" @@ "v",
            "edgeIdTypes">: compute "Coder" @@ "s" @@ "s" @@ core "Type" @@ "t",
            "edgeIds">: compute "Coder" @@ "s" @@ "s" @@ core "Term" @@ "v",
            "propertyTypes">: compute "Coder" @@ "s" @@ "s" @@ core "Type" @@ "t",
            "propertyValues">: compute "Coder" @@ "s" @@ "s" @@ core "Term" @@ "v",
            "annotations">: mappings "AnnotationSchema",
            "defaultVertexId">: "v",
            "defaultEdgeId">: "v"],

      def "ValueSpec" $
        doc "A mapping specification producing values (usually literal values) whose type is understood in context" $
        union [
          "value">:
            doc "A trivial no-op specification which passes the entire value"
            unit,
          "pattern">:
            doc "A compact path representing the function, e.g. engine-${engineInfo/model/name}"
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
