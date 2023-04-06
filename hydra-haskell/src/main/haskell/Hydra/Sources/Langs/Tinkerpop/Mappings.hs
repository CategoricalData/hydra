{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Langs.Tinkerpop.Mappings where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Compute
import Hydra.Sources.Core
import Hydra.Langs.Tinkerpop.Mappings
import Hydra.Dsl.Types as Record
import qualified Hydra.Dsl.Terms as Terms
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
    defterm = dataterm ns
    defRecord tname local fields = dataterm ns local (TypeWrap tname) $ Terms.record tname fields
    toField (k, v) = Field k $ Terms.string v

    elements = [

      def "AnnotationSchema" $
        doc "Configurable annotations for property graph mapping specifications" $
        record [
          "vertexLabel">: string,
          "edgeLabel">: string,
          "vertexId">: string,
          "edgeId">: string,
          "outVertex">: string,
          "outVertexLabel">: string,
          "inVertex">: string,
          "inVertexLabel">: string,
          "outEdge">: string,
          "outEdgeLabel">: string,
          "inEdge">: string,
          "inEdgeLabel">: string,
          "ignore">: string],

--       defRecord _AnnotationSchema "defaultTinkerpopAnnotations" $ toField <$> [
--         (_AnnotationSchema_vertexId, "id"),
--         (_AnnotationSchema_edgeId, "id"),
--         (_AnnotationSchema_outVertex, "out"),
--         (_AnnotationSchema_inVertex, "in"),
--         (_AnnotationSchema_outVertex, "outId"),
--         (_AnnotationSchema_inVertex, "inId"),
--         (_AnnotationSchema_vertexLabel, "label"),
--         (_AnnotationSchema_edgeLabel, "label"),
--         (_AnnotationSchema_ignore, "ignore")],

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
        lambda "s" $ lambda "a" $ lambda "t" $ lambda "v" $ lambda "e" $ lambda "p" $
          record [
            "vertexIds">: compute "Coder" @@ "s" @@ "s" @@ (core "Term" @@ "a") @@ "v",
            "edgeIds">: compute "Coder" @@ "s" @@ "s" @@ (core "Term" @@ "a") @@ "e",
            "propertyTypes">: compute "Coder" @@ "s" @@ "s" @@ (core "Type" @@ "a") @@ "t",
            "propertyValues">: compute "Coder" @@ "s" @@ "s" @@ (core "Term" @@ "a") @@ "p",
            "annotations">: mappings "AnnotationSchema"],

      def "ValueSpec" $
        doc "A mapping specification producing values (usually literal values) whose type is understood in context" $
        union [
          "value">:
            doc "A trivial no-op specification which passes the entire value"
            unit,
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
