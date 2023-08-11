{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Langs.Tinkerpop.PropertyGraph where

import Hydra.Compute
import Hydra.Flows
import Hydra.Module
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types

import Hydra.Sources.Core


tinkerpopPropertyGraphModule :: Module Kv
tinkerpopPropertyGraphModule = Module ns elements [hydraCoreModule] $
    Just "A typed property graph data model"
  where
    ns = Namespace "hydra/langs/tinkerpop/propertyGraph"
    pg = typeref ns
    def = datatype ns

    elements = [

      def "Direction" $
        doc "The direction of an edge" $
          enum ["out", "in", "both"],

      def "Edge" $
        doc "An edge" $
        lambda "v" $ lambda "e" $ lambda "p" $
        record [
          "label">: pg "EdgeLabel",
          "id">: "e",
          "out">: "v",
          "in">: "v",
          "properties">: Types.map (pg "PropertyKey") "p"],

      def "EdgeLabel" $
        doc "The (required) label of an edge" $
        string,

      def "EdgeType" $
        doc "The type of an edge" $
        lambda "e" $ lambda "p" $
          record [
            "label">: pg "EdgeLabel",
            "id">: "e",
            "out">: pg "VertexLabel",
            "in">: pg "VertexLabel",
            "properties">: Types.map (pg "PropertyKey") "p"],

      def "Element" $
        doc "Either a vertex or an edge" $
        lambda "v" $ lambda "e" $ lambda "p" $
        union [
          "vertex">: pg "Vertex" @@ "v" @@ "p",
          "edge">: pg "Edge" @@ "v" @@ "e" @@ "p"],

      def "ElementKind" $
        doc "The kind of an element: vertex or edge" $
        enum ["vertex", "edge"],

      def "ElementTree" $
        doc "An element together with its dependencies in some context" $
        lambda "v" $ lambda "e" $ lambda "p" $
        record [
          "primary">: pg "Element" @@ "v" @@ "e" @@ "p",
          "dependencies">: Types.list $ pg "ElementTree" @@ "v" @@ "e" @@ "p"],

      def "ElementType" $
        doc "The type of a vertex or edge" $
        lambda "v" $ lambda "e" $ lambda "p" $
        union [
          "vertex">: pg "VertexType" @@ "v" @@ "p",
          "edge">: pg "EdgeType" @@ "e" @@ "p"],

      def "ElementTypeTree" $
        doc "An element type together with its dependencies in some context" $
        lambda "v" $ lambda "e" $ lambda "p" $
        record [
          "primary">: pg "ElementType" @@ "v" @@ "e" @@ "p",
          "dependencies">: Types.list $ pg "ElementTypeTree" @@ "v" @@ "e" @@ "p"],

      def "Graph" $
        doc "A graph; a self-contained collection of vertices and edges" $
        lambda "v" $ lambda "e" $ lambda "p" $
        record [
          "vertices">: Types.map "v" $ pg "Vertex" @@ "v" @@ "p",
          "edges">: Types.map "e" $ pg "Edge" @@ "v" @@ "e" @@ "p"],

      def "Label" $
        doc "A vertex or edge label" $
        union [
          "vertex">: pg "VertexLabel",
          "edge">: pg "EdgeLabel"],

      def "Property" $
        doc "A key/value property" $
        lambda "p" $
        record [
          "key">: pg "PropertyKey",
          "value">: "p"],

      def "PropertyKey" $
        doc "A property key"
        string,

      def "PropertyType" $
        doc "The type of a property" $
        lambda "p" $
          record [
            "key">: pg "PropertyKey",
            "value">: "p"],

      def "Vertex" $
        doc "A vertex" $
        lambda "v" $ lambda "p" $
        record [
          "label">: pg "VertexLabel",
          "id">: "v",
          "properties">: Types.map (pg "PropertyKey") "p"],

      def "VertexLabel" $
        doc "The label of a vertex. The default (null) vertex is represented by the empty string" $
        string,

      def "VertexType" $
        doc "The type of a vertex" $
        lambda "v" $ lambda "p" $
          record [
            "label">: pg "VertexLabel",
            "id">: "v",
            "properties">: Types.map (pg "PropertyKey") "p"]]
