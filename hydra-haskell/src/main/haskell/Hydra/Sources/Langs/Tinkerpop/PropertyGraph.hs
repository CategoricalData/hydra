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
        lambda "t" $
        record [
          "label">: pg "EdgeLabel",
          "id">: "t",
          "out">: "t",
          "in">: "t",
          "properties">: Types.map (pg "PropertyKey") "t"],

      def "EdgeLabel" $
        doc "The (required) label of an edge" $
        string,

      def "EdgeType" $
        doc "The type of an edge" $
        lambda "t" $
          record [
            "label">: pg "EdgeLabel",
            "id">: "t",
            "out">: pg "VertexLabel",
            "in">: pg "VertexLabel",
            "properties">: Types.map (pg "PropertyKey") "t"],

      def "Element" $
        doc "Either a vertex or an edge" $
        lambda "v" $
        union [
          "vertex">: pg "Vertex" @@ "v",
          "edge">: pg "Edge" @@ "v"],

      def "ElementKind" $
        doc "The kind of an element: vertex or edge" $
        enum ["vertex", "edge"],

      def "ElementTree" $
        doc "An element together with its dependencies in some context" $
        lambda "v" $
        record [
          "primary">: pg "Element" @@ "v",
          "dependencies">: Types.list $ pg "ElementTree" @@ "v"],

      def "ElementType" $
        doc "The type of a vertex or edge" $
        lambda "t" $
        union [
          "vertex">: pg "VertexType" @@ "t",
          "edge">: pg "EdgeType" @@ "t"],

      def "ElementTypeTree" $
        doc "An element type together with its dependencies in some context" $
        lambda "t" $
        record [
          "primary">: pg "ElementType" @@ "t",
          "dependencies">: Types.list $ pg "ElementTypeTree" @@ "t"],

      def "Graph" $
        doc "A graph; a self-contained collection of vertices and edges" $
        lambda "v" $
        record [
          "vertices">: Types.map "v" $ pg "Vertex" @@ "v",
          "edges">: Types.map "v" $ pg "Edge" @@ "v"],

      def "Label" $
        doc "A vertex or edge label" $
        union [
          "vertex">: pg "VertexLabel",
          "edge">: pg "EdgeLabel"],

      def "Property" $
        doc "A key/value property" $
        lambda "v" $
        record [
          "key">: pg "PropertyKey",
          "value">: "v"],

      def "PropertyKey" $
        doc "A property key"
        string,

      def "PropertyType" $
        doc "The type of a property" $
        lambda "t" $
          record [
            "key">: pg "PropertyKey",
            "value">: "t"],

      def "Vertex" $
        doc "A vertex" $
        lambda "v" $
        record [
          "label">: pg "VertexLabel",
          "id">: "v",
          "properties">: Types.map (pg "PropertyKey") "v"],

      def "VertexLabel" $
        doc "The label of a vertex. The default (null) vertex is represented by the empty string" $
        string,

      def "VertexType" $
        doc "The type of a vertex" $
        lambda "t" $
          record [
            "label">: pg "VertexLabel",
            "id">: "t",
            "properties">: Types.map (pg "PropertyKey") "t"]]
