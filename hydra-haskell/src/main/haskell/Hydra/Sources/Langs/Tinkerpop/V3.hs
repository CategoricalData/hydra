{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Langs.Tinkerpop.V3 where

import Hydra.Compute ( Kv )
import Hydra.Flows ( Kv )
import Hydra.Module
    ( Module(Module, moduleNamespace), Namespace(Namespace) )
import Hydra.Dsl.Annotations
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types


tinkerpopV3Module :: Module Kv
tinkerpopV3Module = Module ns elements [hydraCoreModule] $
    Just "A typed property graph data model"
  where
    ns = Namespace "hydra/langs/tinkerpop/v3"
    core = nsref $ moduleNamespace hydraCoreModule
    v3 = nsref ns
    def = datatype ns

    elements = [

      def "Edge" $
        doc "An edge" $
        lambda "v" $ lambda "e" $ lambda "p" $
        record [
          "label">: v3 "EdgeLabel",
          "id">: "e",
          "out">: "v",
          "in">: "v",
          "properties">: Types.map (v3 "PropertyKey") "p"],

      def "EdgeLabel" $
        doc "The (required) label of an edge" $
        string,

      def "EdgeType" $
        doc "The type of an edge" $
        lambda "t" $
          record [
            "label">: v3 "EdgeLabel",
            "out">: v3 "VertexType" @@ "t",
            "in">: v3 "VertexType" @@ "t",
            "properties">: Types.map (v3 "PropertyKey") "t"],

      def "Element" $
        doc "Either a vertex or an edge" $
        lambda "v" $ lambda "e" $ lambda "p" $
        union [
          "vertex">: v3 "Vertex" @@ "v" @@ "p",
          "edge">: v3 "Edge" @@ "v" @@ "e" @@ "p"],

      def "ElementKind" $
        doc "The kind of an element: vertex or edge" $
        enum ["vertex", "edge"],

      def "ElementType" $
        doc "The type of a vertex or edge" $
        lambda "t" $
        union [
          "vertex">: v3 "VertexType" @@ "t",
          "edge">: v3 "EdgeType" @@ "t"],

      def "Graph" $
        doc "A graph; a self-contained collection of vertices and edges" $
        lambda "v" $ lambda "e" $ lambda "p" $
        record [
          "vertices">: Types.set $ v3 "Vertex" @@ "v" @@ "p",
          "edges">: Types.set $ v3 "Edge" @@ "v" @@ "e" @@ "p"],

      def "Label" $
        doc "A vertex or edge label" $
        union [
          "vertex">: v3 "VertexLabel",
          "edge">: v3 "EdgeLabel"],

      def "Property" $
        doc "A key/value property" $
        lambda "p" $
        record [
          "key">: v3 "PropertyKey",
          "value">: "p"],

      def "PropertyKey" $
        doc "A property key"
        string,

      def "PropertyType" $
        doc "The type of a property" $
        lambda "t" $
          record [
            "key">: v3 "PropertyKey",
            "value">: "t"],

      def "Vertex" $
        doc "A vertex" $
        lambda "v" $ lambda "p" $
        record [
          "label">: v3 "VertexLabel",
          "id">: "v",
          "properties">: Types.map (v3 "PropertyKey") "p"],

      def "VertexLabel" $
        doc "The label of a vertex. The default (null) vertex is represented by the empty string" $
        string,
        
      def "VertexType" $
        doc "The type of a vertex" $
        lambda "t" $
          record [
            "label">: v3 "VertexLabel",
            "properties">: Types.map (v3 "PropertyKey") "t"]]
