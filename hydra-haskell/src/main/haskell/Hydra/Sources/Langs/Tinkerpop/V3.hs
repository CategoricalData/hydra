{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Langs.Tinkerpop.V3 where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types


tinkerpopV3Module :: Module Kv
tinkerpopV3Module = Module ns elements [hydraCoreModule] $
    Just "A simple TinkerPop version 3 syntax model"
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

      def "Element" $
        doc "Either a vertex or an edge" $
        lambda "v" $ lambda "e" $ lambda "p" $
        union [
          "vertex">: v3 "Vertex" @@ "v" @@ "p",
          "edge">: v3 "Edge" @@ "v" @@ "e" @@ "p"],

      def "Graph" $
        doc "A graph; a self-contained collection of vertices and edges" $
        lambda "v" $ lambda "e" $ lambda "p" $
        record [
          "vertices">: Types.set $ v3 "Vertex" @@ "v" @@ "p",
          "edges">: Types.set $ v3 "Edge" @@ "v" @@ "e" @@ "p"],

      def "Property" $
        doc "A key/value property" $
        lambda "p" $
        record [
          "key">: v3 "PropertyKey",
          "value">: "p"],

      def "PropertyKey" $
        doc "A property key"
        string,

      def "Vertex" $
        doc "A vertex" $
        lambda "v" $ lambda "p" $
        record [
          "label">: v3 "VertexLabel",
          "id">: "v",
          "properties">: Types.map (v3 "PropertyKey") "p"],

      def "VertexLabel" $
        doc "The label of a vertex. The default (null) vertex is represented by the empty string" $
        string]
