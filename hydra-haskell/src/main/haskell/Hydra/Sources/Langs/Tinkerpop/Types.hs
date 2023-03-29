{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Langs.Tinkerpop.Types where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types
import Hydra.Sources.Langs.Tinkerpop.V3


tinkerpopTypesModule :: Module Kv
tinkerpopTypesModule = Module ns elements [tinkerpopV3Module] $
    Just "A model for typed TinkerPop vertices, edges, and properties"
  where
    ns = Namespace "hydra/langs/tinkerpop/types"
    types = nsref ns
    v3 = nsref $ moduleNamespace tinkerpopV3Module
    def = datatype ns

    elements = [

      def "EdgeType" $
        doc "The type of a TinkerPop edge" $
        lambda "t" $
          record [
            "label">: v3 "EdgeLabel",
            "out">: types "VertexType" @@ "t",
            "in">: types "VertexType" @@ "t",
            "properties">: Types.map (v3 "PropertyKey") "t"],

      def "ElementKind" $
        doc "The kind of an element: vertex or edge" $
        enum ["vertex", "edge"],

      def "ElementType" $
        doc "The type of a TinkerPop vertex or edge" $
        lambda "t" $
        union [
          "vertex">: types "VertexType" @@ "t",
          "edge">: types "EdgeType" @@ "t"],

      def "PropertyType" $
        doc "The type of a TinkerPop property" $
        lambda "t" $
          record [
            "key">: v3 "PropertyKey",
            "value">: "t"],
        
      def "VertexType" $
        doc "The type of a TinkerPop vertex" $
        lambda "t" $
          record [
            "label">: v3 "VertexLabel",
            "properties">: Types.map (v3 "PropertyKey") "t"]]
