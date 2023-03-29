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
        lambda "v" $ lambda "e" $ lambda "p" $
          record [
            "label">: v3 "EdgeLabel",
            "id">: "e",
            "out">: types "VertexType" @@ "v" @@ "p",
            "in">: types "VertexType" @@ "v" @@ "p",
            "properties">: Types.map (v3 "PropertyKey") "p"],

      def "ElementType" $
        doc "The type of a Tinkerpop vertex or edge" $
        lambda "v" $ lambda "e" $ lambda "p" $
        union [
          "vertex">: types "VertexType" @@ "v" @@ "p",
          "edge">: types "EdgeType" @@ "v" @@ "e" @@ "p"],

      def "VertexType" $
        doc "The type of a TinkerPop vertex" $
        lambda "v" $ lambda "p" $
          record [
            "label">: v3 "VertexLabel",
            "id">: "v",
            "properties">: Types.map (v3 "PropertyKey") "p"]]
