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
    typed = nsref ns
    v3 = nsref $ moduleNamespace tinkerpopV3Module
    def = datatype ns

    elements = [

      def "EdgeType" $
        doc "The type of a TinkerPop edge" $
        lambda "v" $ lambda "e" $ lambda "p" $
          record [
            "label">: v3 "EdgeLabel",
            "id">: "e",
            "out">: typed "VertexType" @@ "v" @@ "p",
            "in">: typed "VertexType" @@ "v" @@ "p",
            "properties">: Types.map (v3 "PropertyKey") "p"],

      def "VertexType" $
        doc "The type of a TinkerPop vertex" $
        lambda "v" $ lambda "p" $
          record [
            "label">: v3 "VertexLabel",
            "id">: "v",
            "properties">: Types.map (v3 "PropertyKey") "p"]]
