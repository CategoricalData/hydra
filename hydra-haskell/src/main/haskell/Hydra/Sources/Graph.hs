{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Graph where

import Hydra.Kernel
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Standard
import Hydra.Sources.Core


hydraGraphModule :: Module Kv
hydraGraphModule = Module ns elements [] $
    Just "The extension to graphs of Hydra's core type system (hydra/core)"
  where
    ns = Namespace "hydra/graph"
    core = nsref $ moduleNamespace hydraCoreModule
    graph = nsref ns
    def = datatype ns

    elements = [

      def "Element" $
        doc "A graph element, having a name, data term (value), and schema term (type)" $
        lambda "m" $ record [
          "name">: core "Name",
          "schema">: core "Term" @@ "m",
          "data">: core "Term" @@ "m"],

      def "Graph" $
        doc ("A graph, or set of named terms, together with its schema graph") $
        lambda "m" $ record [
          "elements">:
            doc "All of the elements in the graph" $
            Types.map (core "Name") (graph "Element" @@ "m"),
          "schema">:
            doc "The schema graph to this graph. If omitted, the graph is its own schema graph." $
            optional $ graph "Graph" @@ "m"]]
