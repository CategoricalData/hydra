{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Graph where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraGraphModule :: Module Meta
hydraGraphModule = Module ns elements [hydraCoreModule] $
    Just "Abstractions for graphs, elements, and modules"
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
            optional $ graph "Graph" @@ "m"],

      def "Module" $
        doc "A logical collection of elements; a graph subset with dependencies on zero or more other subsets" $
        lambda "m" $ record [
          "namespace">:
            doc "A common prefix for all element names in the module" $
            graph "Namespace",
          "elements">:
            doc "The elements defined in this module" $
            list $ graph "Element" @@ "m",
          "dependencies">:
            doc "Any additional modules this one has a direct dependency upon" $
            list $ graph "Module" @@ "m",
          "description">:
            doc "An optional human-readable description of the module" $
            optional string],

      def "Namespace" $
        doc "A prefix for element names"
        string]
