{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Graph where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraGraphModule :: Module Meta
hydraGraphModule = Module hydraGraph [hydraCoreModule]

-- Note: here, the element namespace doubles as a graph name
hydraGraphName :: GraphName
hydraGraphName = GraphName "hydra/graph"

hydraGraph :: Graph Meta
hydraGraph = Graph hydraGraphName elements hydraCoreName
  where
    core = nsref hydraCoreName
    graph = nsref hydraGraphName
    def = datatype hydraGraphName
    
    elements = [

      def "Element" $
        doc "A graph element, having a name, data term (value), and schema term (type)" $
        lambda "m" $ record [
          "name">: core "Name",
          "schema">: core "Term" @@ "m",
          "data">: core "Term" @@ "m"],

      def "Graph" $
        doc ("A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph,"
          ++ " called the schema graph") $
        lambda "m" $ record [
          "name">: 
            doc "The unique (within a given graph set) name of the graph" $
            graph "GraphName",
          "elements">:
            doc "All of the elements in the graph" $
            list $ graph "Element" @@ "m",
          "schemaGraph">:
            doc "A reference to this graph's schema graph within the provided graph set" $
            graph "GraphName"],

      def "GraphName" $
        doc "A unique identifier for a graph within a graph set"
        string,

      def "GraphSet" $
        doc "A collection of graphs with a distinguished root graph" $
        lambda "m" $ record [
          "graphs">: Types.map (graph "GraphName") (graph "Graph" @@ "m"),
          "root">: graph "GraphName"],

     def "Module" $
       doc "A logical collection of elements; a graph subset with dependencies on zero or more other subsets" $
       lambda "m" $ record [
         "graph">: graph "Graph" @@ "m",
         "imports">: list $ graph "Module" @@ "m"]]
