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
hydraGraph = Graph hydraGraphName elements (const True) hydraCoreName
  where
    core = nsref hydraCoreName
    graph = nsref hydraGraphName
    def = datatype hydraGraphName
    
    elements = [

      def "Element" $
        doc "A graph element, having a name, data term (value), and schema term (type)" $
        universal "m" $ record [
          field "name" $ core "Name",
          field "schema" $ universal "m" $ core "Term",
          field "data" $ universal "m" $ core "Term"],

      def "Graph" $
        doc ("A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph,"
          ++ " called the schema graph") $
        universal "m" $ record [
          field "name" $ graph "GraphName",
          field "elements" $ list $ universal "m" $ graph "Element",
          field "termExprs" $ universal "m" (core "Term") --> boolean,
          field "schemaGraph" $
            doc "A reference to this graph's schema graph within the provided graph set" $
            graph "GraphName"],

      def "GraphName" $
        doc "A unique identifier for a graph within a graph set"
        string,

      def "GraphSet" $
        doc "A collection of graphs with a distinguished root graph" $
        universal "m" $ record [
          field "graphs" $ Types.map (graph "GraphName") (universal "m" $ graph "Graph"),
          field "root" $ graph "GraphName"],

     def "Module" $
       doc "A logical collection of elements; a graph subset with dependencies on zero or more other subsets" $
       universal "m" $ record [
         field "graph" $ universal "m" $ graph "Graph",
         field "imports" $ list $ universal "m" $ graph "Module"]]
