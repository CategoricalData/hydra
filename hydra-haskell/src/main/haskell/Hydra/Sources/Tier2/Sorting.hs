{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Sorting where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Sources.Tier1.All as Tier1
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Tier2.Tarjan as Tarjan

import qualified Hydra.Topology as Topo


sortingDefinition :: String -> TTerm a -> TElement a
sortingDefinition = definitionInModule hydraSortingModule

hydraSortingModule :: Module
hydraSortingModule = Module (Namespace "hydra.sorting") elements
    [Tarjan.tarjanModule]
    [Tier1.hydraComputeModule, Tier1.hydraMantleModule, Tier1.hydraTopologyModule] $
    Just ("Utilities for sorting.")
  where
   elements = [
     el createOrderingIsomorphismDef,
     el topologicalSortDef,
     el topologicalSortComponentsDef]

createOrderingIsomorphismDef :: TElement ([a] -> [a] -> Topo.OrderingIsomorphism b)
createOrderingIsomorphismDef = sortingDefinition "createOrderingIsomorphism" $
  withOrd "t0" $
  lambdas ["sourceOrd", "targetOrd"] $ lets [
    "sourceToTargetMapping">: lambda "els" $ lets [
      "mp">: Maps.fromList $ Lists.zip (var "sourceOrd") (var "els")]
      $ Optionals.cat $ Lists.map (lambda "n" $ Maps.lookup (var "n") (var "mp")) (var "targetOrd"),
    "targetToSourceMapping">: lambda "els" $ lets [
      "mp">: Maps.fromList $ Lists.zip (var "targetOrd") (var "els")]
      $ Optionals.cat $ Lists.map (lambda "n" $ Maps.lookup (var "n") (var "mp")) (var "sourceOrd")]
    $ Topology.orderingIsomorphism (var "sourceToTargetMapping") (var "targetToSourceMapping")

topologicalSortDef :: TElement ([(a, [a])] -> Either [[a]] [a])
topologicalSortDef = sortingDefinition "topologicalSort" $
  doc "Sort a directed acyclic graph (DAG) based on an adjacency list. Yields a list of nontrivial strongly connected components if the graph has cycles, otherwise a simple list." $
  withOrd "t0" $
  lambda "pairs" $ lets [
    "sccs">: ref topologicalSortComponentsDef @@ var "pairs",
    "isCycle">: lambda "scc" $ Logic.not $ Lists.null $ Lists.tail $ var "scc",
    "withCycles">: Lists.filter (var "isCycle") (var "sccs")]
    $ Logic.ifElse (Lists.null $ var "withCycles")
      (Mantle.eitherRight $ Lists.concat $ var "sccs")
      (Mantle.eitherLeft $ var "withCycles")

topologicalSortComponentsDef :: TElement ([(a, [a])] -> [[a]])
topologicalSortComponentsDef = sortingDefinition "topologicalSortComponents" $
  doc "Find the strongly connected components (including cycles and isolated vertices) of a graph, in (reverse) topological order, i.e. dependencies before dependents" $
  withOrd "t0" $
  lambda "pairs" $ lets [
    "graphResult">: ref Tarjan.adjacencyListsToGraphDef @@ var "pairs",
    "g">: first $ var "graphResult",
    "getKey">: second $ var "graphResult"]
    $ Lists.map (lambda "component" $ Lists.map (var "getKey") (var "component")) $
      ref Tarjan.stronglyConnectedComponentsDef @@ var "g"
