{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Sorting where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes as Maybes
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
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Tarjan as Tarjan

import qualified Hydra.Topology as Topo


module_ :: Module
module_ = Module (Namespace "hydra.sorting") elements
    [Tarjan.module_]
    kernelTypesModules $
    Just ("Utilities for sorting.")
  where
   elements = [
     el createOrderingIsomorphismDef,
     el topologicalSortDef,
     el topologicalSortComponentsDef,
     el topologicalSortNodesDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

createOrderingIsomorphismDef :: TBinding ([a] -> [a] -> Topo.OrderingIsomorphism b)
createOrderingIsomorphismDef = define "createOrderingIsomorphism" $
  withOrd "t0" $ "sourceOrd" ~> "targetOrd" ~>
  "sourceToTargetMapping" <~ ("els" ~>
    "mp" <~ Maps.fromList (Lists.zip (var "sourceOrd") (var "els")) $
    Maybes.cat $ Lists.map ("n" ~> Maps.lookup (var "n") (var "mp")) (var "targetOrd")) $
  "targetToSourceMapping" <~ ("els" ~>
    "mp" <~ Maps.fromList (Lists.zip (var "targetOrd") (var "els")) $
    Maybes.cat $ Lists.map ("n" ~> Maps.lookup (var "n") (var "mp")) (var "sourceOrd")) $
  Topology.orderingIsomorphism (var "sourceToTargetMapping") (var "targetToSourceMapping")

topologicalSortDef :: TBinding ([(a, [a])] -> Either [[a]] [a])
topologicalSortDef = define "topologicalSort" $
  doc ("Sort a directed acyclic graph (DAG) based on an adjacency list."
    <> " Yields a list of nontrivial strongly connected components if the graph has cycles, otherwise a simple list.") $
  withOrd "t0" $ "pairs" ~>
  "sccs" <~ ref topologicalSortComponentsDef @@ var "pairs" $
  "isCycle" <~ ("scc" ~> Logic.not $ Lists.null $ Lists.tail $ var "scc") $
  "withCycles" <~ Lists.filter (var "isCycle") (var "sccs") $
  Logic.ifElse (Lists.null $ var "withCycles")
    (Mantle.eitherRight $ Lists.concat $ var "sccs")
    (Mantle.eitherLeft $ var "withCycles")

topologicalSortComponentsDef :: TBinding ([(a, [a])] -> [[a]])
topologicalSortComponentsDef = define "topologicalSortComponents" $
  doc ("Find the strongly connected components (including cycles and isolated vertices) of a graph,"
    <> " in (reverse) topological order, i.e. dependencies before dependents") $
  withOrd "t0" $ "pairs" ~>
  "graphResult" <~ ref Tarjan.adjacencyListsToGraphDef @@ var "pairs" $
  "g" <~ first (var "graphResult") $
  "getKey" <~ second (var "graphResult") $
  Lists.map ("comp" ~> Lists.map (var "getKey") (var "comp")) $
    ref Tarjan.stronglyConnectedComponentsDef @@ var "g"

topologicalSortNodesDef :: TBinding ((x -> a) -> (x -> [a]) -> [x] -> [[x]])
topologicalSortNodesDef = define "topologicalSortNodes" $
  doc ("Sort a directed acyclic graph (DAG) of nodes using two helper functions:"
    <> " one for node keys, and one for the adjacency list of connected node keys."
    <> " The result is a list of strongly-connected components (cycles), in which singleton lists represent acyclic nodes.") $
  withOrd "t1" $ "getKey" ~> "getAdj" ~> "nodes" ~>
  "nodesByKey" <~ Maps.fromList (Lists.map ("n" ~> pair (var "getKey" @@ var "n") (var "n")) (var "nodes")) $
  "pairs" <~ Lists.map ("n" ~> pair (var "getKey" @@ var "n") (var "getAdj" @@ var "n")) (var "nodes") $
  "comps" <~ ref topologicalSortComponentsDef @@ var "pairs" $
  Lists.map ("c" ~> Maybes.cat $ Lists.map ("k" ~> Maps.lookup (var "k") (var "nodesByKey")) (var "c")) (var "comps")
