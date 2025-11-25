{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Sorting where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
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
     el findReachableNodesDef,
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

-- Note: not a sorting function
findReachableNodesDef :: TBinding ((a -> S.Set a) -> a -> S.Set a)
findReachableNodesDef = define "findReachableNodes" $
  doc "Given an adjacency function and a distinguished root node, find all reachable nodes (including the root node)" $
  "adj" ~> "root" ~>
  "visit" <~ ("visited" ~> "node" ~>
    "toVisit" <~ Sets.difference (var "adj" @@ var "node") (var "visited") $
    Logic.ifElse (Sets.null $ var "toVisit")
      (var "visited")
      (Lists.foldl
        ("v" ~> "n" ~> var "visit" @@ Sets.insert (var "n") (var "v") @@ var "n")
        (var "visited")
        (Sets.toList $ var "toVisit"))) $
  var "visit" @@ Sets.singleton (var "root") @@ var "root"

topologicalSortDef :: TBinding ([(a, [a])] -> Either [[a]] [a])
topologicalSortDef = define "topologicalSort" $
  doc ("Sort a directed acyclic graph (DAG) based on an adjacency list."
    <> " Yields a list of nontrivial strongly connected components if the graph has cycles, otherwise a simple list.") $
  withOrd "t0" $ "pairs" ~>
  "sccs" <~ ref topologicalSortComponentsDef @@ var "pairs" $
  "isCycle" <~ ("scc" ~> Logic.not $ Lists.null $ Lists.tail $ var "scc") $
  "withCycles" <~ Lists.filter (var "isCycle") (var "sccs") $
  Logic.ifElse (Lists.null $ var "withCycles")
    (right $ Lists.concat $ var "sccs")
    (left $ var "withCycles")

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
  "nodesByKey" <~ Maps.fromList (Lists.map ("n" ~> tuple2 (var "getKey" @@ var "n") (var "n")) (var "nodes")) $
  "pairs" <~ Lists.map ("n" ~> tuple2 (var "getKey" @@ var "n") (var "getAdj" @@ var "n")) (var "nodes") $
  "comps" <~ ref topologicalSortComponentsDef @@ var "pairs" $
  Lists.map ("c" ~> Maybes.cat $ Lists.map ("k" ~> Maps.lookup (var "k") (var "nodesByKey")) (var "c")) (var "comps")
