-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for sorting.

module Hydra.Sorting where

import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Tarjan as Tarjan
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert an adjacency list to a map, concatenating values for duplicate keys
adjacencyListToMap :: Ord t0 => ([(t0, [t1])] -> M.Map t0 [t1])
adjacencyListToMap pairs =
    Lists.foldl (\mp -> \p ->
      let k = Pairs.first p
          vs = Pairs.second p
          existing = Maybes.maybe [] Equality.identity (Maps.lookup k mp)
      in (Maps.insert k (Lists.concat2 existing vs) mp)) Maps.empty pairs

createOrderingIsomorphism :: Ord t0 => ([t0] -> [t0] -> Topology.OrderingIsomorphism t1)
createOrderingIsomorphism sourceOrd targetOrd =

      let sourceToTargetMapping =
              \els ->
                let mp = Maps.fromList (Lists.zip sourceOrd els)
                in (Maybes.cat (Lists.map (\n -> Maps.lookup n mp) targetOrd))
          targetToSourceMapping =
                  \els ->
                    let mp = Maps.fromList (Lists.zip targetOrd els)
                    in (Maybes.cat (Lists.map (\n -> Maps.lookup n mp) sourceOrd))
      in Topology.OrderingIsomorphism {
        Topology.orderingIsomorphismEncode = sourceToTargetMapping,
        Topology.orderingIsomorphismDecode = targetToSourceMapping}

-- | Given an adjacency function and a distinguished root node, find all reachable nodes (including the root node)
findReachableNodes :: Ord t0 => ((t0 -> S.Set t0) -> t0 -> S.Set t0)
findReachableNodes adj root =

      let visit =
              \visited -> \node ->
                let toVisit = Sets.difference (adj node) visited
                in (Logic.ifElse (Sets.null toVisit) visited (Lists.foldl (\v -> \n -> visit (Sets.insert n v) n) visited (Sets.toList toVisit)))
      in (visit (Sets.singleton root) root)

-- | Given a graph as an adjacency list of edges and a list of explicit tags per node, compute the full set of tags for each node by propagating tags through edges. If there is an edge from n1 to n2 and n2 has tag t, then n1 also has tag t. Note: pairs in the output are not ordered.
propagateTags :: (Ord t0, Ord t1) => ([(t0, [t0])] -> [(t0, [t1])] -> [(t0, (S.Set t1))])
propagateTags edges nodeTags =

      let adjMap = adjacencyListToMap edges
          tagMap = Maps.map Sets.fromList (adjacencyListToMap nodeTags)
          allNodes = Sets.toList (Sets.fromList (Lists.concat2 (Lists.map Pairs.first edges) (Lists.map Pairs.first nodeTags)))
          getTagsForNode =
                  \node ->
                    let reachable = findReachableNodes (\n -> Sets.fromList (Maybes.maybe [] Equality.identity (Maps.lookup n adjMap))) node
                    in (Sets.unions (Lists.map (\n -> Maybes.maybe Sets.empty Equality.identity (Maps.lookup n tagMap)) (Sets.toList reachable)))
      in (Lists.map (\n -> (n, (getTagsForNode n))) allNodes)

-- | Sort a directed acyclic graph (DAG) based on an adjacency list. Yields a list of nontrivial strongly connected components if the graph has cycles, otherwise a simple list.
topologicalSort :: Ord t0 => ([(t0, [t0])] -> Either [[t0]] [t0])
topologicalSort pairs =

      let sccs = topologicalSortComponents pairs
          isCycle = \scc -> Logic.not (Lists.null (Lists.tail scc))
          withCycles = Lists.filter isCycle sccs
      in (Logic.ifElse (Lists.null withCycles) (Right (Lists.concat sccs)) (Left withCycles))

-- | Find the strongly connected components (including cycles and isolated vertices) of a graph, in (reverse) topological order, i.e. dependencies before dependents
topologicalSortComponents :: Ord t0 => ([(t0, [t0])] -> [[t0]])
topologicalSortComponents pairs =

      let graphResult = Tarjan.adjacencyListsToGraph pairs
          g = Pairs.first graphResult
      in (Lists.map (\comp -> Lists.map (Pairs.second graphResult) comp) (Tarjan.stronglyConnectedComponents g))

-- | Sort a directed acyclic graph (DAG) of nodes using two helper functions: one for node keys, and one for the adjacency list of connected node keys. The result is a list of strongly-connected components (cycles), in which singleton lists represent acyclic nodes.
topologicalSortNodes :: Ord t1 => ((t0 -> t1) -> (t0 -> [t1]) -> [t0] -> [[t0]])
topologicalSortNodes getKey getAdj nodes =

      let nodesByKey = Maps.fromList (Lists.map (\n -> (getKey n, n)) nodes)
          pairs = Lists.map (\n -> (getKey n, (getAdj n))) nodes
          comps = topologicalSortComponents pairs
      in (Lists.map (\c -> Maybes.cat (Lists.map (\k -> Maps.lookup k nodesByKey) c)) comps)
