-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for sorting. This module includes an implementation of Tarjan's algorithm, originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC.

module Hydra.Sorting where

import qualified Hydra.Constants as Constants
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
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

-- | Given a list of adjacency lists represented as (key, [key]) pairs, construct a graph along with a function mapping each vertex (an Int) back to its original key.
adjacencyListsToGraph :: Ord t0 => ([(t0, [t0])] -> (M.Map Int [Int], (Int -> t0)))
adjacencyListsToGraph edges0 =

      let sortedEdges = Lists.sortOn Pairs.first edges0
          indexedEdges = Lists.zip (Math.range 0 (Lists.length sortedEdges)) sortedEdges
          keyToVertex =
                  Maps.fromList (Lists.map (\vkNeighbors ->
                    let v = Pairs.first vkNeighbors
                        kNeighbors = Pairs.second vkNeighbors
                        k = Pairs.first kNeighbors
                    in (k, v)) indexedEdges)
          vertexMap =
                  Maps.fromList (Lists.map (\vkNeighbors ->
                    let v = Pairs.first vkNeighbors
                        kNeighbors = Pairs.second vkNeighbors
                        k = Pairs.first kNeighbors
                    in (v, k)) indexedEdges)
          graph =
                  Maps.fromList (Lists.map (\vkNeighbors ->
                    let v = Pairs.first vkNeighbors
                        kNeighbors = Pairs.second vkNeighbors
                        neighbors = Pairs.second kNeighbors
                    in (v, (Maybes.mapMaybe (\k -> Maps.lookup k keyToVertex) neighbors))) indexedEdges)
          vertexToKey = \v -> Maybes.fromJust (Maps.lookup v vertexMap)
      in (graph, vertexToKey)

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

-- | Initial state for Tarjan's algorithm
initialState :: Topology.TarjanState
initialState =
    Topology.TarjanState {
      Topology.tarjanStateCounter = 0,
      Topology.tarjanStateIndices = Maps.empty,
      Topology.tarjanStateLowLinks = Maps.empty,
      Topology.tarjanStateStack = [],
      Topology.tarjanStateOnStack = Sets.empty,
      Topology.tarjanStateSccs = []}

-- | Pop vertices off the stack until the given vertex is reached, collecting the current strongly connected component
popStackUntil :: Int -> Topology.TarjanState -> ([Int], Topology.TarjanState)
popStackUntil v st0 =

      let go =
              \acc -> \st ->
                let x = Lists.head (Topology.tarjanStateStack st)
                    xs = Lists.tail (Topology.tarjanStateStack st)
                    newSt =
                            Topology.TarjanState {
                              Topology.tarjanStateCounter = (Topology.tarjanStateCounter st),
                              Topology.tarjanStateIndices = (Topology.tarjanStateIndices st),
                              Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks st),
                              Topology.tarjanStateStack = xs,
                              Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack st),
                              Topology.tarjanStateSccs = (Topology.tarjanStateSccs st)}
                    newSt2 =
                            Topology.TarjanState {
                              Topology.tarjanStateCounter = (Topology.tarjanStateCounter newSt),
                              Topology.tarjanStateIndices = (Topology.tarjanStateIndices newSt),
                              Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks newSt),
                              Topology.tarjanStateStack = (Topology.tarjanStateStack newSt),
                              Topology.tarjanStateOnStack = (Sets.delete x (Topology.tarjanStateOnStack st)),
                              Topology.tarjanStateSccs = (Topology.tarjanStateSccs newSt)}
                    acc_ = Lists.cons x acc
                in (Logic.ifElse (Equality.equal x v) (Lists.reverse acc_, newSt2) (go acc_ newSt2))
      in (go [] st0)

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

-- | Visit a vertex and recursively explore its successors
strongConnect :: M.Map Int [Int] -> Int -> Topology.TarjanState -> Topology.TarjanState
strongConnect graph v st =

      let i = Topology.tarjanStateCounter st
          newSt =
                  Topology.TarjanState {
                    Topology.tarjanStateCounter = (Math.add i 1),
                    Topology.tarjanStateIndices = (Maps.insert v i (Topology.tarjanStateIndices st)),
                    Topology.tarjanStateLowLinks = (Maps.insert v i (Topology.tarjanStateLowLinks st)),
                    Topology.tarjanStateStack = (Lists.cons v (Topology.tarjanStateStack st)),
                    Topology.tarjanStateOnStack = (Sets.insert v (Topology.tarjanStateOnStack st)),
                    Topology.tarjanStateSccs = (Topology.tarjanStateSccs st)}
          neighbors = Maps.findWithDefault [] v graph
          processNeighbor =
                  \st_ -> \w ->
                    let lowLink =
                            \s ->
                              let lowV1 = Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateLowLinks s)
                                  idx_w = Maps.findWithDefault Constants.maxInt32 w (Topology.tarjanStateIndices s)
                              in Topology.TarjanState {
                                Topology.tarjanStateCounter = (Topology.tarjanStateCounter s),
                                Topology.tarjanStateIndices = (Topology.tarjanStateIndices s),
                                Topology.tarjanStateLowLinks = (Maps.insert v (Equality.min lowV1 idx_w) (Topology.tarjanStateLowLinks s)),
                                Topology.tarjanStateStack = (Topology.tarjanStateStack s),
                                Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack s),
                                Topology.tarjanStateSccs = (Topology.tarjanStateSccs s)}
                    in (Logic.ifElse (Logic.not (Maps.member w (Topology.tarjanStateIndices st_))) (
                      let stAfter = strongConnect graph w st_
                          lowV2 = Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateLowLinks stAfter)
                          low_w = Maps.findWithDefault Constants.maxInt32 w (Topology.tarjanStateLowLinks stAfter)
                      in Topology.TarjanState {
                        Topology.tarjanStateCounter = (Topology.tarjanStateCounter stAfter),
                        Topology.tarjanStateIndices = (Topology.tarjanStateIndices stAfter),
                        Topology.tarjanStateLowLinks = (Maps.insert v (Equality.min lowV2 low_w) (Topology.tarjanStateLowLinks stAfter)),
                        Topology.tarjanStateStack = (Topology.tarjanStateStack stAfter),
                        Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack stAfter),
                        Topology.tarjanStateSccs = (Topology.tarjanStateSccs stAfter)}) (Logic.ifElse (Sets.member w (Topology.tarjanStateOnStack st_)) (lowLink st_) st_))
          stAfterNeighbors = Lists.foldl processNeighbor newSt neighbors
          low_v = Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateLowLinks stAfterNeighbors)
          idx_v = Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateIndices stAfterNeighbors)
      in (Logic.ifElse (Equality.equal low_v idx_v) (
        let compResult = popStackUntil v stAfterNeighbors
            comp = Pairs.first compResult
            stPopped = Pairs.second compResult
        in Topology.TarjanState {
          Topology.tarjanStateCounter = (Topology.tarjanStateCounter stPopped),
          Topology.tarjanStateIndices = (Topology.tarjanStateIndices stPopped),
          Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks stPopped),
          Topology.tarjanStateStack = (Topology.tarjanStateStack stPopped),
          Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack stPopped),
          Topology.tarjanStateSccs = (Lists.cons comp (Topology.tarjanStateSccs stPopped))}) stAfterNeighbors)

-- | Compute the strongly connected components of the given graph. The components are returned in reverse topological order
stronglyConnectedComponents :: M.Map Int [Int] -> [[Int]]
stronglyConnectedComponents graph =

      let verts = Maps.keys graph
          finalState =
                  Lists.foldl (\st -> \v -> Logic.ifElse (Maps.member v (Topology.tarjanStateIndices st)) st (strongConnect graph v st)) initialState verts
      in (Lists.reverse (Lists.map Lists.sort (Topology.tarjanStateSccs finalState)))

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

      let graphResult = adjacencyListsToGraph pairs
          g = Pairs.first graphResult
      in (Lists.map (\comp -> Lists.map (Pairs.second graphResult) comp) (stronglyConnectedComponents g))

-- | Sort a directed acyclic graph (DAG) of nodes using two helper functions: one for node keys, and one for the adjacency list of connected node keys. The result is a list of strongly-connected components (cycles), in which singleton lists represent acyclic nodes.
topologicalSortNodes :: Ord t1 => ((t0 -> t1) -> (t0 -> [t1]) -> [t0] -> [[t0]])
topologicalSortNodes getKey getAdj nodes =

      let nodesByKey = Maps.fromList (Lists.map (\n -> (getKey n, n)) nodes)
          pairs = Lists.map (\n -> (getKey n, (getAdj n))) nodes
          comps = topologicalSortComponents pairs
      in (Lists.map (\c -> Maybes.cat (Lists.map (\k -> Maps.lookup k nodesByKey) c)) comps)
