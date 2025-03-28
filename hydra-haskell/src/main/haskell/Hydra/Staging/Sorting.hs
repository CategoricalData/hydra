-- | Utilities for sorting

module Hydra.Staging.Sorting (
  OrderingIsomorphism(..),
  createOrderingIsomorphism,
  topologicalSort,
  topologicalSortComponents
) where

import Hydra.Staging.Tarjan

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


-- Note: useful, but currently unused
data OrderingIsomorphism a = OrderingIsomorphism {
  orderingIsomorphismEncode :: [a] -> [a],
  orderingIsomorphismDecode :: [a] -> [a]}

-- Note: useful, but currently unused
createOrderingIsomorphism :: Ord a => [a] -> [a] -> OrderingIsomorphism b
createOrderingIsomorphism sourceOrd targetOrd = OrderingIsomorphism sourceToTargetMapping targetToSourceMapping
  where
    sourceToTargetMapping els = Y.catMaybes $ fmap (\n -> M.lookup n mp) targetOrd
      where
        mp = M.fromList $ L.zip sourceOrd els
    targetToSourceMapping els = Y.catMaybes $ fmap (\n -> M.lookup n mp) sourceOrd
      where
        mp = M.fromList $ L.zip targetOrd els

-- | Sort a directed acyclic graph (DAG) based on an adjacency list
--   Yields a list of nontrivial strongly connected components if the graph has cycles, otherwise a simple list.
topologicalSort :: Ord a => [(a, [a])] -> Either [[a]] [a]
topologicalSort pairs = if L.null withCycles
    then Right $ L.concat sccs
    else Left withCycles
  where
    sccs = topologicalSortComponents pairs
    withCycles = L.filter isCycle sccs
    isCycle = not . L.null . L.tail

-- | Find the strongly connected components (including cycles and isolated vertices) of a graph,
--   in (reverse) topological order, i.e. dependencies before dependents
topologicalSortComponents :: Ord a => [(a, [a])] -> [[a]]
topologicalSortComponents pairs = fmap (fmap getKey) $ stronglyConnectedComponents g
  where
    (g, getKey) = adjacencyListsToGraph (toEdge <$> pairs)
      where
        toEdge (key, keys) = (key, keys)
