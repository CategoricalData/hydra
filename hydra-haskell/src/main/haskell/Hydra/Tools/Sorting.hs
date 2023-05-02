-- | Utilities for sorting

module Hydra.Tools.Sorting (
  topologicalSort,
  topologicalSortComponents,
) where

import qualified Data.List as L
import qualified Data.Bifunctor as BF

import qualified Data.Graph as G


-- | Sort a directed acyclic graph (DAG) based on an adjacency list
--   Note: assumes that the input is in fact a DAG; the ordering is incomplete in the presence of cycles.
topologicalSort :: Ord a => [(a, [a])] -> Maybe [a]
topologicalSort pairs = Just (getKey nodeFromVertex <$> G.topSort g)
  where
    (g, nodeFromVertex) = initGraph pairs

-- | Find the strongly connected components of a graph, in (reverse) topological order
topologicalSortComponents :: Ord a => [(a, [a])] -> [[a]]
topologicalSortComponents pairs = (fmap (getKey nodeFromVertex) . treeToList) <$> G.scc g
  where
    (g, nodeFromVertex) = initGraph pairs

treeToList :: G.Tree a -> [a]
treeToList (G.Node root subforest) = root:(L.concat (treeToList <$> subforest))

getKey :: (G.Vertex -> ((), a, [a])) -> G.Vertex -> a
getKey nodeFromVertex v = n
  where
    (_, n, _) = nodeFromVertex v

initGraph :: Ord a => [(a, [a])] -> (G.Graph, G.Vertex -> ((), a, [a]))
initGraph pairs = (g, nodeFromVertex)
  where
    (g, nodeFromVertex, _) = G.graphFromEdges (toEdge <$> pairs)
    toEdge (key, keys) = ((), key, keys)
