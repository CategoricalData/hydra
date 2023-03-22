-- | Utilities for sorting

module Hydra.Tools.Sorting where

import qualified Data.List as L
import qualified Data.Bifunctor as BF

import qualified Data.Graph as G


-- | Sort a directed acyclic graph (DAG) based on an adjacency list
--   Note: assumes that the input is in fact a DAG; the ordering is incomplete in the presence of cycles.
topologicalSort :: Ord a => [(a, [a])] -> Maybe [a]
topologicalSort pairs = Just (getKey <$> G.topSort g)
  where
    (g, nodeFromVertex, _) = G.graphFromEdges (toEdge <$> pairs)
    toEdge (key, keys) = ((), key, keys)
    getKey v = n
      where
        (_, n, _) = nodeFromVertex v
