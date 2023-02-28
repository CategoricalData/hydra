-- | Utilities for sorting

module Hydra.Tools.Sorting where

import qualified Data.List as L
import qualified Data.Bifunctor as BF


-- | Sort a directed acyclic graph (DAG) based on an adjacency list
--   Note: assumes that the input is in fact a DAG; the ordering is incomplete in the presence of cycles.
topologicalSort :: Eq a => [(a, [a])] -> Maybe [a]
topologicalSort pairs = if L.length result < L.length pairs
    then Nothing
    else Just result
  where
    result = foldl makePrecede [] pairs
    makePrecede ts (x, xs) = L.nub $
      case L.elemIndex x ts of
        Just i -> uncurry (++) $ BF.first (++ xs) $ splitAt i ts
        _ -> ts ++ xs ++ [x]
