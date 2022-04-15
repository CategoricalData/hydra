module Hydra.Sorting where

import qualified Data.List as L
import qualified Data.Bifunctor as BF


-- Note: requires a DAG (the ordering is incomplete in the presence of cycles).
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
