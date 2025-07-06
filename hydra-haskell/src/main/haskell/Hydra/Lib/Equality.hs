-- | Haskell implementations of hydra.lib.equality primitives. These simply make use of derived Eq.

module Hydra.Lib.Equality where

import Hydra.Core
import Hydra.Graph

import Data.Int


compare :: Ord a => a -> a -> Comparison
compare x y
  | x < y     = ComparisonLessThan
  | x > y     = ComparisonGreaterThan
  | otherwise = ComparisonEqualTo

equal :: Eq a => a -> a -> Bool
equal = (==)

gt :: Ord a => a -> a -> Bool
gt = (>)

gte :: Ord a => a -> a -> Bool
gte = (>=)

identity :: a -> a
identity = id

lt :: Ord a => a -> a -> Bool
lt = (<)

lte :: Ord a => a -> a -> Bool
lte = (<=)
