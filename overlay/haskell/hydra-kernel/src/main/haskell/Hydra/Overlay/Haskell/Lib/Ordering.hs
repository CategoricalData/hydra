-- | Haskell implementations of hydra.lib.ordering primitives.

module Hydra.Overlay.Haskell.Lib.Ordering where

import Hydra.Util


-- | Compare two values and return a Comparison.
compare :: Ord a => a -> a -> Comparison
compare x y
  | x < y     = ComparisonLessThan
  | x > y     = ComparisonGreaterThan
  | otherwise = ComparisonEqualTo

-- | Check if first value is greater than second.
gt :: Ord a => a -> a -> Bool
gt = (>)

-- | Check if first value is greater than or equal to second.
gte :: Ord a => a -> a -> Bool
gte = (>=)

-- | Check if first value is less than second.
lt :: Ord a => a -> a -> Bool
lt = (<)

-- | Check if first value is less than or equal to second.
lte :: Ord a => a -> a -> Bool
lte = (<=)

-- | Return the maximum of two values.
max :: Ord a => a -> a -> a
max = Prelude.max

-- | Return the minimum of two values.
min :: Ord a => a -> a -> a
min = Prelude.min
