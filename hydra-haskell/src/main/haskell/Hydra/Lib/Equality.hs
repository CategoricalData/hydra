-- | Haskell implementations of hydra.lib.equality primitives. These simply make use of derived Eq.

module Hydra.Lib.Equality where

import Hydra.Util

import Data.Int


-- | Compare two values and return a Comparison.
compare :: Ord a => a -> a -> Comparison
compare x y
  | x < y     = ComparisonLessThan
  | x > y     = ComparisonGreaterThan
  | otherwise = ComparisonEqualTo

-- | Check if two values are equal.
equal :: Eq a => a -> a -> Bool
equal = (==)

-- | Check if first value is greater than second.
gt :: Ord a => a -> a -> Bool
gt = (>)

-- | Check if first value is greater than or equal to second.
gte :: Ord a => a -> a -> Bool
gte = (>=)

-- | Return the identity of a value.
identity :: a -> a
identity = id

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
