-- | Haskell implementations of hydra.lib.equality primitives.

module Hydra.Overlay.Haskell.Lib.Equality where


-- | Check if two values are equal.
equal :: Eq a => a -> a -> Bool
equal = (==)

-- | Check if two values are unequal.
notEqual :: Eq a => a -> a -> Bool
notEqual = (/=)
