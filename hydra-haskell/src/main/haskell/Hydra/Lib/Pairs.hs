-- | Haskell implementations of hydra.lib.pairs primitives

module Hydra.Lib.Pairs where

import qualified Data.Bifunctor as BF


-- | Map over both elements of a pair.
bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimap = BF.bimap

-- | Get the first element of a pair.
first :: (a, b) -> a
first (x, _) = x

-- | Get the second element of a pair.
second :: (a, b) -> b
second (_, y) = y
