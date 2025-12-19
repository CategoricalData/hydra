-- | Standard library functions for working with pairs

module Hydra.Lib.Pairs where

import qualified Data.Bifunctor as BF


bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimap = BF.bimap

first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, y) = y
