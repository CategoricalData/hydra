-- | Standard library functions for working with pairs

module Hydra.Lib.Pairs where


first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, y) = y
