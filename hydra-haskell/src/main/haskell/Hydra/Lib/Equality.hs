-- | Haskell implementations of hydra/lib/equality primitives. These simply make use of derived Eq.

module Hydra.Lib.Equality where

import Hydra.Core


equalTerm :: Eq a => Term a -> Term a -> Bool
equalTerm = (==)

equalType :: Eq a => Type a -> Type a -> Bool
equalType = (==)
