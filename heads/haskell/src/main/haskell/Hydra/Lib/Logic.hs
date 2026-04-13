-- | Haskell implementations of hydra.lib.logic (logic and control flow) primitives

module Hydra.Lib.Logic where

import Hydra.Core

import Data.Int


-- | Compute the logical AND of two boolean values.
and :: Bool -> Bool -> Bool
and x y = x && y

-- | Compute a conditional expression.
ifElse :: Bool -> a -> a -> a
ifElse b x y = if b then x else y

-- | Compute the logical NOT of a boolean value.
not :: Bool -> Bool
not = Prelude.not

-- | Compute the logical OR of two boolean values.
or :: Bool -> Bool -> Bool
or x y = x || y
