-- | Haskell implementations of hydra/lib/logic (logic and control flow) primitives

module Hydra.Lib.Logic where

import Hydra.Core

import Data.Int


and :: Bool -> Bool -> Bool
and x y = x && y

ifElse :: a -> a -> Bool -> a
ifElse x y b = if b then x else y

not :: Bool -> Bool
not = Prelude.not

or :: Bool -> Bool -> Bool
or x y = x || y
