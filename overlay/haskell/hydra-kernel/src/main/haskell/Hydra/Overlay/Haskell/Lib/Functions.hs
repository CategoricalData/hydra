-- | Haskell implementations of hydra.lib.functions primitives.

module Hydra.Overlay.Haskell.Lib.Functions where

import qualified Prelude as P


-- | Compose two functions: compose g f x = g (f x).
compose :: (b -> c) -> (a -> b) -> a -> c
compose g f = \x -> g (f x)

-- | Return the first argument, ignoring the second.
const :: a -> b -> a
const x _ = x

-- | Swap the argument order of a binary function.
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- | Return the argument unchanged.
identity :: a -> a
identity = P.id
