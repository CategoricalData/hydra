-- | Haskell implementations of hydra.lib.tuples primitives

module Hydra.Lib.Tuples where


curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

fst :: (a, b) -> a
fst = Prelude.fst

snd :: (a, b) -> b
snd = Prelude.snd

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y
