-- | Haskell implementations of hydra.lib.math primitives

-- TODO: real-valued primitives such as pi, exp, log, etc. for symmetry with Prelude

module Hydra.Lib.Math where


abs :: Num a => a -> a
abs = Prelude.abs

add :: Num a => a -> a -> a
add x y = x + y

-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
div :: Integral a => a -> a -> a
div = Prelude.div

even :: Integral a => a -> Bool
even = Prelude.even

-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
mod :: Integral a => a -> a -> a
mod = Prelude.mod

mul :: Num a => a -> a -> a
mul x y = x * y

negate :: Num a => a -> a
negate = Prelude.negate

odd :: Integral a => a -> Bool
odd = Prelude.odd

pred :: Enum a => a -> a
pred = Prelude.pred

range :: Enum a => a -> a -> [a]
range start end = [start .. end]

-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
rem :: Integral a => a -> a -> a
rem = Prelude.rem

signum :: Num a => a -> a
signum = Prelude.signum

sub :: Num a => a -> a -> a
sub x y = x - y

succ :: Enum a => a -> a
succ = Prelude.succ