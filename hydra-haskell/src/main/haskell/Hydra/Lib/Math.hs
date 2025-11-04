-- | Haskell implementations of hydra.lib.math primitives

module Hydra.Lib.Math where


abs :: Num a => a -> a
abs = Prelude.abs

add :: Num a => a -> a -> a
add x y = x + y

div :: Integral a => a -> a -> a
div = Prelude.div

mod :: Integral a => a -> a -> a
mod = Prelude.mod

mul :: Num a => a -> a -> a
mul x y = x * y

negate :: Num a => a -> a
negate = Prelude.negate

pred :: Enum a => a -> a
pred = Prelude.pred

range :: Enum a => a -> a -> [a]
range start end = [start .. end]

rem :: Integral a => a -> a -> a
rem = Prelude.rem

signum :: Num a => a -> a
signum = Prelude.signum

sub :: Num a => a -> a -> a
sub x y = x - y

succ :: Enum a => a -> a
succ = Prelude.succ