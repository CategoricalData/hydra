-- | Haskell implementations of hydra.lib.math primitives

module Hydra.Lib.Math where


neg :: Num a => a -> a
neg = negate

add :: Num a => a -> a -> a
add x y = x + y

div :: Integral a => a -> a -> a
div = Prelude.div

mod :: Integral a => a -> a -> a
mod = Prelude.mod

mul :: Num a => a -> a -> a
mul x y = x * y

range :: Enum a => a -> a -> [a]
range start end = [start .. end]

rem :: Integral a => a -> a -> a
rem = Prelude.rem

sub :: Num a => a -> a -> a
sub x y = x - y
