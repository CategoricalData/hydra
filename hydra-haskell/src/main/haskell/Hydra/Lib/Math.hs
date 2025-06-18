-- | Haskell implementations of hydra.lib.math primitives

module Hydra.Lib.Math where


neg :: Int -> Int
neg = negate

add :: Int -> Int -> Int
add x y = x + y

sub :: Int -> Int -> Int
sub x y = x - y

mul :: Int -> Int -> Int
mul x y = x * y

div :: Int -> Int -> Int
div = Prelude.div

min :: Int -> Int -> Int
min = Prelude.min

mod :: Int -> Int -> Int
mod = Prelude.mod

rangeInt32 :: Int -> Int -> [Int]
rangeInt32 start end = [start .. end]

rem :: Int -> Int -> Int
rem = Prelude.rem
