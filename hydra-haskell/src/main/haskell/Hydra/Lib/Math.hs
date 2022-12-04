-- | Haskell implementations of hydra/lib/math primitives

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

mod :: Int -> Int -> Int
mod = Prelude.mod

rem :: Int -> Int -> Int
rem = Prelude.rem
