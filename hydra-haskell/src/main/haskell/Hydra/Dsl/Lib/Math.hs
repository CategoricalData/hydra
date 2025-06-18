module Hydra.Dsl.Lib.Math where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms


add :: TTerm Int -> TTerm Int -> TTerm Int
add = primitive2 _math_add

div :: TTerm Int -> TTerm Int -> TTerm Int
div = primitive2 _math_div

min :: TTerm Int -> TTerm Int -> TTerm Int
min = primitive2 _math_min

mod :: TTerm Int -> TTerm Int -> TTerm Int
mod = primitive2 _math_mod

mul :: TTerm Int -> TTerm Int -> TTerm Int
mul = primitive2 _math_mul

neg :: TTerm Int -> TTerm Int
neg = primitive1 _math_neg

rangeInt32 :: TTerm Int -> TTerm Int -> TTerm [Int]
rangeInt32 start end = primitive2 _math_rangeInt32 start end

rem :: TTerm Int -> TTerm Int -> TTerm Int
rem = primitive2 _math_rem

sub :: TTerm Int -> TTerm Int -> TTerm Int
sub = primitive2 _math_sub

-- Some forward-looking aliases
addInt32 = Hydra.Dsl.Lib.Math.add
divInt32 = Hydra.Dsl.Lib.Math.div
modInt32 = Hydra.Dsl.Lib.Math.mod
mulInt32 = Hydra.Dsl.Lib.Math.mul
negInt32 = Hydra.Dsl.Lib.Math.neg
remInt32 = Hydra.Dsl.Lib.Math.rem
subInt32 = Hydra.Dsl.Lib.Math.sub
