module Hydra.Dsl.Lib.Math where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms


add :: TTerm Int -> TTerm Int -> TTerm Int
add = primitive2 _math_add

div :: TTerm Int -> TTerm Int -> TTerm Int
div = primitive2 _math_div

mod :: TTerm Int -> TTerm Int -> TTerm Int
mod = primitive2 _math_mod

mul :: TTerm Int -> TTerm Int -> TTerm Int
mul = primitive2 _math_mul

neg :: TTerm Int -> TTerm Int
neg = primitive1 _math_neg

rem :: TTerm Int -> TTerm Int -> TTerm Int
rem = primitive2 _math_rem

sub :: TTerm Int -> TTerm Int -> TTerm Int
sub = primitive2 _math_sub
