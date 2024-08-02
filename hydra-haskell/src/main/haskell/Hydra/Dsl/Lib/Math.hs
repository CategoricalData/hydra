module Hydra.Dsl.Lib.Math where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


add :: TTerm (Int -> Int -> Int)
add = TTerm $ Terms.primitive _math_add

div :: TTerm (Int -> Int -> Int)
div = TTerm $ Terms.primitive _math_div

mod :: TTerm (Int -> Int -> Int)
mod = TTerm $ Terms.primitive _math_mod

mul :: TTerm (Int -> Int -> Int)
mul = TTerm $ Terms.primitive _math_mul

neg :: TTerm (Int -> Int)
neg = TTerm $ Terms.primitive _math_neg

rem :: TTerm (Int -> Int -> Int)
rem = TTerm $ Terms.primitive _math_rem

sub :: TTerm (Int -> Int -> Int)
sub = TTerm $ Terms.primitive _math_sub
