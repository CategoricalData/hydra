-- | Phantom-typed term DSL for the hydra.lib.math library

module Hydra.Dsl.Lib.Math where

import Hydra.Phantoms
import Hydra.Dsl.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


abs :: Num a => TTerm a -> TTerm a
abs = primitive1 _math_abs

add :: Num a => TTerm a -> TTerm a -> TTerm a
add = primitive2 _math_add

div :: Integral a => TTerm a -> TTerm a -> TTerm a
div = primitive2 _math_div

even :: Integral a => TTerm a -> TTerm Bool
even = primitive1 _math_even

mod :: Integral a => TTerm a -> TTerm a -> TTerm a
mod = primitive2 _math_mod

mul :: Num a => TTerm a -> TTerm a -> TTerm a
mul = primitive2 _math_mul

negate :: Num a => TTerm a -> TTerm a
negate = primitive1 _math_negate

odd :: Integral a => TTerm a -> TTerm Bool
odd = primitive1 _math_odd

pred :: Enum a => TTerm a -> TTerm a
pred = primitive1 _math_pred

range :: Enum a => TTerm a -> TTerm a -> TTerm [a]
range start end = primitive2 _math_range start end

rem :: Integral a => TTerm a -> TTerm a -> TTerm a
rem = primitive2 _math_rem

signum :: Num a => TTerm a -> TTerm a
signum = primitive1 _math_signum

sub :: Num a => TTerm a -> TTerm a -> TTerm a
sub = primitive2 _math_sub

succ :: Enum a => TTerm a -> TTerm a
succ = primitive1 _math_succ
