-- | Phantom-typed term DSL for the hydra.lib.math library

module Hydra.Dsl.Lib.Math where

import Hydra.Phantoms
import Hydra.Dsl.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


abs :: TTerm a -> TTerm a
abs = primitive1 _math_abs

add :: TTerm a -> TTerm a -> TTerm a
add = primitive2 _math_add

div :: TTerm a -> TTerm a -> TTerm a
div = primitive2 _math_div

mod :: TTerm a -> TTerm a -> TTerm a
mod = primitive2 _math_mod

mul :: TTerm a -> TTerm a -> TTerm a
mul = primitive2 _math_mul

negate :: TTerm a -> TTerm a
negate = primitive1 _math_negate

pred :: TTerm a -> TTerm a
pred = primitive1 _math_pred

range :: TTerm a -> TTerm a -> TTerm [a]
range start end = primitive2 _math_range start end

rem :: TTerm a -> TTerm a -> TTerm a
rem = primitive2 _math_rem

signum :: TTerm a -> TTerm a
signum = primitive1 _math_signum

sub :: TTerm a -> TTerm a -> TTerm a
sub = primitive2 _math_sub

succ :: TTerm a -> TTerm a
succ = primitive1 _math_succ