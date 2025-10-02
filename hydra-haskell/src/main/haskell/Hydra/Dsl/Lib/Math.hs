-- | Phantom-typed term DSL for the hydra.lib.math library

module Hydra.Dsl.Lib.Math where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms


add :: TTerm a -> TTerm a -> TTerm a
add = primitive2 _math_add

div :: TTerm a -> TTerm a -> TTerm a
div = primitive2 _math_div

mod :: TTerm a -> TTerm a -> TTerm a
mod = primitive2 _math_mod

mul :: TTerm a -> TTerm a -> TTerm a
mul = primitive2 _math_mul

neg :: TTerm a -> TTerm a
neg = primitive1 _math_neg

range :: TTerm a -> TTerm a -> TTerm [a]
range start end = primitive2 _math_range start end

rem :: TTerm a -> TTerm a -> TTerm a
rem = primitive2 _math_rem

sub :: TTerm a -> TTerm a -> TTerm a
sub = primitive2 _math_sub
