-- | Phantom-typed term DSL for the hydra.lib.math library

module Hydra.Dsl.Meta.Lib.Math where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


abs :: Num a => TTerm a -> TTerm a
abs = primitive1 _math_abs

acos :: TTerm Double -> TTerm Double
acos = primitive1 _math_acos

acosh :: TTerm Double -> TTerm Double
acosh = primitive1 _math_acosh

add :: Num a => TTerm a -> TTerm a -> TTerm a
add = primitive2 _math_add

asin :: TTerm Double -> TTerm Double
asin = primitive1 _math_asin

asinh :: TTerm Double -> TTerm Double
asinh = primitive1 _math_asinh

atan :: TTerm Double -> TTerm Double
atan = primitive1 _math_atan

atan2 :: TTerm Double -> TTerm Double -> TTerm Double
atan2 = primitive2 _math_atan2

atanh :: TTerm Double -> TTerm Double
atanh = primitive1 _math_atanh

ceiling :: TTerm Double -> TTerm Integer
ceiling = primitive1 _math_ceiling

cos :: TTerm Double -> TTerm Double
cos = primitive1 _math_cos

cosh :: TTerm Double -> TTerm Double
cosh = primitive1 _math_cosh

div :: Integral a => TTerm a -> TTerm a -> TTerm a
div = primitive2 _math_div

e :: TTerm Double
e = primitive _math_e

even :: Integral a => TTerm a -> TTerm Bool
even = primitive1 _math_even

exp :: TTerm Double -> TTerm Double
exp = primitive1 _math_exp

floor :: TTerm Double -> TTerm Integer
floor = primitive1 _math_floor

log :: TTerm Double -> TTerm Double
log = primitive1 _math_log

logBase :: TTerm Double -> TTerm Double -> TTerm Double
logBase = primitive2 _math_logBase

max :: Ord a => TTerm a -> TTerm a -> TTerm a
max = primitive2 _math_max

min :: Ord a => TTerm a -> TTerm a -> TTerm a
min = primitive2 _math_min

mod :: Integral a => TTerm a -> TTerm a -> TTerm a
mod = primitive2 _math_mod

mul :: Num a => TTerm a -> TTerm a -> TTerm a
mul = primitive2 _math_mul

negate :: Num a => TTerm a -> TTerm a
negate = primitive1 _math_negate

odd :: Integral a => TTerm a -> TTerm Bool
odd = primitive1 _math_odd

pi :: TTerm Double
pi = primitive _math_pi

pow :: TTerm Double -> TTerm Double -> TTerm Double
pow = primitive2 _math_pow

pred :: Enum a => TTerm a -> TTerm a
pred = primitive1 _math_pred

range :: Enum a => TTerm a -> TTerm a -> TTerm [a]
range start end = primitive2 _math_range start end

rem :: Integral a => TTerm a -> TTerm a -> TTerm a
rem = primitive2 _math_rem

round :: TTerm Double -> TTerm Integer
round = primitive1 _math_round

signum :: Num a => TTerm a -> TTerm a
signum = primitive1 _math_signum

sin :: TTerm Double -> TTerm Double
sin = primitive1 _math_sin

sinh :: TTerm Double -> TTerm Double
sinh = primitive1 _math_sinh

sqrt :: TTerm Double -> TTerm Double
sqrt = primitive1 _math_sqrt

sub :: Num a => TTerm a -> TTerm a -> TTerm a
sub = primitive2 _math_sub

succ :: Enum a => TTerm a -> TTerm a
succ = primitive1 _math_succ

tan :: TTerm Double -> TTerm Double
tan = primitive1 _math_tan

tanh :: TTerm Double -> TTerm Double
tanh = primitive1 _math_tanh

truncate :: TTerm Double -> TTerm Integer
truncate = primitive1 _math_truncate
