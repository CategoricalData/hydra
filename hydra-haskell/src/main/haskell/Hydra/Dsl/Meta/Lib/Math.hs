-- | Phantom-typed term DSL for the hydra.lib.math library

module Hydra.Dsl.Meta.Lib.Math where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


-- | Return the absolute value.
abs :: Num a => TTerm a -> TTerm a
abs = primitive1 _math_abs

-- | Return the arc cosine of x in radians.
acos :: TTerm Double -> TTerm Double
acos = primitive1 _math_acos

-- | Return the inverse hyperbolic cosine of x.
acosh :: TTerm Double -> TTerm Double
acosh = primitive1 _math_acosh

-- | Add two numbers.
add :: Num a => TTerm a -> TTerm a -> TTerm a
add = primitive2 _math_add

-- | Return the arc sine of x in radians.
asin :: TTerm Double -> TTerm Double
asin = primitive1 _math_asin

-- | Return the inverse hyperbolic sine of x.
asinh :: TTerm Double -> TTerm Double
asinh = primitive1 _math_asinh

-- | Return the arc tangent of x in radians.
atan :: TTerm Double -> TTerm Double
atan = primitive1 _math_atan

-- | Return the arc tangent of y/x in radians, using signs to determine quadrant.
atan2 :: TTerm Double -> TTerm Double -> TTerm Double
atan2 = primitive2 _math_atan2

-- | Return the inverse hyperbolic tangent of x.
atanh :: TTerm Double -> TTerm Double
atanh = primitive1 _math_atanh

-- | Return the ceiling of x as an integer.
ceiling :: TTerm Double -> TTerm Integer
ceiling = primitive1 _math_ceiling

-- | Return the cosine of x radians.
cos :: TTerm Double -> TTerm Double
cos = primitive1 _math_cos

-- | Return the hyperbolic cosine of x.
cosh :: TTerm Double -> TTerm Double
cosh = primitive1 _math_cosh

-- | Divide two integers using integer division.
div :: Integral a => TTerm a -> TTerm a -> TTerm a
div = primitive2 _math_div

-- | Euler's number (e = 2.71828).
e :: TTerm Double
e = primitive _math_e

-- | Check if an integer is even.
even :: Integral a => TTerm a -> TTerm Bool
even = primitive1 _math_even

-- | Return e raised to the power x.
exp :: TTerm Double -> TTerm Double
exp = primitive1 _math_exp

-- | Return the floor of x as an integer.
floor :: TTerm Double -> TTerm Integer
floor = primitive1 _math_floor

-- | Return the natural logarithm of x.
log :: TTerm Double -> TTerm Double
log = primitive1 _math_log

-- | Return the logarithm of x to the given base.
logBase :: TTerm Double -> TTerm Double -> TTerm Double
logBase = primitive2 _math_logBase

-- | Return the maximum of two values.
max :: Ord a => TTerm a -> TTerm a -> TTerm a
max = primitive2 _math_max

-- | Return the minimum of two values.
min :: Ord a => TTerm a -> TTerm a -> TTerm a
min = primitive2 _math_min

-- | Mathematical modulo.
mod :: Integral a => TTerm a -> TTerm a -> TTerm a
mod = primitive2 _math_mod

-- | Multiply two numbers.
mul :: Num a => TTerm a -> TTerm a -> TTerm a
mul = primitive2 _math_mul

-- | Negate a number.
negate :: Num a => TTerm a -> TTerm a
negate = primitive1 _math_negate

-- | Check if an integer is odd.
odd :: Integral a => TTerm a -> TTerm Bool
odd = primitive1 _math_odd

-- | Pi (pi = 3.14159).
pi :: TTerm Double
pi = primitive _math_pi

-- | Return x raised to the power y.
pow :: TTerm Double -> TTerm Double -> TTerm Double
pow = primitive2 _math_pow

-- | Return the predecessor (x - 1).
pred :: Enum a => TTerm a -> TTerm a
pred = primitive1 _math_pred

-- | Generate a range of values from start to end (inclusive).
range :: Enum a => TTerm a -> TTerm a -> TTerm [a]
range start end = primitive2 _math_range start end

-- | Integer remainder.
rem :: Integral a => TTerm a -> TTerm a -> TTerm a
rem = primitive2 _math_rem

-- | Return x rounded to the nearest integer.
round :: TTerm Double -> TTerm Integer
round = primitive1 _math_round

-- | Round a bigfloat to n significant digits.
roundBigfloat :: TTerm Int -> TTerm Double -> TTerm Double
roundBigfloat = primitive2 _math_roundBigfloat

-- | Round a float32 to n significant digits.
roundFloat32 :: TTerm Int -> TTerm Float -> TTerm Float
roundFloat32 = primitive2 _math_roundFloat32

-- | Round a float64 to n significant digits.
roundFloat64 :: TTerm Int -> TTerm Double -> TTerm Double
roundFloat64 = primitive2 _math_roundFloat64

-- | Return the sign of a number (-1, 0, or 1).
signum :: Num a => TTerm a -> TTerm a
signum = primitive1 _math_signum

-- | Return the sine of x radians.
sin :: TTerm Double -> TTerm Double
sin = primitive1 _math_sin

-- | Return the hyperbolic sine of x.
sinh :: TTerm Double -> TTerm Double
sinh = primitive1 _math_sinh

-- | Return the square root of x.
sqrt :: TTerm Double -> TTerm Double
sqrt = primitive1 _math_sqrt

-- | Subtract two numbers.
sub :: Num a => TTerm a -> TTerm a -> TTerm a
sub = primitive2 _math_sub

-- | Return the successor (x + 1).
succ :: Enum a => TTerm a -> TTerm a
succ = primitive1 _math_succ

-- | Return the tangent of x radians.
tan :: TTerm Double -> TTerm Double
tan = primitive1 _math_tan

-- | Return the hyperbolic tangent of x.
tanh :: TTerm Double -> TTerm Double
tanh = primitive1 _math_tanh

-- | Return x truncated to an integer (towards zero).
truncate :: TTerm Double -> TTerm Integer
truncate = primitive1 _math_truncate
