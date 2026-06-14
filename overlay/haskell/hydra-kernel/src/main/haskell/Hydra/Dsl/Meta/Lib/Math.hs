-- | Phantom-typed term DSL for the hydra.lib.math library

module Hydra.Dsl.Meta.Lib.Math where

import Hydra.Typed
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Math as DefMath


-- | Return the absolute value.
abs :: Num a => TypedTerm a -> TypedTerm a
abs = primitive1 (Prims.primName DefMath.abs)

-- | Return the arc cosine of x in radians.
acos :: TypedTerm Double -> TypedTerm Double
acos = primitive1 (Prims.primName DefMath.acos)

-- | Return the inverse hyperbolic cosine of x.
acosh :: TypedTerm Double -> TypedTerm Double
acosh = primitive1 (Prims.primName DefMath.acosh)

-- | Add two numbers.
add :: Num a => TypedTerm a -> TypedTerm a -> TypedTerm a
add = primitive2 (Prims.primName DefMath.add)

-- | Add two Float64 numbers.
addFloat64 :: TypedTerm Double -> TypedTerm Double -> TypedTerm Double
addFloat64 = primitive2 (Prims.primName DefMath.addFloat64)

-- | Return the arc sine of x in radians.
asin :: TypedTerm Double -> TypedTerm Double
asin = primitive1 (Prims.primName DefMath.asin)

-- | Return the inverse hyperbolic sine of x.
asinh :: TypedTerm Double -> TypedTerm Double
asinh = primitive1 (Prims.primName DefMath.asinh)

-- | Return the arc tangent of x in radians.
atan :: TypedTerm Double -> TypedTerm Double
atan = primitive1 (Prims.primName DefMath.atan)

-- | Return the arc tangent of y/x in radians, using signs to determine quadrant.
atan2 :: TypedTerm Double -> TypedTerm Double -> TypedTerm Double
atan2 = primitive2 (Prims.primName DefMath.atan2)

-- | Return the inverse hyperbolic tangent of x.
atanh :: TypedTerm Double -> TypedTerm Double
atanh = primitive1 (Prims.primName DefMath.atanh)

-- | Return the ceiling of x as a float64.
-- DIVERGENCE FROM HASKELL: returns Double rather than Integer so that NaN and
-- ±Inf inputs propagate per IEEE 754.
ceiling :: TypedTerm Double -> TypedTerm Double
ceiling = primitive1 (Prims.primName DefMath.ceiling)

-- | Return the cosine of x radians.
cos :: TypedTerm Double -> TypedTerm Double
cos = primitive1 (Prims.primName DefMath.cos)

-- | Return the hyperbolic cosine of x.
cosh :: TypedTerm Double -> TypedTerm Double
cosh = primitive1 (Prims.primName DefMath.cosh)

-- | Euler's number (e = 2.71828).
e :: TypedTerm Double
e = primitive (Prims.primName DefMath.e)

-- | Check if an integer is even.
even :: Integral a => TypedTerm a -> TypedTerm Bool
even = primitive1 (Prims.primName DefMath.even)

-- | Return e raised to the power x.
exp :: TypedTerm Double -> TypedTerm Double
exp = primitive1 (Prims.primName DefMath.exp)

-- | Return the floor of x as a float64.
-- DIVERGENCE FROM HASKELL: returns Double rather than Integer so that NaN and
-- ±Inf inputs propagate per IEEE 754.
floor :: TypedTerm Double -> TypedTerm Double
floor = primitive1 (Prims.primName DefMath.floor)

-- | Return the natural logarithm of x.
log :: TypedTerm Double -> TypedTerm Double
log = primitive1 (Prims.primName DefMath.log)

-- | Return the logarithm of x to the given base.
logBase :: TypedTerm Double -> TypedTerm Double -> TypedTerm Double
logBase = primitive2 (Prims.primName DefMath.logBase)

-- | Return the maximum of two values.
max :: Ord a => TypedTerm a -> TypedTerm a -> TypedTerm a
max = primitive2 (Prims.primName DefMath.max)

-- | Divide two integers using integer division, returning Nothing on division by zero.
maybeDiv :: TypedTerm Int -> TypedTerm Int -> TypedTerm (Maybe Int)
maybeDiv = primitive2 (Prims.primName DefMath.maybeDiv)

-- | Mathematical modulo, returning Nothing on division by zero.
maybeMod :: TypedTerm Int -> TypedTerm Int -> TypedTerm (Maybe Int)
maybeMod = primitive2 (Prims.primName DefMath.maybeMod)

-- | Return the predecessor (x - 1), returning Nothing on minBound.
maybePred :: TypedTerm Int -> TypedTerm (Maybe Int)
maybePred = primitive1 (Prims.primName DefMath.maybePred)

-- | Integer remainder, returning Nothing on division by zero.
maybeRem :: TypedTerm Int -> TypedTerm Int -> TypedTerm (Maybe Int)
maybeRem = primitive2 (Prims.primName DefMath.maybeRem)

-- | Return the successor (x + 1), returning Nothing on maxBound.
maybeSucc :: TypedTerm Int -> TypedTerm (Maybe Int)
maybeSucc = primitive1 (Prims.primName DefMath.maybeSucc)

-- | Return the minimum of two values.
min :: Ord a => TypedTerm a -> TypedTerm a -> TypedTerm a
min = primitive2 (Prims.primName DefMath.min)

-- | Multiply two numbers.
mul :: Num a => TypedTerm a -> TypedTerm a -> TypedTerm a
mul = primitive2 (Prims.primName DefMath.mul)

-- | Multiply two Float64 numbers.
mulFloat64 :: TypedTerm Double -> TypedTerm Double -> TypedTerm Double
mulFloat64 = primitive2 (Prims.primName DefMath.mulFloat64)

-- | Negate a number.
negate :: Num a => TypedTerm a -> TypedTerm a
negate = primitive1 (Prims.primName DefMath.negate)

-- | Negate a Float64 number.
negateFloat64 :: TypedTerm Double -> TypedTerm Double
negateFloat64 = primitive1 (Prims.primName DefMath.negateFloat64)

-- | Check if an integer is odd.
odd :: Integral a => TypedTerm a -> TypedTerm Bool
odd = primitive1 (Prims.primName DefMath.odd)

-- | Pi (pi = 3.14159).
pi :: TypedTerm Double
pi = primitive (Prims.primName DefMath.pi)

-- | Return x raised to the power y.
pow :: TypedTerm Double -> TypedTerm Double -> TypedTerm Double
pow = primitive2 (Prims.primName DefMath.pow)

-- | Generate a range of values from start to end (inclusive).
range :: Enum a => TypedTerm a -> TypedTerm a -> TypedTerm [a]
range start end = primitive2 (Prims.primName DefMath.range) start end

-- | Return x rounded to the nearest integer, as a float64.
-- DIVERGENCE FROM HASKELL: returns Double rather than Integer so that NaN and
-- ±Inf inputs propagate per IEEE 754.
round :: TypedTerm Double -> TypedTerm Double
round = primitive1 (Prims.primName DefMath.round)

-- | Round a float32 to n significant digits.
roundFloat32 :: TypedTerm Int -> TypedTerm Float -> TypedTerm Float
roundFloat32 = primitive2 (Prims.primName DefMath.roundFloat32)

-- | Round a float64 to n significant digits.
roundFloat64 :: TypedTerm Int -> TypedTerm Double -> TypedTerm Double
roundFloat64 = primitive2 (Prims.primName DefMath.roundFloat64)

-- | Return the sign of a number (-1, 0, or 1).
signum :: Num a => TypedTerm a -> TypedTerm a
signum = primitive1 (Prims.primName DefMath.signum)

-- | Return the sine of x radians.
sin :: TypedTerm Double -> TypedTerm Double
sin = primitive1 (Prims.primName DefMath.sin)

-- | Return the hyperbolic sine of x.
sinh :: TypedTerm Double -> TypedTerm Double
sinh = primitive1 (Prims.primName DefMath.sinh)

-- | Return the square root of x.
sqrt :: TypedTerm Double -> TypedTerm Double
sqrt = primitive1 (Prims.primName DefMath.sqrt)

-- | Subtract two numbers.
sub :: Num a => TypedTerm a -> TypedTerm a -> TypedTerm a
sub = primitive2 (Prims.primName DefMath.sub)

-- | Subtract two Float64 numbers.
subFloat64 :: TypedTerm Double -> TypedTerm Double -> TypedTerm Double
subFloat64 = primitive2 (Prims.primName DefMath.subFloat64)

-- | Return the tangent of x radians.
tan :: TypedTerm Double -> TypedTerm Double
tan = primitive1 (Prims.primName DefMath.tan)

-- | Return the hyperbolic tangent of x.
tanh :: TypedTerm Double -> TypedTerm Double
tanh = primitive1 (Prims.primName DefMath.tanh)

-- | Return x truncated (towards zero), as a float64.
-- DIVERGENCE FROM HASKELL: returns Double rather than Integer so that NaN and
-- ±Inf inputs propagate per IEEE 754.
truncate :: TypedTerm Double -> TypedTerm Double
truncate = primitive1 (Prims.primName DefMath.truncate)
