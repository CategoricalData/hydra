-- | Haskell implementations of hydra.lib.math primitives

module Hydra.Lib.Math where

import Prelude (Num, Ord, Integral, Enum, Bool, Double, Int, Integer, Float, (.), ($), (+), (-), (*))
import qualified Prelude


-- | Return the absolute value.
abs :: Num a => a -> a
abs = Prelude.abs

-- | Return the arc cosine of x in radians.
acos :: Double -> Double
acos = Prelude.acos

-- | Return the inverse hyperbolic cosine of x.
acosh :: Double -> Double
acosh = Prelude.acosh

-- | Return the arc sine of x in radians.
asin :: Double -> Double
asin = Prelude.asin

-- | Return the inverse hyperbolic sine of x.
asinh :: Double -> Double
asinh = Prelude.asinh

-- | Return the arc tangent of x in radians.
atan :: Double -> Double
atan = Prelude.atan

-- | Return the arc tangent of y/x in radians, using signs to determine quadrant.
atan2 :: Double -> Double -> Double
atan2 = Prelude.atan2

-- | Return the inverse hyperbolic tangent of x.
atanh :: Double -> Double
atanh = Prelude.atanh

-- | Return the ceiling of x as an integer.
ceiling :: Double -> Integer
ceiling = Prelude.ceiling

-- | Return the cosine of x radians.
cos :: Double -> Double
cos = Prelude.cos

-- | Return the hyperbolic cosine of x.
cosh :: Double -> Double
cosh = Prelude.cosh

-- | Add two numbers.
add :: Num a => a -> a -> a
add x y = x + y

-- | Divide two integers using integer division.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
div :: Integral a => a -> a -> a
div = Prelude.div

-- | Euler's number (e ≈ 2.71828).
e :: Double
e = Prelude.exp 1.0

-- | Check if an integer is even.
even :: Integral a => a -> Bool
even = Prelude.even

-- | Return e raised to the power x.
exp :: Double -> Double
exp = Prelude.exp

-- | Return the floor of x as an integer.
floor :: Double -> Integer
floor = Prelude.floor

-- | Return the natural logarithm of x.
log :: Double -> Double
log = Prelude.log

-- | Return the logarithm of x to the given base.
logBase :: Double -> Double -> Double
logBase = Prelude.logBase

-- | Return the maximum of two values.
max :: Ord a => a -> a -> a
max = Prelude.max

-- | Return the minimum of two values.
min :: Ord a => a -> a -> a
min = Prelude.min

-- | Mathematical modulo.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
mod :: Integral a => a -> a -> a
mod = Prelude.mod

-- | Multiply two numbers.
mul :: Num a => a -> a -> a
mul x y = x * y

-- | Negate a number.
negate :: Num a => a -> a
negate = Prelude.negate

-- | Check if an integer is odd.
odd :: Integral a => a -> Bool
odd = Prelude.odd

-- | Pi (π ≈ 3.14159).
pi :: Double
pi = Prelude.pi

-- | Return x raised to the power y.
pow :: Double -> Double -> Double
pow = (Prelude.**)

-- | Return the predecessor (x - 1).
pred :: Enum a => a -> a
pred = Prelude.pred

-- | Generate a range of values from start to end (inclusive).
range :: Enum a => a -> a -> [a]
range start end = [start .. end]

-- | Integer remainder.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
rem :: Integral a => a -> a -> a
rem = Prelude.rem

-- | Return x rounded to the nearest integer.
round :: Double -> Integer
round = Prelude.round

-- | Return the sign of a number (-1, 0, or 1).
signum :: Num a => a -> a
signum = Prelude.signum

-- | Return the sine of x radians.
sin :: Double -> Double
sin = Prelude.sin

-- | Return the hyperbolic sine of x.
sinh :: Double -> Double
sinh = Prelude.sinh

-- | Return the square root of x.
sqrt :: Double -> Double
sqrt = Prelude.sqrt

-- | Subtract two numbers.
sub :: Num a => a -> a -> a
sub x y = x - y

-- | Return the successor (x + 1).
succ :: Enum a => a -> a
succ = Prelude.succ

-- | Return the tangent of x radians.
tan :: Double -> Double
tan = Prelude.tan

-- | Return the hyperbolic tangent of x.
tanh :: Double -> Double
tanh = Prelude.tanh

-- | Return x truncated to an integer (towards zero).
truncate :: Double -> Integer
truncate = Prelude.truncate