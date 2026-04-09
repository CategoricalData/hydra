-- | Haskell implementations of hydra.lib.math primitives

module Hydra.Lib.Math where

import Prelude (Num, Ord, Integral, Enum, Bool, Double, Int, Integer, Float, Maybe(..), (.), ($), (+), (-), (*), (==), (||))
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

-- | Add two numbers.
add :: Num a => a -> a -> a
add x y = x + y

-- | Add two Float64 numbers.
addFloat64 :: Double -> Double -> Double
addFloat64 = add

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

-- | Return the ceiling of x as a float.
--
-- DIVERGENCE FROM HASKELL: Haskell's Prelude.ceiling returns an Integer, which
-- cannot represent NaN or Inf; GHC's behavior on those inputs is undefined and
-- produces nonsensical gigantic integers. Hydra returns a Float64 instead so
-- that NaN and ±Inf propagate naturally per IEEE 754, matching the conventions
-- of C (ceil), Java (Math.ceil), Go (math.Ceil), Rust (f64::ceil), and JavaScript
-- (Math.ceil). Users who need an Integer value must convert explicitly.
ceiling :: Double -> Double
ceiling x
  | Prelude.isNaN x Prelude.|| Prelude.isInfinite x = x
  | Prelude.otherwise = Prelude.fromIntegral (Prelude.ceiling x :: Integer)

-- | Return the cosine of x radians.
cos :: Double -> Double
cos = Prelude.cos

-- | Return the hyperbolic cosine of x.
cosh :: Double -> Double
cosh = Prelude.cosh

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

-- | Return the floor of x as a float.
--
-- DIVERGENCE FROM HASKELL: see the note on ceiling. Returns Float64 rather than
-- Integer so that NaN and ±Inf propagate naturally per IEEE 754.
floor :: Double -> Double
floor x
  | Prelude.isNaN x Prelude.|| Prelude.isInfinite x = x
  | Prelude.otherwise = Prelude.fromIntegral (Prelude.floor x :: Integer)

-- | Return the natural logarithm of x.
log :: Double -> Double
log = Prelude.log

-- | Return the logarithm of x to the given base.
logBase :: Double -> Double -> Double
logBase = Prelude.logBase

-- | Return the maximum of two values.
max :: Ord a => a -> a -> a
max = Prelude.max

-- | Divide two integers using integer division, returning Nothing on division by zero.
maybeDiv :: Int -> Int -> Maybe Int
maybeDiv _ 0 = Nothing
maybeDiv x y = Just (Prelude.div x y)

-- | Return the minimum of two values.
min :: Ord a => a -> a -> a
min = Prelude.min

-- | Mathematical modulo, returning Nothing on division by zero.
maybeMod :: Int -> Int -> Maybe Int
maybeMod _ 0 = Nothing
maybeMod x y = Just (Prelude.mod x y)

-- | Mathematical modulo.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
mod :: Integral a => a -> a -> a
mod = Prelude.mod

-- | Multiply two numbers.
mul :: Num a => a -> a -> a
mul x y = x * y

-- | Multiply two Float64 numbers.
mulFloat64 :: Double -> Double -> Double
mulFloat64 = mul

-- | Negate a number.
negate :: Num a => a -> a
negate = Prelude.negate

-- | Negate a Float64 number.
negateFloat64 :: Double -> Double
negateFloat64 = Prelude.negate

-- | Check if an integer is odd.
odd :: Integral a => a -> Bool
odd = Prelude.odd

-- | Pi (π ≈ 3.14159).
pi :: Double
pi = Prelude.pi

-- | Return x raised to the power y.
pow :: Double -> Double -> Double
pow = (Prelude.**)

-- | Return the predecessor (x - 1), returning Nothing on int32 minBound.
maybePred :: Int -> Maybe Int
maybePred x
  | x == (-2147483648) = Nothing
  | Prelude.otherwise = Just (x - 1)

-- | Return the predecessor (x - 1).
pred :: Enum a => a -> a
pred = Prelude.pred

-- | Generate a range of values from start to end (inclusive).
range :: Enum a => a -> a -> [a]
range start end = [start .. end]

-- | Integer remainder, returning Nothing on division by zero.
maybeRem :: Int -> Int -> Maybe Int
maybeRem _ 0 = Nothing
maybeRem x y = Just (Prelude.rem x y)

-- | Integer remainder.
-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
rem :: Integral a => a -> a -> a
rem = Prelude.rem

-- | Return x rounded to the nearest integer, as a float.
--
-- DIVERGENCE FROM HASKELL: see the note on ceiling. Returns Float64 rather than
-- Integer so that NaN and ±Inf propagate naturally per IEEE 754.
round :: Double -> Double
round x
  | Prelude.isNaN x Prelude.|| Prelude.isInfinite x = x
  | Prelude.otherwise = Prelude.fromIntegral (Prelude.round x :: Integer)

-- | Round a bigfloat to n significant digits.
roundBigfloat :: Int -> Double -> Double
roundBigfloat = roundFloat64

-- | Round a float32 to n significant digits.
-- Returns NaN/Inf inputs unchanged (no rounding is possible).
roundFloat32 :: Int -> Float -> Float
roundFloat32 n x
  | Prelude.isNaN x Prelude.|| Prelude.isInfinite x = x
  | x Prelude.== 0 = 0
  | Prelude.otherwise =
      let factor = 10 Prelude.^^ (n - 1 - Prelude.floor (Prelude.logBase 10 (Prelude.abs x)))
      in Prelude.fromIntegral (Prelude.round (x * factor) :: Integer) Prelude./ factor

-- | Round a float64 to n significant digits.
-- Returns NaN/Inf inputs unchanged (no rounding is possible).
roundFloat64 :: Int -> Double -> Double
roundFloat64 n x
  | Prelude.isNaN x Prelude.|| Prelude.isInfinite x = x
  | x Prelude.== 0 = 0
  | Prelude.otherwise =
      let factor = 10 Prelude.^^ (n - 1 - Prelude.floor (Prelude.logBase 10 (Prelude.abs x)))
      in Prelude.fromIntegral (Prelude.round (x * factor) :: Integer) Prelude./ factor

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

-- | Subtract two Float64 numbers.
subFloat64 :: Double -> Double -> Double
subFloat64 = sub

-- | Return the successor (x + 1), returning Nothing on int32 maxBound.
maybeSucc :: Int -> Maybe Int
maybeSucc x
  | x == 2147483647 = Nothing
  | Prelude.otherwise = Just (x + 1)

-- | Return the successor (x + 1).
succ :: Enum a => a -> a
succ = Prelude.succ

-- | Return the tangent of x radians.
tan :: Double -> Double
tan = Prelude.tan

-- | Return the hyperbolic tangent of x.
tanh :: Double -> Double
tanh = Prelude.tanh

-- | Return x truncated (towards zero), as a float.
--
-- DIVERGENCE FROM HASKELL: see the note on ceiling. Returns Float64 rather than
-- Integer so that NaN and ±Inf propagate naturally per IEEE 754.
truncate :: Double -> Double
truncate x
  | Prelude.isNaN x Prelude.|| Prelude.isInfinite x = x
  | Prelude.otherwise = Prelude.fromIntegral (Prelude.truncate x :: Integer)
