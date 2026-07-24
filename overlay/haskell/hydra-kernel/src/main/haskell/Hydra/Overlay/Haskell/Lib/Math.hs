-- | Haskell implementations of hydra.lib.math primitives
{-# LANGUAGE RankNTypes #-}

module Hydra.Overlay.Haskell.Lib.Math where

import Prelude (Num, Ord, Integral, Enum, Bool, Double, Int, Integer, Float, Maybe(..), String, (.), ($), (+), (-), (*), (==), (||))
import qualified Prelude
import qualified Hydra.Core as Core


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

-- | Divide two integers using integer division, returning Nothing on division by zero.
div :: Int -> Int -> Maybe Int
div _ 0 = Nothing
div x y = Just (Prelude.div x y)

-- | Mathematical modulo, returning Nothing on division by zero.
mod :: Int -> Int -> Maybe Int
mod _ 0 = Nothing
mod x y = Just (Prelude.mod x y)

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

-- | Generate a range of values from start to end (inclusive).
range :: Enum a => a -> a -> [a]
range start end = [start .. end]

-- | Integer remainder, returning Nothing on division by zero.
rem :: Int -> Int -> Maybe Int
rem _ 0 = Nothing
rem x y = Just (Prelude.rem x y)

-- | Return x rounded to the nearest integer, as a float.
--
-- DIVERGENCE FROM HASKELL: see the note on ceiling. Returns Float64 rather than
-- Integer so that NaN and ±Inf propagate naturally per IEEE 754.
round :: Double -> Double
round x
  | Prelude.isNaN x Prelude.|| Prelude.isInfinite x = x
  | Prelude.otherwise = Prelude.fromIntegral (Prelude.round x :: Integer)

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

-- Constraint-polymorphic ('numeric') arithmetic over raw terms.
--
-- These implementations back the polymorphic add/sub/mul/negate primitives, which are
-- registered with identity (Term) coders (see Hydra.Overlay.Haskell.Libraries) so that the
-- runtime numeric type is discovered by dispatching on the argument's literal variant. Each
-- variant delegates to Haskell's Num instance for the underlying representation type, which
-- gives the correct per-type semantics for free: two's-complement wraparound for the
-- fixed-width integer types, arbitrary precision for bigint, and IEEE 754 for the floats.
--
-- Type inference guarantees both operands of a binary op share one 'numeric' type, so the
-- dispatch keys on the first operand and requires the second to match; a mismatch or a
-- non-numeric literal is an internal invariant violation and fails loudly.

-- | Apply a Num-polymorphic binary operation to two numeric terms, dispatching on the runtime
--   integer/float variant.
numericBinary :: String -> (forall a. Num a => a -> a -> a) -> Core.Term -> Core.Term -> Core.Term
numericBinary opName op x y = case (numericLiteral opName x, numericLiteral opName y) of
  (Core.LiteralInteger ix, Core.LiteralInteger iy) ->
    Core.TermLiteral $ Core.LiteralInteger $ integerBinary opName op ix iy
  (Core.LiteralFloat fx, Core.LiteralFloat fy) ->
    Core.TermLiteral $ Core.LiteralFloat $ floatBinary opName op fx fy
  _ -> Prelude.error $ "hydra.lib.math." Prelude.++ opName Prelude.++ ": operands are not the same numeric kind"

-- | Apply a Num-polymorphic unary operation to a numeric term.
numericUnary :: String -> (forall a. Num a => a -> a) -> Core.Term -> Core.Term
numericUnary opName op x = case numericLiteral opName x of
  Core.LiteralInteger ix -> Core.TermLiteral $ Core.LiteralInteger $ integerUnary op ix
  Core.LiteralFloat fx -> Core.TermLiteral $ Core.LiteralFloat $ floatUnary op fx
  _ -> Prelude.error $ "hydra.lib.math." Prelude.++ opName Prelude.++ ": operand is not numeric"

-- | Extract the literal from a term, failing loudly on any non-literal term.
numericLiteral :: String -> Core.Term -> Core.Literal
numericLiteral opName term = case term of
  Core.TermLiteral lit -> lit
  _ -> Prelude.error $ "hydra.lib.math." Prelude.++ opName Prelude.++ ": expected a literal term"

integerBinary :: String -> (forall a. Num a => a -> a -> a) -> Core.IntegerValue -> Core.IntegerValue -> Core.IntegerValue
integerBinary opName op ix iy = case (ix, iy) of
  (Core.IntegerValueBigint a, Core.IntegerValueBigint b) -> Core.IntegerValueBigint (op a b)
  (Core.IntegerValueInt8 a,   Core.IntegerValueInt8 b)   -> Core.IntegerValueInt8 (op a b)
  (Core.IntegerValueInt16 a,  Core.IntegerValueInt16 b)  -> Core.IntegerValueInt16 (op a b)
  (Core.IntegerValueInt32 a,  Core.IntegerValueInt32 b)  -> Core.IntegerValueInt32 (op a b)
  (Core.IntegerValueInt64 a,  Core.IntegerValueInt64 b)  -> Core.IntegerValueInt64 (op a b)
  (Core.IntegerValueUint8 a,  Core.IntegerValueUint8 b)  -> Core.IntegerValueUint8 (op a b)
  (Core.IntegerValueUint16 a, Core.IntegerValueUint16 b) -> Core.IntegerValueUint16 (op a b)
  (Core.IntegerValueUint32 a, Core.IntegerValueUint32 b) -> Core.IntegerValueUint32 (op a b)
  (Core.IntegerValueUint64 a, Core.IntegerValueUint64 b) -> Core.IntegerValueUint64 (op a b)
  _ -> Prelude.error $ "hydra.lib.math." Prelude.++ opName Prelude.++ ": integer operands differ in precision"

integerUnary :: (forall a. Num a => a -> a) -> Core.IntegerValue -> Core.IntegerValue
integerUnary op iv = case iv of
  Core.IntegerValueBigint a -> Core.IntegerValueBigint (op a)
  Core.IntegerValueInt8 a   -> Core.IntegerValueInt8 (op a)
  Core.IntegerValueInt16 a  -> Core.IntegerValueInt16 (op a)
  Core.IntegerValueInt32 a  -> Core.IntegerValueInt32 (op a)
  Core.IntegerValueInt64 a  -> Core.IntegerValueInt64 (op a)
  Core.IntegerValueUint8 a  -> Core.IntegerValueUint8 (op a)
  Core.IntegerValueUint16 a -> Core.IntegerValueUint16 (op a)
  Core.IntegerValueUint32 a -> Core.IntegerValueUint32 (op a)
  Core.IntegerValueUint64 a -> Core.IntegerValueUint64 (op a)

floatBinary :: String -> (forall a. Num a => a -> a -> a) -> Core.FloatValue -> Core.FloatValue -> Core.FloatValue
floatBinary opName op fx fy = case (fx, fy) of
  (Core.FloatValueFloat32 a, Core.FloatValueFloat32 b) -> Core.FloatValueFloat32 (op a b)
  (Core.FloatValueFloat64 a, Core.FloatValueFloat64 b) -> Core.FloatValueFloat64 (op a b)
  _ -> Prelude.error $ "hydra.lib.math." Prelude.++ opName Prelude.++ ": float operands differ in precision"

floatUnary :: (forall a. Num a => a -> a) -> Core.FloatValue -> Core.FloatValue
floatUnary op fv = case fv of
  Core.FloatValueFloat32 a -> Core.FloatValueFloat32 (op a)
  Core.FloatValueFloat64 a -> Core.FloatValueFloat64 (op a)

-- | Polymorphic addition over numeric terms.
addTerm :: Core.Term -> Core.Term -> Core.Term
addTerm = numericBinary "add" (+)

-- | Polymorphic subtraction over numeric terms.
subTerm :: Core.Term -> Core.Term -> Core.Term
subTerm = numericBinary "sub" (-)

-- | Polymorphic multiplication over numeric terms.
mulTerm :: Core.Term -> Core.Term -> Core.Term
mulTerm = numericBinary "mul" (*)

-- | Polymorphic negation over numeric terms.
negateTerm :: Core.Term -> Core.Term
negateTerm = numericUnary "negate" Prelude.negate
