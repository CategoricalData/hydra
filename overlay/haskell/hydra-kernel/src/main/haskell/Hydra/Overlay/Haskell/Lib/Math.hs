-- | Haskell implementations of hydra.lib.math primitives
{-# LANGUAGE RankNTypes #-}

module Hydra.Overlay.Haskell.Lib.Math where

import Prelude (Num, Fractional, Ord, Integral, Enum, Bool, Double, Int, Integer, Float, Maybe(..), String, (.), ($), (+), (-), (*), (/), (==), (||))
import qualified Prelude
import qualified Data.Int as I
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

-- | Divide two numbers (native call contract; monomorphized at Float or Double by the
-- caller). IEEE 754 total division: GHC's Fractional (/) already produces the IEEE
-- sentinels (±Infinity, NaN) for both Float and Double, so no special-casing is needed
-- here (cf. add/sub/mul, which are also directly the Prelude class method).
divide :: Fractional a => a -> a -> a
divide x y = x / y

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
-- variant delegates to Haskell's Num instance for the underlying representation type, then
-- (for int32/uint8/uint16/uint32/uint64, whose IntegerValue payload is an oversized container
-- rather than a matching fixed-width type — see Hydra.Core's IntegerValueInt32 :: Int,
-- IntegerValueUint32 :: I.Int64, etc.) explicitly re-narrows the result to the nominal bit
-- width. int8/int16/int64 use exactly-sized containers (I.Int8/I.Int16/I.Int64) and get
-- two's-complement wraparound for free from GHC's Num instance; bigint is unbounded by design.
-- Without the explicit narrowing, e.g. abs(minBound::int32) or (maxBound + maxBound :: uint8)
-- silently escape their nominal range instead of wrapping.
--
-- Type inference guarantees both operands of a binary op share one 'numeric' type, so the
-- dispatch keys on the first operand and requires the second to match; a mismatch or a
-- non-numeric literal is an internal invariant violation and fails loudly.

-- | Narrow a container-typed intermediate result back to the nominal signed bit width
-- (two's-complement wraparound: mask to 2^bits, then re-center into the signed range).
wrapSigned :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
wrapSigned bits raw =
  let m = (2 :: Prelude.Integer) Prelude.^ bits
      halfM = m `Prelude.div` 2
      masked = raw `Prelude.mod` m
  in if masked Prelude.>= halfM then masked Prelude.- m else masked

-- | Narrow a container-typed intermediate result back to the nominal unsigned bit width.
wrapUnsigned :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
wrapUnsigned bits raw = raw `Prelude.mod` ((2 :: Prelude.Integer) Prelude.^ bits)

wrapInt32 :: Int -> Int
wrapInt32 raw = Prelude.fromIntegral (wrapSigned 32 (Prelude.toInteger raw))

wrapUint8 :: I.Int16 -> I.Int16
wrapUint8 raw = Prelude.fromIntegral (wrapUnsigned 8 (Prelude.toInteger raw))

wrapUint16 :: Int -> Int
wrapUint16 raw = Prelude.fromIntegral (wrapUnsigned 16 (Prelude.toInteger raw))

wrapUint32 :: I.Int64 -> I.Int64
wrapUint32 raw = Prelude.fromIntegral (wrapUnsigned 32 (Prelude.toInteger raw))

wrapUint64 :: Prelude.Integer -> Prelude.Integer
wrapUint64 = wrapUnsigned 64

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
  (Core.IntegerValueInt32 a,  Core.IntegerValueInt32 b)  -> Core.IntegerValueInt32 (wrapInt32 (op a b))
  (Core.IntegerValueInt64 a,  Core.IntegerValueInt64 b)  -> Core.IntegerValueInt64 (op a b)
  (Core.IntegerValueUint8 a,  Core.IntegerValueUint8 b)  -> Core.IntegerValueUint8 (wrapUint8 (op a b))
  (Core.IntegerValueUint16 a, Core.IntegerValueUint16 b) -> Core.IntegerValueUint16 (wrapUint16 (op a b))
  (Core.IntegerValueUint32 a, Core.IntegerValueUint32 b) -> Core.IntegerValueUint32 (wrapUint32 (op a b))
  (Core.IntegerValueUint64 a, Core.IntegerValueUint64 b) -> Core.IntegerValueUint64 (wrapUint64 (op a b))
  _ -> Prelude.error $ "hydra.lib.math." Prelude.++ opName Prelude.++ ": integer operands differ in precision"

integerUnary :: (forall a. Num a => a -> a) -> Core.IntegerValue -> Core.IntegerValue
integerUnary op iv = case iv of
  Core.IntegerValueBigint a -> Core.IntegerValueBigint (op a)
  Core.IntegerValueInt8 a   -> Core.IntegerValueInt8 (op a)
  Core.IntegerValueInt16 a  -> Core.IntegerValueInt16 (op a)
  Core.IntegerValueInt32 a  -> Core.IntegerValueInt32 (wrapInt32 (op a))
  Core.IntegerValueInt64 a  -> Core.IntegerValueInt64 (op a)
  Core.IntegerValueUint8 a  -> Core.IntegerValueUint8 (wrapUint8 (op a))
  Core.IntegerValueUint16 a -> Core.IntegerValueUint16 (wrapUint16 (op a))
  Core.IntegerValueUint32 a -> Core.IntegerValueUint32 (wrapUint32 (op a))
  Core.IntegerValueUint64 a -> Core.IntegerValueUint64 (wrapUint64 (op a))

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

-- | Polymorphic absolute value over numeric terms.
absTerm :: Core.Term -> Core.Term
absTerm = numericUnary "abs" Prelude.abs

-- | Polymorphic sign function over numeric terms.
signumTerm :: Core.Term -> Core.Term
signumTerm = numericUnary "signum" Prelude.signum

-- Constraint-polymorphic ('fractional') division over raw terms.
--
-- This implementation backs the polymorphic divide primitive, which is registered with an
-- identity (Term) coder (see Hydra.Overlay.Haskell.Libraries) so that the runtime float type
-- is discovered by dispatching on the argument's literal variant. Both arms delegate to
-- GHC's Fractional (/) for the underlying representation type (Float or Double), which
-- already produces the IEEE 754 sentinels (±Infinity, NaN) for free.
--
-- Type inference guarantees both operands share one 'fractional' type, so the dispatch keys
-- on the first operand and requires the second to match; a mismatch or a non-float literal
-- is an internal invariant violation and fails loudly (same discipline as numericBinary).

-- | Apply divide to two float terms, dispatching on the runtime float32/float64 variant.
divideTerm :: Core.Term -> Core.Term -> Core.Term
divideTerm x y = case (fractionalLiteral x, fractionalLiteral y) of
  (Core.LiteralFloat fx, Core.LiteralFloat fy) ->
    Core.TermLiteral $ Core.LiteralFloat $ floatDivide fx fy
  _ -> Prelude.error "hydra.lib.math.divide: operands are not the same fractional kind"

-- | Extract the literal from a term, failing loudly on any non-literal term.
fractionalLiteral :: Core.Term -> Core.Literal
fractionalLiteral term = case term of
  Core.TermLiteral lit -> lit
  _ -> Prelude.error "hydra.lib.math.divide: expected a literal term"

floatDivide :: Core.FloatValue -> Core.FloatValue -> Core.FloatValue
floatDivide fx fy = case (fx, fy) of
  (Core.FloatValueFloat32 a, Core.FloatValueFloat32 b) -> Core.FloatValueFloat32 (divide a b)
  (Core.FloatValueFloat64 a, Core.FloatValueFloat64 b) -> Core.FloatValueFloat64 (divide a b)
  _ -> Prelude.error "hydra.lib.math.divide: float operands differ in precision"

-- Constraint-polymorphic ('integral') division/modulus/remainder/parity over raw terms.
--
-- These implementations back the polymorphic div/mod/rem/even primitives, registered with
-- identity (Term) coders (see Hydra.Overlay.Haskell.Libraries) so the runtime integer type
-- is discovered by dispatching on the argument's literal variant. div/mod are floor-based
-- (sign follows the divisor); rem is truncated (sign follows the dividend) — this matches
-- GHC's own div/mod vs quot/rem split. Both dispatch families guard the zero-divisor case
-- (returning Nothing) BEFORE calling the underlying Integral operation.
--
-- The (minBound, -1) boundary needs an explicit guard on div only: GHC's div/quot for
-- Int8/Int16/Int32/Int64 throw "arithmetic overflow" there (the quotient +maxBound+1 is not
-- representable), while mod/rem do NOT trap (Haskell special-cases them to 0 at that point,
-- since no overflow occurs in the remainder). Guarding div wraps the quotient to minBound,
-- matching the two's-complement contract (design round4 finding E1). Bigint/uint has no such
-- boundary (unbounded / no negative domain).

-- | Apply floor division to two integer terms, dispatching on the runtime integer variant.
divTerm :: Core.Term -> Core.Term -> Maybe Core.Term
divTerm x y = case (integralLiteral "div" x, integralLiteral "div" y) of
  (Core.IntegerValueBigint a, Core.IntegerValueBigint b) ->
    integralResult $ Core.IntegerValueBigint Prelude.<$> maybeDivBounded Prelude.Nothing a b
  (Core.IntegerValueInt8 a,   Core.IntegerValueInt8 b)   ->
    integralResult $ Core.IntegerValueInt8 Prelude.<$> maybeDivBounded (Prelude.Just (Prelude.minBound, -1)) a b
  (Core.IntegerValueInt16 a,  Core.IntegerValueInt16 b)  ->
    integralResult $ Core.IntegerValueInt16 Prelude.<$> maybeDivBounded (Prelude.Just (Prelude.minBound, -1)) a b
  (Core.IntegerValueInt32 a,  Core.IntegerValueInt32 b)  ->
    integralResult $ Core.IntegerValueInt32 Prelude.<$> maybeDivBounded (Prelude.Just (-2147483648, -1)) a b
  (Core.IntegerValueInt64 a,  Core.IntegerValueInt64 b)  ->
    integralResult $ Core.IntegerValueInt64 Prelude.<$> maybeDivBounded (Prelude.Just (Prelude.minBound, -1)) a b
  (Core.IntegerValueUint8 a,  Core.IntegerValueUint8 b)  ->
    integralResult $ Core.IntegerValueUint8 Prelude.<$> maybeDivBounded Prelude.Nothing a b
  (Core.IntegerValueUint16 a, Core.IntegerValueUint16 b) ->
    integralResult $ Core.IntegerValueUint16 Prelude.<$> maybeDivBounded Prelude.Nothing a b
  (Core.IntegerValueUint32 a, Core.IntegerValueUint32 b) ->
    integralResult $ Core.IntegerValueUint32 Prelude.<$> maybeDivBounded Prelude.Nothing a b
  (Core.IntegerValueUint64 a, Core.IntegerValueUint64 b) ->
    integralResult $ Core.IntegerValueUint64 Prelude.<$> maybeDivBounded Prelude.Nothing a b
  _ -> Prelude.error "hydra.lib.math.div: integer operands differ in precision"

-- | div, guarded against the zero-divisor case and (when a boundary pair is given) the
-- minBound/-1 overflow case, which is wrapped rather than left to trap.
maybeDivBounded :: (Integral a) => Maybe (a, a) -> a -> a -> Maybe a
maybeDivBounded boundary x y
  | y Prelude.== 0 = Prelude.Nothing
  | Prelude.Just (mn, negOne) <- boundary, x Prelude.== mn, y Prelude.== negOne = Prelude.Just mn
  | Prelude.otherwise = Prelude.Just (Prelude.div x y)

-- | Apply floor modulus to two integer terms, dispatching on the runtime integer variant.
modTerm :: Core.Term -> Core.Term -> Maybe Core.Term
modTerm x y = case (integralLiteral "mod" x, integralLiteral "mod" y) of
  (Core.IntegerValueBigint a, Core.IntegerValueBigint b) -> integralResult $ Core.IntegerValueBigint Prelude.<$> maybeMod a b
  (Core.IntegerValueInt8 a,   Core.IntegerValueInt8 b)   -> integralResult $ Core.IntegerValueInt8 Prelude.<$> maybeMod a b
  (Core.IntegerValueInt16 a,  Core.IntegerValueInt16 b)  -> integralResult $ Core.IntegerValueInt16 Prelude.<$> maybeMod a b
  (Core.IntegerValueInt32 a,  Core.IntegerValueInt32 b)  -> integralResult $ Core.IntegerValueInt32 Prelude.<$> maybeMod a b
  (Core.IntegerValueInt64 a,  Core.IntegerValueInt64 b)  -> integralResult $ Core.IntegerValueInt64 Prelude.<$> maybeMod a b
  (Core.IntegerValueUint8 a,  Core.IntegerValueUint8 b)  -> integralResult $ Core.IntegerValueUint8 Prelude.<$> maybeMod a b
  (Core.IntegerValueUint16 a, Core.IntegerValueUint16 b) -> integralResult $ Core.IntegerValueUint16 Prelude.<$> maybeMod a b
  (Core.IntegerValueUint32 a, Core.IntegerValueUint32 b) -> integralResult $ Core.IntegerValueUint32 Prelude.<$> maybeMod a b
  (Core.IntegerValueUint64 a, Core.IntegerValueUint64 b) -> integralResult $ Core.IntegerValueUint64 Prelude.<$> maybeMod a b
  _ -> Prelude.error "hydra.lib.math.mod: integer operands differ in precision"

maybeMod :: (Integral a) => a -> a -> Maybe a
maybeMod x y
  | y Prelude.== 0 = Prelude.Nothing
  | Prelude.otherwise = Prelude.Just (Prelude.mod x y)

-- | Apply truncated remainder to two integer terms, dispatching on the runtime integer variant.
remTerm :: Core.Term -> Core.Term -> Maybe Core.Term
remTerm x y = case (integralLiteral "rem" x, integralLiteral "rem" y) of
  (Core.IntegerValueBigint a, Core.IntegerValueBigint b) -> integralResult $ Core.IntegerValueBigint Prelude.<$> maybeRem a b
  (Core.IntegerValueInt8 a,   Core.IntegerValueInt8 b)   -> integralResult $ Core.IntegerValueInt8 Prelude.<$> maybeRem a b
  (Core.IntegerValueInt16 a,  Core.IntegerValueInt16 b)  -> integralResult $ Core.IntegerValueInt16 Prelude.<$> maybeRem a b
  (Core.IntegerValueInt32 a,  Core.IntegerValueInt32 b)  -> integralResult $ Core.IntegerValueInt32 Prelude.<$> maybeRem a b
  (Core.IntegerValueInt64 a,  Core.IntegerValueInt64 b)  -> integralResult $ Core.IntegerValueInt64 Prelude.<$> maybeRem a b
  (Core.IntegerValueUint8 a,  Core.IntegerValueUint8 b)  -> integralResult $ Core.IntegerValueUint8 Prelude.<$> maybeRem a b
  (Core.IntegerValueUint16 a, Core.IntegerValueUint16 b) -> integralResult $ Core.IntegerValueUint16 Prelude.<$> maybeRem a b
  (Core.IntegerValueUint32 a, Core.IntegerValueUint32 b) -> integralResult $ Core.IntegerValueUint32 Prelude.<$> maybeRem a b
  (Core.IntegerValueUint64 a, Core.IntegerValueUint64 b) -> integralResult $ Core.IntegerValueUint64 Prelude.<$> maybeRem a b
  _ -> Prelude.error "hydra.lib.math.rem: integer operands differ in precision"

maybeRem :: (Integral a) => a -> a -> Maybe a
maybeRem x y
  | y Prelude.== 0 = Prelude.Nothing
  | Prelude.otherwise = Prelude.Just (Prelude.rem x y)

-- | Apply a parity test to an integer term, dispatching on the runtime integer variant.
evenTerm :: Core.Term -> Bool
evenTerm x = case integralLiteral "even" x of
  Core.IntegerValueBigint a -> Prelude.even a
  Core.IntegerValueInt8 a   -> Prelude.even a
  Core.IntegerValueInt16 a  -> Prelude.even a
  Core.IntegerValueInt32 a  -> Prelude.even a
  Core.IntegerValueInt64 a  -> Prelude.even a
  Core.IntegerValueUint8 a  -> Prelude.even a
  Core.IntegerValueUint16 a -> Prelude.even a
  Core.IntegerValueUint32 a -> Prelude.even a
  Core.IntegerValueUint64 a -> Prelude.even a

-- | Apply a parity test (odd) to an integer term, dispatching on the runtime integer variant.
oddTerm :: Core.Term -> Bool
oddTerm x = case integralLiteral "odd" x of
  Core.IntegerValueBigint a -> Prelude.odd a
  Core.IntegerValueInt8 a   -> Prelude.odd a
  Core.IntegerValueInt16 a  -> Prelude.odd a
  Core.IntegerValueInt32 a  -> Prelude.odd a
  Core.IntegerValueInt64 a  -> Prelude.odd a
  Core.IntegerValueUint8 a  -> Prelude.odd a
  Core.IntegerValueUint16 a -> Prelude.odd a
  Core.IntegerValueUint32 a -> Prelude.odd a
  Core.IntegerValueUint64 a -> Prelude.odd a

-- | Extract the integer value from a term, failing loudly on any non-integer literal term.
integralLiteral :: String -> Core.Term -> Core.IntegerValue
integralLiteral opName term = case term of
  Core.TermLiteral (Core.LiteralInteger iv) -> iv
  _ -> Prelude.error $ "hydra.lib.math." Prelude.++ opName Prelude.++ ": expected an integer literal term"

-- | Wrap a Maybe integer result as a Term-encoded optional integer literal.
integralResult :: Maybe Core.IntegerValue -> Maybe Core.Term
integralResult Prelude.Nothing = Prelude.Nothing
integralResult (Prelude.Just iv) = Prelude.Just (Core.TermLiteral (Core.LiteralInteger iv))
