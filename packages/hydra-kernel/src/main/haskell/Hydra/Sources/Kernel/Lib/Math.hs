-- | Primitive declarations for the hydra.lib.math namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Math where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), abs, acos, acosh, asin, asinh, atan, atan2, atanh,
                               ceiling, cos, cosh, div, even, exp, floor, log, logBase,
                               max, min, mod, negate, odd, pi, rem, round, signum, sin, sinh,
                               sqrt, tan, tanh, truncate)
import qualified Data.Int                    as I


ns :: ModuleName
ns = ModuleName "hydra.lib.math"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.math module.")}
  where
    definitions = [abs, acos, acosh, add, addFloat64, asin, asinh, atan, atan2, atanh,
                   ceiling, cos, cosh, div, e, even, exp, floor, log, logBase, mod,
                   mul, mulFloat64, negate,
                   negateFloat64, odd, pi, pow, range, rem, round, roundFloat32, roundFloat64,
                   signum, sin, sinh, sqrt, sub, subFloat64, tan, tanh, truncate]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Shared type variable for numeric-polymorphic signatures.
tx :: Type
tx = Types.var "x"

-- Constraint-polymorphic 'numeric' signatures (numeric x => ...).
-- These mirror the argument-driven pattern used by equality/ordering primitives.
numericTo :: TermSignature
numericTo = sig $ Types.polyConstrained [("x", [Name "numeric"])] (tx Types.~> tx)

numericTo2 :: TermSignature
numericTo2 = sig $ Types.polyConstrained [("x", [Name "numeric"])] (tx Types.~> tx Types.~> tx)

-- Shared monomorphic signatures.
f64Const :: TermSignature
f64Const = sig $ TypeScheme [] Types.float64 Nothing

f64To :: [(String, String)] -> TermSignature
f64To params = sigWithParams params $ TypeScheme [] (Types.float64 Types.~> Types.float64) Nothing

f64To2 :: [(String, String)] -> TermSignature
f64To2 params = sigWithParams params $ TypeScheme [] (Types.float64 Types.~> Types.float64 Types.~> Types.float64) Nothing

int32F32ToF32 :: [(String, String)] -> TermSignature
int32F32ToF32 params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.float32 Types.~> Types.float32) Nothing

int32F64ToF64 :: [(String, String)] -> TermSignature
int32F64ToF64 params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.float64 Types.~> Types.float64) Nothing

int32To :: [(String, String)] -> TermSignature
int32To params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.int32) Nothing

int32To2 :: [(String, String)] -> TermSignature
int32To2 params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.int32 Types.~> Types.int32) Nothing

int32To2List :: [(String, String)] -> TermSignature
int32To2List params = sigWithParams params $ TypeScheme []
  (Types.int32 Types.~> Types.int32 Types.~> Types.list Types.int32) Nothing

int32To2Maybe :: [(String, String)] -> TermSignature
int32To2Maybe params = sigWithParams params $ TypeScheme []
  (Types.int32 Types.~> Types.int32 Types.~> Types.optional Types.int32) Nothing

int32ToBool :: [(String, String)] -> TermSignature
int32ToBool params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.boolean) Nothing

int32ToMaybe :: [(String, String)] -> TermSignature
int32ToMaybe params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.optional Types.int32) Nothing

abs :: PrimitiveDefinition
abs = define "abs" "The absolute value of an integer." (int32To [("x", "the integer whose absolute value is taken")])
  ["Absolute value of a signed 32-bit two's-complement integer. For non-negative inputs the result\
  \ equals the input; for negative inputs the result is the arithmetic negation.",
   "The function is total but not injective at the boundary: abs(minBound) = minBound (i.e.\
  \ abs(-2147483648) = -2147483648), because +2147483648 is not representable in int32.",
   "Corresponds to Haskell's abs :: Int32 -> Int32."]

acos :: PrimitiveDefinition
acos = define "acos" "The arc cosine of a floating-point number." (f64To [("x", "the cosine value whose arc cosine is computed")])
  ["Principal value of the inverse cosine, in radians. The result is in [0, \x03C0].",
   "For arguments outside the domain [-1, +1] (including \xB1\x221E), the result is NaN. acos(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 acos operation and to Haskell's acos :: Double -> Double."]

acosh :: PrimitiveDefinition
acosh = define "acosh" "The hyperbolic arc cosine of a floating-point number." (f64To [("x", "the value whose inverse hyperbolic cosine is computed")])
  ["Principal value of the inverse hyperbolic cosine. The result is in [0, +\x221E).",
   "For arguments less than 1 the result is NaN; acosh(1) = +0; acosh(+\x221E) = +\x221E; acosh(NaN) is\
  \ NaN.",
   "Corresponds to the IEEE 754 \xA79.2 acosh operation and to Haskell's acosh :: Double -> Double."]

add :: PrimitiveDefinition
add = define "add" "Numeric addition." numericTo2
  ["Constraint-polymorphic addition over any type with a 'numeric' instance. The per-type semantics\
  \ live in the instance: two's-complement wraparound for fixed-width integers (e.g. int32 addition\
  \ reduces x + y modulo 2^32 and reinterprets as signed, wrapping silently on overflow),\
  \ IEEE 754 round-to-nearest for floating-point, and arbitrary precision for bigint.",
   "Requires a 'numeric' type-class constraint on the argument type, which is the closest Hydra\
   \ equivalent to Haskell's Num instance.",
   "The operation is total. Corresponds to Haskell's (+) :: Num a => a -> a -> a."]

addFloat64 :: PrimitiveDefinition
addFloat64 = define "addFloat64" "Floating-point addition." (f64To2 [("x", "the first addend"), ("y", "the second addend")])
  ["IEEE 754 binary64 addition. The result is the value of x + y rounded to the nearest representable\
  \ float64 under the roundTiesToEven rounding-direction attribute.",
   "Adding infinities of opposite sign (+\x221E + -\x221E or -\x221E + +\x221E) produces a NaN; adding\
  \ any value to NaN produces a NaN. The sum of two zeros is +0, except (-0) + (-0) = -0.",
   "The operation is total: it never raises, but it may produce NaN. Corresponds to the IEEE 754\
  \ \xA75.4.1 addition operation and to Haskell's (+) :: Double -> Double -> Double."]

asin :: PrimitiveDefinition
asin = define "asin" "The arc sine of a floating-point number." (f64To [("x", "the sine value whose arc sine is computed")])
  ["Principal value of the inverse sine, in radians. The result is in [-\x03C0/2, +\x03C0/2].",
   "For arguments outside the domain [-1, +1] (including \xB1\x221E), the result is NaN; asin(\xB10) =\
  \ \xB10; asin(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 asin operation and to Haskell's asin :: Double -> Double."]

asinh :: PrimitiveDefinition
asinh = define "asinh" "The hyperbolic arc sine of a floating-point number." (f64To [("x", "the value whose inverse hyperbolic sine is computed")])
  ["Principal value of the inverse hyperbolic sine. Defined for all finite reals; asinh(\xB10) = \xB10;\
  \ asinh(\xB1\x221E) = \xB1\x221E; asinh(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 asinh operation and to Haskell's asinh :: Double -> Double."]

atan :: PrimitiveDefinition
atan = define "atan" "The arc tangent of a floating-point number." (f64To [("x", "the tangent value whose arc tangent is computed")])
  ["Principal value of the inverse tangent, in radians. The result is in (-\x03C0/2, +\x03C0/2);\
  \ atan(\xB10) = \xB10; atan(\xB1\x221E) = \xB1\x03C0/2; atan(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 atan operation and to Haskell's atan :: Double -> Double."]

atan2 :: PrimitiveDefinition
atan2 = define "atan2" "The two-argument arc tangent (atan2)." (f64To2 [("y", "the ordinate (y-coordinate)"), ("x", "the abscissa (x-coordinate)")])
  ["atan2(y, x) returns the angle in radians, in (-\x03C0, +\x03C0], from the positive x-axis to the\
  \ point (x, y), using the signs of both arguments to determine the quadrant.",
   "Special cases follow IEEE 754 \xA79.2: atan2(\xB10, +x>0) = \xB10; atan2(\xB10, -x<0) = \xB1\x03C0;\
  \ atan2(\xB1y>0, 0) = \xB1\x03C0/2; atan2(\xB1y, +\x221E) = \xB10; atan2(\xB1y, -\x221E) = \xB1\x03C0;\
  \ atan2(\xB1\x221E, finite) = \xB1\x03C0/2; atan2(\xB1\x221E, +\x221E) = \xB1\x03C0/4;\
  \ atan2(\xB1\x221E, -\x221E) = \xB13\x03C0/4; atan2 of any NaN argument is NaN.",
   "Corresponds to Haskell's atan2 :: Double -> Double -> Double."]

atanh :: PrimitiveDefinition
atanh = define "atanh" "The hyperbolic arc tangent of a floating-point number." (f64To [("x", "the value whose inverse hyperbolic tangent is computed")])
  ["Principal value of the inverse hyperbolic tangent. Defined for arguments in (-1, +1); atanh(\xB11) =\
  \ \xB1\x221E (with division-by-zero exception in IEEE 754); arguments outside [-1, +1] produce NaN;\
  \ atanh(\xB10) = \xB10; atanh(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 atanh operation and to Haskell's atanh :: Double -> Double."]

ceiling :: PrimitiveDefinition
ceiling = define "ceiling" "The smallest integer greater than or equal to the argument, as a float." (f64To [("x", "the value to round up toward positive infinity")])
  ["The smallest integer value not less than the argument, returned as a float64. Equivalent to IEEE 754\
  \ \xA75.3.1 roundToIntegralTowardPositive: ceiling(\xB10) = \xB10; ceiling(\xB1\x221E) = \xB1\x221E;\
  \ ceiling(NaN) is NaN; the sign of a negative result is preserved when rounding to zero (e.g.\
  \ ceiling(-0.5) = -0).",
   "Note that the return type is float64, not an integer type, so the result can exceed the integer\
  \ range.",
   "Corresponds to Haskell's fromIntegral . ceiling :: Double -> Double."]

cos :: PrimitiveDefinition
cos = define "cos" "The cosine of a floating-point number." (f64To [("x", "the angle in radians")])
  ["Cosine of an angle in radians, correctly rounded for finite arguments. The result is in [-1, +1];\
  \ cos(\xB10) = 1; cos(\xB1\x221E) is NaN (with invalid-operation exception in IEEE 754); cos(NaN) is\
  \ NaN.",
   "Corresponds to the IEEE 754 \xA79.2 cos operation and to Haskell's cos :: Double -> Double."]

cosh :: PrimitiveDefinition
cosh = define "cosh" "The hyperbolic cosine of a floating-point number." (f64To [("x", "the value whose hyperbolic cosine is computed")])
  ["Hyperbolic cosine. The result is in [1, +\x221E]; cosh(\xB10) = 1; cosh(\xB1\x221E) = +\x221E;\
  \ cosh(NaN) is NaN. Large-magnitude arguments overflow to +\x221E.",
   "Corresponds to the IEEE 754 \xA79.2 cosh operation and to Haskell's cosh :: Double -> Double."]

e :: PrimitiveDefinition
e = define "e" "Euler's constant (the base of the natural logarithm)." f64Const
  ["The mathematical constant e \x2248 2.718281828459045, the base of the natural logarithm, as the\
  \ nearest representable float64. Equal to exp(1).",
   "Corresponds to Haskell's exp 1 :: Double."]

even :: PrimitiveDefinition
even = defineWithDefault "even" "Test whether an integer is even." (int32ToBool [("x", "the integer to test for evenness")])
  ["True if the argument is divisible by 2 (i.e. x mod 2 = 0), false otherwise.",
   "Total on all int32 inputs including negative numbers and minBound. Corresponds to Haskell's\
  \ even :: Int32 -> Bool."]
  (("x" ~> Equality.equal
    (Optionals.withDefault (int32 0) (Math.mod (var "x") (int32 2)))
    (int32 0)) :: TypedTerm (I.Int32 -> Bool))

exp :: PrimitiveDefinition
exp = define "exp" "The exponential function." (f64To [("x", "the exponent")])
  ["The exponential function: exp(x) = e^x. exp(\xB10) = 1; exp(-\x221E) = +0; exp(+\x221E) = +\x221E;\
  \ exp(NaN) is NaN. Large positive arguments overflow to +\x221E; large negative arguments underflow\
  \ to +0.",
   "Corresponds to the IEEE 754 \xA79.2 exp operation and to Haskell's exp :: Double -> Double."]

floor :: PrimitiveDefinition
floor = define "floor" "The largest integer less than or equal to the argument, as a float." (f64To [("x", "the value to round down toward negative infinity")])
  ["The largest integer value not greater than the argument, returned as a float64. Equivalent to IEEE\
  \ 754 \xA75.3.1 roundToIntegralTowardNegative: floor(\xB10) = \xB10; floor(\xB1\x221E) = \xB1\x221E;\
  \ floor(NaN) is NaN.",
   "Note that the return type is float64, not an integer type, so the result can exceed the integer\
  \ range.",
   "Corresponds to Haskell's fromIntegral . floor :: Double -> Double."]

log :: PrimitiveDefinition
log = define "log" "The natural logarithm." (f64To [("x", "the value whose natural logarithm is computed")])
  ["The natural (base-e) logarithm. log(1) = +0; log(+0) = log(-0) = -\x221E (with division-by-zero\
  \ exception in IEEE 754); log(x) for x < 0 is NaN (with invalid-operation exception); log(+\x221E) =\
  \ +\x221E; log(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 log operation and to Haskell's log :: Double -> Double."]

logBase :: PrimitiveDefinition
logBase = define "logBase" "Logarithm of the second argument in the base of the first." (f64To2 [("b", "the logarithm base"), ("x", "the value whose logarithm is computed")])
  ["logBase(b, x) computes the logarithm of x in base b, equivalent to log(x) / log(b).",
   "Inherits the special-case behavior of log for each argument (NaN propagation, sign of zeros,\
  \ division by zero on log of a zero, NaN on negative arguments).",
   "Corresponds to Haskell's logBase :: Double -> Double -> Double."]

div :: PrimitiveDefinition
div = define "div" "Integer division, or Nothing if dividing by zero." (int32To2Maybe [("x", "the dividend"), ("y", "the divisor")])
  ["Total integer division: div(x, y) returns Just(x divided by y, rounded toward negative\
  \ infinity) when y is non-zero, and Nothing when y = 0.",
   "The division rounds toward negative infinity (floor), so for example div(-7, 2) = Just(-4).",
   "The boundary case div(minBound, -1), whose mathematical result +2147483648 is not representable\
  \ in int32, wraps to minBound (the two's-complement overflow).",
   "Corresponds to Haskell's div :: Int32 -> Int32 -> Int32, wrapped in maybe to make the zero-divisor\
  \ case total."]

mod :: PrimitiveDefinition
mod = define "mod" "Integer modulus, or Nothing if dividing by zero." (int32To2Maybe [("x", "the dividend"), ("y", "the divisor")])
  ["Total integer modulus: mod(x, y) returns Just(x mod y) when y is non-zero, and Nothing when y =\
  \ 0.",
   "The result satisfies the identity x = (div(x, y) result) * y + (mod(x, y) result), so the\
  \ sign of the result matches the sign of y (Knuth-style floor division). For example mod(-7, 2)\
  \ = Just(1).",
   "Corresponds to Haskell's mod :: Int32 -> Int32 -> Int32, wrapped in maybe to make the zero-divisor\
  \ case total."]

rem :: PrimitiveDefinition
rem = define "rem" "Integer remainder, or Nothing if dividing by zero." (int32To2Maybe [("x", "the dividend"), ("y", "the divisor")])
  ["Total integer remainder: rem(x, y) returns Just(x rem y) when y is non-zero, and Nothing when y\
  \ = 0.",
   "The result satisfies x = (truncate(x / y)) * y + (rem(x, y) result), so the sign of the result\
  \ matches the sign of x (truncated division, C-style remainder). For example rem(-7, 2) =\
  \ Just(-1).",
   "The boundary case rem(minBound, -1) is 0 (no overflow, since the quotient overflow is\
  \ absorbed).",
   "Corresponds to Haskell's rem :: Int32 -> Int32 -> Int32, wrapped in maybe to make the zero-divisor\
  \ case total."]

mul :: PrimitiveDefinition
mul = define "mul" "Numeric multiplication." numericTo2
  ["Constraint-polymorphic multiplication over any type with a 'numeric' instance. The per-type\
  \ semantics live in the instance: two's-complement wraparound for fixed-width integers (e.g. int32\
  \ multiplication reduces x * y modulo 2^32 and reinterprets as signed, wrapping silently on\
  \ overflow), IEEE 754 round-to-nearest for floating-point, and arbitrary precision for bigint.",
   "Requires a 'numeric' type-class constraint on the argument type.",
   "The operation is total. Corresponds to Haskell's (*) :: Num a => a -> a -> a."]

mulFloat64 :: PrimitiveDefinition
mulFloat64 = define "mulFloat64" "Floating-point multiplication." (f64To2 [("x", "the first factor"), ("y", "the second factor")])
  ["IEEE 754 binary64 multiplication. The result is the value of x * y rounded to the nearest\
  \ representable float64 under the roundTiesToEven rounding-direction attribute.",
   "Multiplying 0 by \xB1\x221E (in either order) produces a NaN (with invalid-operation exception in\
  \ IEEE 754); multiplying any value by NaN produces a NaN. The result's sign is the XOR of the operand\
  \ signs (\xB10 * \xB10 = \xB10 accordingly).",
   "The operation is total: it never raises, but it may produce NaN or \xB1\x221E. Corresponds to the\
  \ IEEE 754 \xA75.4.1 multiplication operation and to Haskell's (*) :: Double -> Double -> Double."]

negate :: PrimitiveDefinition
negate = define "negate" "Numeric negation." numericTo
  ["Constraint-polymorphic arithmetic negation over any type with a 'numeric' instance. The per-type\
  \ semantics live in the instance: for fixed-width integers the result is 0 - x reduced modulo 2^32\
  \ and reinterpreted as signed (total but not injective at the boundary: negate(minBound) = minBound,\
  \ because +2147483648 is not representable in int32); for floating-point it flips the sign bit per\
  \ IEEE 754 \xA75.5.1.",
   "Requires a 'numeric' type-class constraint on the argument type.",
   "Corresponds to Haskell's negate :: Num a => a -> a."]

negateFloat64 :: PrimitiveDefinition
negateFloat64 = define "negateFloat64" "Negate a floating-point number." (f64To [("x", "the value to negate")])
  ["Sign reversal of a float64. Equivalent to IEEE 754 \xA75.5.1 negate: flips the sign bit, so\
  \ negate(\xB10) = \xB10 (sign flips), negate(\xB1\x221E) = \xB1\x221E (sign flips), and negate(NaN) is\
  \ a NaN (sign may flip; payload preserved).",
   "This is a bit-level operation that does not raise any floating-point exception. Corresponds to\
  \ Haskell's negate :: Double -> Double."]

odd :: PrimitiveDefinition
odd = defineWithDefault "odd" "Test whether an integer is odd." (int32ToBool [("x", "the integer to test for oddness")])
  ["True if the argument is not divisible by 2 (i.e. x mod 2 \x2260 0), false otherwise.",
   "Total on all int32 inputs including negative numbers and minBound. Corresponds to Haskell's\
  \ odd :: Int32 -> Bool."]
  (("x" ~> Logic.not (Math.even (var "x"))) :: TypedTerm (I.Int32 -> Bool))

pi :: PrimitiveDefinition
pi = define "pi" "The mathematical constant pi." f64Const
  ["The mathematical constant \x03C0 \x2248 3.141592653589793, the ratio of a circle's circumference to\
  \ its diameter, as the nearest representable float64.",
   "Corresponds to Haskell's pi :: Double."]

pow :: PrimitiveDefinition
pow = define "pow" "Raise the first argument to the power of the second." (f64To2 [("x", "the base"), ("y", "the exponent")])
  ["pow(x, y) = x^y.",
   "Follows the IEEE 754 \xA79.2 pow operation: pow(\xB10, y) for y < 0 is \xB1\x221E (with\
  \ division-by-zero exception); pow(\xB10, y) for y > 0 is \xB10 if y is an odd integer, else +0;\
  \ pow(1, y) = 1 for any y including NaN; pow(x, \xB10) = 1 for any x including NaN; pow(x, y) for\
  \ negative x and non-integer y is NaN (with invalid-operation exception); pow(\xB1\x221E, y) follows\
  \ the limits in the usual way. Otherwise the result is x^y rounded to the nearest representable\
  \ float64.",
   "Corresponds to Haskell's (**) :: Double -> Double -> Double."]

range :: PrimitiveDefinition
range = define "range" "Construct the inclusive integer range from the first to the second argument." (int32To2List [("a", "the inclusive lower bound of the range"), ("b", "the inclusive upper bound of the range")])
  ["range(a, b) returns the list [a, a+1, ..., b]. The range is inclusive at both ends; if a > b the\
  \ result is the empty list (i.e. the range does not count downward). For a = b the result is the\
  \ singleton [a]. The length of the result is max(0, b - a + 1).",
   "Corresponds to Haskell's enumFromTo :: Int32 -> Int32 -> [Int32], equivalent to the\
  \ list-comprehension form [a..b]."]

round :: PrimitiveDefinition
round = define "round" "Round a floating-point number to the nearest integer-valued float." (f64To [("x", "the value to round to the nearest integer")])
  ["Round to the nearest integer value, returned as a float64, with ties rounded to the nearest even\
  \ integer (banker's rounding). Equivalent to IEEE 754 \xA75.3.1 roundToIntegralTiesToEven:\
  \ round(\xB10) = \xB10; round(\xB1\x221E) = \xB1\x221E; round(NaN) is NaN.",
   "Note that the return type is float64, not an integer type, so the result can exceed the integer\
  \ range.",
   "Corresponds to Haskell's fromIntegral . round :: Double -> Double."]

roundFloat32 :: PrimitiveDefinition
roundFloat32 = define "roundFloat32" "Round a float32 to the given number of decimal places." (int32F32ToF32 [("n", "the number of decimal places to round to"), ("x", "the float32 value to round")])
  ["roundFloat32(n, x) rounds the float32 value x to n decimal places using round-half-to-even. The\
  \ result is the nearest float32 representation of x rounded to that decimal precision; if the exact\
  \ decimal-rounded value is not representable in float32 (the usual case), the closest float32 is\
  \ returned.",
   "Special values pass through: roundFloat32(n, \xB10) = \xB10; roundFloat32(n, \xB1\x221E) =\
  \ \xB1\x221E; roundFloat32(n, NaN) is NaN.",
   "Negative n is supported in principle (rounding to powers of ten above 1); host implementations may\
  \ differ on out-of-range n."]

roundFloat64 :: PrimitiveDefinition
roundFloat64 = define "roundFloat64" "Round a float64 to the given number of decimal places." (int32F64ToF64 [("n", "the number of decimal places to round to"), ("x", "the float64 value to round")])
  ["roundFloat64(n, x) rounds the float64 value x to n decimal places using round-half-to-even. The\
  \ result is the nearest float64 representation of x rounded to that decimal precision; if the exact\
  \ decimal-rounded value is not representable in float64 (the usual case), the closest float64 is\
  \ returned.",
   "Special values pass through: roundFloat64(n, \xB10) = \xB10; roundFloat64(n, \xB1\x221E) =\
  \ \xB1\x221E; roundFloat64(n, NaN) is NaN.",
   "Negative n is supported in principle (rounding to powers of ten above 1); host implementations may\
  \ differ on out-of-range n."]

signum :: PrimitiveDefinition
signum = define "signum" "Return the sign of an integer as -1, 0, or 1." (int32To [("x", "the integer whose sign is returned")])
  ["signum(x) returns -1 if x < 0, 0 if x = 0, and 1 if x > 0.",
   "The function is total and satisfies the identity abs(x) * signum(x) = x for all int32 except\
  \ minBound (where abs(minBound) * (-1) wraps to minBound rather than equalling -minBound, since\
  \ +2147483648 is not representable).",
   "Corresponds to Haskell's signum :: Int32 -> Int32."]

sin :: PrimitiveDefinition
sin = define "sin" "The sine of a floating-point number." (f64To [("x", "the angle in radians")])
  ["Sine of an angle in radians, correctly rounded for finite arguments. The result is in [-1, +1];\
  \ sin(\xB10) = \xB10; sin(\xB1\x221E) is NaN (with invalid-operation exception in IEEE 754); sin(NaN)\
  \ is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 sin operation and to Haskell's sin :: Double -> Double."]

sinh :: PrimitiveDefinition
sinh = define "sinh" "The hyperbolic sine of a floating-point number." (f64To [("x", "the value whose hyperbolic sine is computed")])
  ["Hyperbolic sine. sinh(\xB10) = \xB10; sinh(\xB1\x221E) = \xB1\x221E; sinh(NaN) is NaN.\
  \ Large-magnitude arguments overflow to \xB1\x221E.",
   "Corresponds to the IEEE 754 \xA79.2 sinh operation and to Haskell's sinh :: Double -> Double."]

sqrt :: PrimitiveDefinition
sqrt = define "sqrt" "The non-negative square root of a floating-point number." (f64To [("x", "the value whose square root is computed")])
  ["IEEE 754 binary64 square root. The result is the value of \x221A\&x correctly rounded to the nearest\
  \ representable float64 under roundTiesToEven.",
   "sqrt(+0) = +0; sqrt(-0) = -0 (sign preserved); sqrt(x) for x < 0 (including -\x221E) is NaN (with\
  \ invalid-operation exception); sqrt(+\x221E) = +\x221E; sqrt(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA75.4.1 squareRoot operation and to Haskell's sqrt :: Double ->\
  \ Double."]

sub :: PrimitiveDefinition
sub = define "sub" "Numeric subtraction." numericTo2
  ["Constraint-polymorphic subtraction over any type with a 'numeric' instance. The per-type semantics\
  \ live in the instance: two's-complement wraparound for fixed-width integers (e.g. int32 subtraction\
  \ reduces x - y modulo 2^32 and reinterprets as signed, wrapping silently on overflow), IEEE 754\
  \ round-to-nearest for floating-point, and arbitrary precision for bigint.",
   "Requires a 'numeric' type-class constraint on the argument type.",
   "The operation is total. Corresponds to Haskell's (-) :: Num a => a -> a -> a."]

subFloat64 :: PrimitiveDefinition
subFloat64 = define "subFloat64" "Floating-point subtraction." (f64To2 [("x", "the minuend"), ("y", "the subtrahend")])
  ["IEEE 754 binary64 subtraction, defined as x + (-y). The result is correctly rounded to the nearest\
  \ representable float64 under roundTiesToEven.",
   "Subtracting infinities of the same sign (+\x221E - +\x221E or -\x221E - -\x221E) produces a NaN;\
  \ subtracting any value involving NaN produces a NaN. The difference of two equal finite values is +0\
  \ (or -0 under round-toward-negative, which is not the default).",
   "The operation is total: it never raises, but it may produce NaN. Corresponds to the IEEE 754\
  \ \xA75.4.1 subtraction operation and to Haskell's (-) :: Double -> Double -> Double."]

tan :: PrimitiveDefinition
tan = define "tan" "The tangent of a floating-point number." (f64To [("x", "the angle in radians")])
  ["Tangent of an angle in radians, correctly rounded for finite arguments. tan(\xB10) = \xB10; near odd\
  \ multiples of \x03C0/2 the result has large magnitude but remains finite (no exception is raised);\
  \ tan(\xB1\x221E) is NaN (with invalid-operation exception in IEEE 754); tan(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 tan operation and to Haskell's tan :: Double -> Double."]

tanh :: PrimitiveDefinition
tanh = define "tanh" "The hyperbolic tangent of a floating-point number." (f64To [("x", "the value whose hyperbolic tangent is computed")])
  ["Hyperbolic tangent. The result is in [-1, +1]; tanh(\xB10) = \xB10; tanh(\xB1\x221E) = \xB11;\
  \ tanh(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 tanh operation and to Haskell's tanh :: Double -> Double."]

truncate :: PrimitiveDefinition
truncate = define "truncate" "Truncate a floating-point number toward zero, as a float." (f64To [("x", "the value to truncate toward zero")])
  ["Round toward zero (truncate the fractional part), returned as a float64. Equivalent to IEEE 754\
  \ \xA75.3.1 roundToIntegralTowardZero: truncate(\xB10) = \xB10; truncate(\xB1\x221E) = \xB1\x221E;\
  \ truncate(NaN) is NaN; sign of the result matches the sign of the argument (so truncate(-0.7) = -0,\
  \ truncate(+0.7) = +0).",
   "Note that the return type is float64, not an integer type, so the result can exceed the integer\
  \ range.",
   "Corresponds to Haskell's fromIntegral . truncate :: Double -> Double."]
