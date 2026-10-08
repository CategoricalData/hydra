-- | Primitive declarations for the hydra.core.lib.math namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Math where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), abs, acos, acosh, asin, asinh, atan, atan2, atanh,
                               ceiling, cos, cosh, div, even, exp, floor, log, logBase,
                               max, min, mod, negate, odd, pi, rem, round, signum, sin, sinh,
                               sqrt, tan, tanh, truncate)


ns :: ModuleName
ns = ModuleName "hydra.core.lib.math"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.core.lib.math module.")}
  where
    definitions = [abs, acos, acosh, add, addFloat64, asin, asinh, atan, atan2, atanh,
                   ceiling, cos, cosh, div, divide, e, even, exp, floor, log, logBase, mod,
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
f64Const = sig $ TypeScheme [] Types.float64 mempty

f64To :: [(String, String)] -> TermSignature
f64To params = sigWithParams params $ TypeScheme [] (Types.float64 Types.~> Types.float64) mempty

f64To2 :: [(String, String)] -> TermSignature
f64To2 params = sigWithParams params $ TypeScheme [] (Types.float64 Types.~> Types.float64 Types.~> Types.float64) mempty

int32F32ToF32 :: [(String, String)] -> TermSignature
int32F32ToF32 params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.float32 Types.~> Types.float32) mempty

int32F64ToF64 :: [(String, String)] -> TermSignature
int32F64ToF64 params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.float64 Types.~> Types.float64) mempty

int32To :: [(String, String)] -> TermSignature
int32To params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.int32) mempty

int32To2 :: [(String, String)] -> TermSignature
int32To2 params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.int32 Types.~> Types.int32) mempty

int32To2List :: [(String, String)] -> TermSignature
int32To2List params = sigWithParams params $ TypeScheme []
  (Types.int32 Types.~> Types.int32 Types.~> Types.list Types.int32) mempty

int32To2Maybe :: [(String, String)] -> TermSignature
int32To2Maybe params = sigWithParams params $ TypeScheme []
  (Types.int32 Types.~> Types.int32 Types.~> Types.optional Types.int32) mempty

int32ToBool :: [(String, String)] -> TermSignature
int32ToBool params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.boolean) mempty

int32ToMaybe :: [(String, String)] -> TermSignature
int32ToMaybe params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.optional Types.int32) mempty

fractionalTo2 :: TermSignature
fractionalTo2 = sig $ Types.polyConstrained [("x", [Name "fractional"])]
  (tx Types.~> tx Types.~> tx)

-- Constraint-polymorphic 'integral' signatures (integral x => ...).
integralTo2Maybe :: [(String, String)] -> TermSignature
integralTo2Maybe params = sigWithParams params $ Types.polyConstrained [("x", [Name "integral"])]
  (tx Types.~> tx Types.~> Types.optional tx)

integralToBool :: [(String, String)] -> TermSignature
integralToBool params = sigWithParams params $ Types.polyConstrained [("x", [Name "integral"])]
  (tx Types.~> Types.boolean)

abs :: PrimitiveDefinition
abs = withProvisions [
  requirement "preservesMagnitudeAcrossTypes"
    "abs returns the non-negative magnitude of the argument, for every numeric type with a\
    \ 'numeric' instance (signed/unsigned integers of every width, bigint, float32/float64).",
  requirement "boundaryAndSpecialValues"
    "abs at the signed-integer minimum value wraps to itself (not representable as its positive\
    \ magnitude); abs of signed zero preserves sign to +0; abs(NaN) is NaN; abs(-Infinity) is\
    \ +Infinity."]
  $ define "abs" "Numeric absolute value." numericTo
  ["Constraint-polymorphic absolute value over any type with a 'numeric' instance. The per-type\
  \ semantics live in the instance: for fixed-width signed integers the result is total but not\
  \ injective at the boundary (abs(minBound) = minBound, e.g. abs(-2147483648) = -2147483648 for\
  \ int32, because +2147483648 is not representable); for unsigned integers and bigint the result\
  \ is the input itself or its arithmetic negation, whichever is non-negative; for floating-point\
  \ it clears the sign bit per IEEE 754 \xA75.5.1 (abs(\xB10) = +0, abs(NaN) is NaN).",
   "Requires a 'numeric' type-class constraint on the argument type.",
   "Corresponds to Haskell's abs :: Num a => a -> a."]

acos :: PrimitiveDefinition
acos = withProvisions [
  requirement "inverseCosineInPrincipalRange"
    "acos returns the principal value of the inverse cosine, in radians, in [0, pi], for\
    \ arguments in its domain [-1, +1].",
  requirement "outOfDomainIsNanSpecialValuesPropagate"
    "acos of an argument outside [-1, +1] (including +-Infinity) is NaN; acos(NaN) is NaN."]
  $ define "acos" "The arc cosine of a floating-point number." (f64To [("x", "the cosine value whose arc cosine is computed")])
  ["Principal value of the inverse cosine, in radians. The result is in [0, \x03C0].",
   "For arguments outside the domain [-1, +1] (including \xB1\x221E), the result is NaN. acos(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 acos operation and to Haskell's acos :: Double -> Double."]

acosh :: PrimitiveDefinition
acosh = withProvisions [
  requirement "inverseHyperbolicCosineInNonnegativeRange"
    "acosh returns the principal value of the inverse hyperbolic cosine, in [0, +Infinity), for\
    \ arguments in its domain [1, +Infinity).",
  requirement "outOfDomainIsNanAsymmetricAtInfinity"
    "acosh of an argument less than 1 (including -Infinity) is NaN; acosh(+Infinity) is\
    \ +Infinity; acosh(NaN) is NaN."]
  $ define "acosh" "The hyperbolic arc cosine of a floating-point number." (f64To [("x", "the value whose inverse hyperbolic cosine is computed")])
  ["Principal value of the inverse hyperbolic cosine. The result is in [0, +\x221E).",
   "For arguments less than 1 the result is NaN; acosh(1) = +0; acosh(+\x221E) = +\x221E; acosh(NaN) is\
  \ NaN.",
   "Corresponds to the IEEE 754 \xA79.2 acosh operation and to Haskell's acosh :: Double -> Double."]

add :: PrimitiveDefinition
add = withProvisions [
  requirement "wrapsOrRoundsPerType"
    "add combines two values of a numeric type per that type's own arithmetic: two's-complement\
    \ wraparound for fixed-width integers, arbitrary precision for bigint, IEEE 754\
    \ round-to-nearest for floating-point -- and dispatches correctly regardless of which numeric\
    \ type is supplied at the call site.",
  requirement "boundaryWraparoundAndIeeeSpecialValues"
    "add wraps silently at every fixed-width integer type's boundary (no overflow exception);\
    \ add of two bigints beyond int64/uint64 range remains exact; add on float64/float32 follows\
    \ IEEE 754 special-value rules (opposite-sign infinities and any NaN operand yield NaN, ties\
    \ round to even, -0 + -0 = -0)."]
  $ define "add" "Numeric addition." numericTo2
  ["Constraint-polymorphic addition over any type with a 'numeric' instance. The per-type semantics\
  \ live in the instance: two's-complement wraparound for fixed-width integers (e.g. int32 addition\
  \ reduces x + y modulo 2^32 and reinterprets as signed, wrapping silently on overflow),\
  \ IEEE 754 round-to-nearest for floating-point, and arbitrary precision for bigint.",
   "Requires a 'numeric' type-class constraint on the argument type, which is the closest Hydra\
   \ equivalent to Haskell's Num instance.",
   "The operation is total. Corresponds to Haskell's (+) :: Num a => a -> a -> a."]

addFloat64 :: PrimitiveDefinition
addFloat64 = withProvisions [
  requirement "addsFloat64Values"
    "addFloat64 computes the IEEE 754 binary64 sum of its two arguments."]
  $ deprecatedSince "0.18" "hydra.core.lib.math.add" $ define "addFloat64" "Floating-point addition." (f64To2 [("x", "the first addend"), ("y", "the second addend")])
  ["IEEE 754 binary64 addition. The result is the value of x + y rounded to the nearest representable\
  \ float64 under the roundTiesToEven rounding-direction attribute.",
   "Adding infinities of opposite sign (+\x221E + -\x221E or -\x221E + +\x221E) produces a NaN; adding\
  \ any value to NaN produces a NaN. The sum of two zeros is +0, except (-0) + (-0) = -0.",
   "The operation is total: it never raises, but it may produce NaN. Corresponds to the IEEE 754\
  \ \xA75.4.1 addition operation and to Haskell's (+) :: Double -> Double -> Double."]

asin :: PrimitiveDefinition
asin = withProvisions [
  requirement "inverseSineInPrincipalRange"
    "asin returns the principal value of the inverse sine, in radians, in [-pi/2, +pi/2], for\
    \ arguments in its domain [-1, +1].",
  requirement "outOfDomainIsNanSpecialValuesPropagate"
    "asin of an argument outside [-1, +1] (including +-Infinity) is NaN; asin(NaN) is NaN."]
  $ define "asin" "The arc sine of a floating-point number." (f64To [("x", "the sine value whose arc sine is computed")])
  ["Principal value of the inverse sine, in radians. The result is in [-\x03C0/2, +\x03C0/2].",
   "For arguments outside the domain [-1, +1] (including \xB1\x221E), the result is NaN; asin(\xB10) =\
  \ \xB10; asin(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 asin operation and to Haskell's asin :: Double -> Double."]

asinh :: PrimitiveDefinition
asinh = withProvisions [
  requirement "inverseHyperbolicSineDefinedForAllFiniteReals"
    "asinh returns the principal value of the inverse hyperbolic sine, defined for every finite\
    \ real argument.",
  requirement "oddSymmetryAtInfinityNanPropagates"
    "asinh(+-Infinity) is +-Infinity respectively (odd symmetry preserved at the boundary);\
    \ asinh(NaN) is NaN."]
  $ define "asinh" "The hyperbolic arc sine of a floating-point number." (f64To [("x", "the value whose inverse hyperbolic sine is computed")])
  ["Principal value of the inverse hyperbolic sine. Defined for all finite reals; asinh(\xB10) = \xB10;\
  \ asinh(\xB1\x221E) = \xB1\x221E; asinh(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 asinh operation and to Haskell's asinh :: Double -> Double."]

atan :: PrimitiveDefinition
atan = withProvisions [
  requirement "inverseTangentInPrincipalRange"
    "atan returns the principal value of the inverse tangent, in radians, in (-pi/2, +pi/2), for\
    \ any finite argument.",
  requirement "saturatesAtInfinityNanPropagates"
    "atan of +-Infinity saturates to +-pi/2 respectively (rather than NaN); atan(NaN) is NaN."]
  $ define "atan" "The arc tangent of a floating-point number." (f64To [("x", "the tangent value whose arc tangent is computed")])
  ["Principal value of the inverse tangent, in radians. The result is in (-\x03C0/2, +\x03C0/2);\
  \ atan(\xB10) = \xB10; atan(\xB1\x221E) = \xB1\x03C0/2; atan(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 atan operation and to Haskell's atan :: Double -> Double."]

atan2 :: PrimitiveDefinition
atan2 = withProvisions [
  requirement "quadrantAwareArcTangentOfTwoArguments"
    "atan2(y, x) returns the angle in radians, in (-pi, +pi], from the positive x-axis to the\
    \ point (x, y), using the signs of both arguments to determine the quadrant.",
  requirement "ieeeSpecialValueMatrix"
    "atan2 follows the IEEE 754 9.2 special-value table for signed-zero, infinite, and NaN\
    \ arguments in either position, including the cross-infinity combinations where this\
    \ implementation returns NaN."]
  $ define "atan2" "The two-argument arc tangent (atan2)." (f64To2 [("y", "the ordinate (y-coordinate)"), ("x", "the abscissa (x-coordinate)")])
  ["atan2(y, x) returns the angle in radians, in (-\x03C0, +\x03C0], from the positive x-axis to the\
  \ point (x, y), using the signs of both arguments to determine the quadrant.",
   "Special match follow IEEE 754 \xA79.2: atan2(\xB10, +x>0) = \xB10; atan2(\xB10, -x<0) = \xB1\x03C0;\
  \ atan2(\xB1y>0, 0) = \xB1\x03C0/2; atan2(\xB1y, +\x221E) = \xB10; atan2(\xB1y, -\x221E) = \xB1\x03C0;\
  \ atan2(\xB1\x221E, finite) = \xB1\x03C0/2; atan2(\xB1\x221E, +\x221E) = \xB1\x03C0/4;\
  \ atan2(\xB1\x221E, -\x221E) = \xB13\x03C0/4; atan2 of any NaN argument is NaN.",
   "Corresponds to Haskell's atan2 :: Double -> Double -> Double."]

atanh :: PrimitiveDefinition
atanh = withProvisions [
  requirement "inverseHyperbolicTangentOnOpenInterval"
    "atanh returns the principal value of the inverse hyperbolic tangent, defined on the open\
    \ interval (-1, +1).",
  requirement "boundaryIsInfiniteOutOfDomainIsNan"
    "atanh at the boundary (+-1) is +-Infinity respectively; atanh of an argument outside\
    \ [-1, +1] (including +-Infinity) is NaN; atanh(NaN) is NaN."]
  $ define "atanh" "The hyperbolic arc tangent of a floating-point number." (f64To [("x", "the value whose inverse hyperbolic tangent is computed")])
  ["Principal value of the inverse hyperbolic tangent. Defined for arguments in (-1, +1); atanh(\xB11) =\
  \ \xB1\x221E (with division-by-zero exception in IEEE 754); arguments outside [-1, +1] produce NaN;\
  \ atanh(\xB10) = \xB10; atanh(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 atanh operation and to Haskell's atanh :: Double -> Double."]

ceiling :: PrimitiveDefinition
ceiling = withProvisions [
  requirement "roundsTowardPositiveInfinityAsFloat"
    "ceiling returns the smallest integer value not less than the argument, as a float64.",
  requirement "ieeeSpecialValuesPassThrough"
    "ceiling of +-Infinity or NaN passes the value through unchanged."]
  $ define "ceiling" "The smallest integer greater than or equal to the argument, as a float." (f64To [("x", "the value to round up toward positive infinity")])
  ["The smallest integer value not less than the argument, returned as a float64. Equivalent to IEEE 754\
  \ \xA75.3.1 roundToIntegralTowardPositive: ceiling(\xB10) = \xB10; ceiling(\xB1\x221E) = \xB1\x221E;\
  \ ceiling(NaN) is NaN; the sign of a negative result is preserved when rounding to zero (e.g.\
  \ ceiling(-0.5) = -0).",
   "Note that the return type is float64, not an integer type, so the result can exceed the integer\
  \ range.",
   "Corresponds to Haskell's fromIntegral . ceiling :: Double -> Double."]

cos :: PrimitiveDefinition
cos = withProvisions [
  requirement "cosineOfAngleInRadians"
    "cos returns the cosine of an angle in radians, correctly rounded for finite arguments, in\
    \ the range [-1, +1].",
  requirement "undefinedAtInfinityNanPropagates"
    "cos of +-Infinity is NaN; cos(NaN) is NaN."]
  $ define "cos" "The cosine of a floating-point number." (f64To [("x", "the angle in radians")])
  ["Cosine of an angle in radians, correctly rounded for finite arguments. The result is in [-1, +1];\
  \ cos(\xB10) = 1; cos(\xB1\x221E) is NaN (with invalid-operation exception in IEEE 754); cos(NaN) is\
  \ NaN.",
   "Corresponds to the IEEE 754 \xA79.2 cos operation and to Haskell's cos :: Double -> Double."]

cosh :: PrimitiveDefinition
cosh = withProvisions [
  requirement "hyperbolicCosineInRangeAtLeastOne"
    "cosh returns the hyperbolic cosine, in [1, +Infinity), for any finite real argument.",
  requirement "evenSymmetryAtInfinityNanPropagates"
    "cosh(+-Infinity) is +Infinity (even symmetry -- both signs saturate to the same value);\
    \ cosh(NaN) is NaN."]
  $ define "cosh" "The hyperbolic cosine of a floating-point number." (f64To [("x", "the value whose hyperbolic cosine is computed")])
  ["Hyperbolic cosine. The result is in [1, +\x221E]; cosh(\xB10) = 1; cosh(\xB1\x221E) = +\x221E;\
  \ cosh(NaN) is NaN. Large-magnitude arguments overflow to +\x221E.",
   "Corresponds to the IEEE 754 \xA79.2 cosh operation and to Haskell's cosh :: Double -> Double."]

divide :: PrimitiveDefinition
divide = withProvisions [
  requirement "roundsToNearestPerIeee754"
    "divide computes the IEEE 754 correctly-rounded quotient of its two float32/float64\
    \ arguments for finite, non-zero-divisor cases.",
  requirement "ieeeSpecialValuesAndSignedZeroDivision"
    "divide is total via IEEE 754 sentinels rather than a partial/optional result: dividing a\
    \ finite non-zero value by signed zero yields signed infinity (sign = XOR of operand signs);\
    \ zero divided by zero and infinity divided by infinity are NaN; infinity divided by a\
    \ finite value is infinity (sign XOR); a finite value divided by infinity is signed zero;\
    \ NaN propagates unconditionally."]
  $ define "divide" "Floating-point division." fractionalTo2
  ["Constraint-polymorphic division over any type with a 'fractional' instance (float32,\
  \ float64). IEEE 754 division: the result is x / y correctly rounded (roundTiesToEven) for\
  \ finite y \x2260 0.",
   "The operation is total via IEEE sentinels rather than an optional codomain: for finite\
  \ nonzero x, x / \xB10 = \xB1\x221E (sign = XOR of the operand signs); \xB10 / \xB10 and\
  \ \xB1\x221E / \xB1\x221E are NaN; \xB1\x221E / finite = \xB1\x221E (sign XOR); finite /\
  \ \xB1\x221E = \xB10 (sign XOR); NaN propagates unconditionally.",
   "Requires a 'fractional' type-class constraint on the argument type.",
   "Corresponds to Haskell's (/) :: Fractional a => a -> a -> a."]

e :: PrimitiveDefinition
e = withProvisions [
  requirement "isEulersConstant"
    "e is the float64 value nearest to Euler's number, equal to exp(1)."]
  $ define "e" "Euler's constant (the base of the natural logarithm)." f64Const
  ["The mathematical constant e \x2248 2.718281828459045, the base of the natural logarithm, as the\
  \ nearest representable float64. Equal to exp(1).",
   "Corresponds to Haskell's exp 1 :: Double."]

even :: PrimitiveDefinition
even = withProvisions [
  requirement "testsDivisibilityByTwo"
    "even returns true exactly when the argument is divisible by 2, for every integral type,\
    \ including at the signed-integer minimum value and for negative values (parity is\
    \ preserved under two's-complement wraparound)."]
  $ define "even" "Test whether a value is even." (integralToBool [("x", "the value to test for evenness")])
  ["Constraint-polymorphic parity test over any type with an 'integral' instance: true if the\
  \ argument is divisible by 2 (i.e. x mod 2 = 0), false otherwise.",
   "Total on all integral inputs including negative numbers and signed minBound (parity is\
  \ preserved by two's-complement wraparound).",
   "Requires an 'integral' type-class constraint on the argument type.",
   "Corresponds to Haskell's even :: Integral a => a -> Bool."]

exp :: PrimitiveDefinition
exp = withProvisions [
  requirement "exponentialFunctionBaseE"
    "exp(x) returns e^x, correctly rounded, for finite x.",
  requirement "saturatesAtInfinityNanPropagates"
    "exp(-Infinity) underflows to +0; exp(+Infinity) is +Infinity; exp(NaN) is NaN."]
  $ define "exp" "The exponential function." (f64To [("x", "the exponent")])
  ["The exponential function: exp(x) = e^x. exp(\xB10) = 1; exp(-\x221E) = +0; exp(+\x221E) = +\x221E;\
  \ exp(NaN) is NaN. Large positive arguments overflow to +\x221E; large negative arguments underflow\
  \ to +0.",
   "Corresponds to the IEEE 754 \xA79.2 exp operation and to Haskell's exp :: Double -> Double."]

floor :: PrimitiveDefinition
floor = withProvisions [
  requirement "roundsTowardNegativeInfinityAsFloat"
    "floor returns the largest integer value not greater than the argument, as a float64.",
  requirement "ieeeSpecialValuesPassThrough"
    "floor of +-Infinity or NaN passes the value through unchanged."]
  $ define "floor" "The largest integer less than or equal to the argument, as a float." (f64To [("x", "the value to round down toward negative infinity")])
  ["The largest integer value not greater than the argument, returned as a float64. Equivalent to IEEE\
  \ 754 \xA75.3.1 roundToIntegralTowardNegative: floor(\xB10) = \xB10; floor(\xB1\x221E) = \xB1\x221E;\
  \ floor(NaN) is NaN.",
   "Note that the return type is float64, not an integer type, so the result can exceed the integer\
  \ range.",
   "Corresponds to Haskell's fromIntegral . floor :: Double -> Double."]

log :: PrimitiveDefinition
log = withProvisions [
  requirement "naturalLogarithmOnPositiveDomain"
    "log returns the natural (base-e) logarithm, correctly rounded, for positive finite\
    \ arguments.",
  requirement "boundaryIsNegativeInfinityNegativeIsNan"
    "log of zero (either sign) is -Infinity; log of a negative argument is NaN; log(+Infinity)\
    \ is +Infinity; log(NaN) is NaN."]
  $ define "log" "The natural logarithm." (f64To [("x", "the value whose natural logarithm is computed")])
  ["The natural (base-e) logarithm. log(1) = +0; log(+0) = log(-0) = -\x221E (with division-by-zero\
  \ exception in IEEE 754); log(x) for x < 0 is NaN (with invalid-operation exception); log(+\x221E) =\
  \ +\x221E; log(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 log operation and to Haskell's log :: Double -> Double."]

logBase :: PrimitiveDefinition
logBase = withProvisions [
  requirement "logarithmInArbitraryBase"
    "logBase(b, x) computes the logarithm of x in base b, equivalent to log(x) / log(b), for\
    \ positive finite b and x.",
  requirement "inheritsLogsSpecialCaseBehaviorInEachArgument"
    "logBase inherits the special-case behavior of log independently in each argument\
    \ position: a zero or negative x yields -Infinity/NaN respectively (per log's own rule), a\
    \ non-positive base yields NaN, and NaN/+-Infinity in either argument propagate per log's\
    \ own special-value rules."]
  $ define "logBase" "Logarithm of the second argument in the base of the first." (f64To2 [("b", "the logarithm base"), ("x", "the value whose logarithm is computed")])
  ["logBase(b, x) computes the logarithm of x in base b, equivalent to log(x) / log(b).",
   "Inherits the special-case behavior of log for each argument (NaN propagation, sign of zeros,\
  \ division by zero on log of a zero, NaN on negative arguments).",
   "Corresponds to Haskell's logBase :: Double -> Double -> Double."]

div :: PrimitiveDefinition
div = withProvisions [
  requirement "flooredQuotientWhenDivisorNonzero"
    "div returns the integer quotient of its two arguments rounded toward negative infinity\
    \ (floor division), wrapped, for every integral type, when the divisor is non-zero.",
  requirement "noneOnZeroDivisorWraparoundAtMinBound"
    "div is none when the divisor is zero, for every integral type; at a fixed-width signed\
    \ integer type's boundary case (minimum value divided by -1), the result wraps to the\
    \ minimum value rather than overflowing."]
  $ define "div" "Integral division, or Nothing if dividing by zero." (integralTo2Maybe [("x", "the dividend"), ("y", "the divisor")])
  ["Constraint-polymorphic total integer division over any type with an 'integral' instance:\
  \ div(x, y) returns Just(x divided by y, rounded toward negative infinity) when y is non-zero,\
  \ and Nothing when y = 0.",
   "The division rounds toward negative infinity (floor), so for example div(-7, 2) = Just(-4).",
   "For fixed-width signed integers, the boundary case div(minBound, -1) wraps to minBound (the\
  \ two's-complement overflow), since the mathematical result is not representable at that width.",
   "Requires an 'integral' type-class constraint on the argument type.",
   "Corresponds to Haskell's div :: Integral a => a -> a -> a, wrapped in maybe to make the\
  \ zero-divisor case total."]

mod :: PrimitiveDefinition
mod = withProvisions [
  requirement "flooredRemainderSignFollowsDivisor"
    "mod returns the integer remainder of floored division, wrapped, whose sign matches the\
    \ sign of the divisor (Knuth-style floor division), for every integral type, when the\
    \ divisor is non-zero.",
  requirement "noneOnZeroDivisor"
    "mod is none when the divisor is zero, for every integral type; at a fixed-width signed\
    \ integer type's boundary case (minimum value divided by -1), the result is zero (no\
    \ overflow -- the quotient's overflow is absorbed by div, not mod)."]
  $ define "mod" "Integral modulus, or Nothing if dividing by zero." (integralTo2Maybe [("x", "the dividend"), ("y", "the divisor")])
  ["Constraint-polymorphic total integer modulus over any type with an 'integral' instance:\
  \ mod(x, y) returns Just(x mod y) when y is non-zero, and Nothing when y = 0.",
   "The result satisfies the identity x = (div(x, y) result) * y + (mod(x, y) result), so the\
  \ sign of the result matches the sign of y (Knuth-style floor division). For example mod(-7, 2)\
  \ = Just(1).",
   "Requires an 'integral' type-class constraint on the argument type.",
   "Corresponds to Haskell's mod :: Integral a => a -> a -> a, wrapped in maybe to make the\
  \ zero-divisor case total."]

rem :: PrimitiveDefinition
rem = withProvisions [
  requirement "truncatedRemainderSignFollowsDividend"
    "rem returns the integer remainder of truncated division (toward zero), wrapped, whose\
    \ sign matches the sign of the dividend (C-style remainder), for every integral type, when\
    \ the divisor is non-zero.",
  requirement "noneOnZeroDivisorNoOverflowAtMinBound"
    "rem is none when the divisor is zero, for every integral type; at a fixed-width signed\
    \ integer type's boundary case (minimum value divided by -1), the result is zero (no\
    \ overflow -- the quotient's overflow is absorbed by div, not rem)."]
  $ define "rem" "Integral remainder, or Nothing if dividing by zero." (integralTo2Maybe [("x", "the dividend"), ("y", "the divisor")])
  ["Constraint-polymorphic total integer remainder over any type with an 'integral' instance:\
  \ rem(x, y) returns Just(x rem y) when y is non-zero, and Nothing when y = 0.",
   "The result satisfies x = (truncate(x / y)) * y + (rem(x, y) result), so the sign of the result\
  \ matches the sign of x (truncated division, C-style remainder). For example rem(-7, 2) =\
  \ Just(-1).",
   "For fixed-width signed integers, the boundary case rem(minBound, -1) is 0 (no overflow, since\
  \ the quotient overflow is absorbed by div, not rem).",
   "Requires an 'integral' type-class constraint on the argument type.",
   "Corresponds to Haskell's rem :: Integral a => a -> a -> a, wrapped in maybe to make the\
  \ zero-divisor case total."]

mul :: PrimitiveDefinition
mul = withProvisions [
  requirement "wrapsOrRoundsPerType"
    "mul computes the product of two values of a numeric type per that type's own arithmetic:\
    \ two's-complement wraparound for fixed-width integers, arbitrary precision for bigint, IEEE\
    \ 754 round-to-nearest for floating-point -- and dispatches correctly regardless of which\
    \ numeric type is supplied at the call site.",
  requirement "boundaryWraparoundAndIeeeSpecialValues"
    "mul wraps silently at every fixed-width integer type's boundary; mul on bigint beyond\
    \ int64/uint64 range remains exact; mul on float64/float32 follows IEEE 754 special-value\
    \ rules (zero times infinity is NaN, any NaN operand yields NaN, sign of the result is the\
    \ XOR of operand signs, results underflow to signed zero or overflow to signed infinity when\
    \ out of representable range)."]
  $ define "mul" "Numeric multiplication." numericTo2
  ["Constraint-polymorphic multiplication over any type with a 'numeric' instance. The per-type\
  \ semantics live in the instance: two's-complement wraparound for fixed-width integers (e.g. int32\
  \ multiplication reduces x * y modulo 2^32 and reinterprets as signed, wrapping silently on\
  \ overflow), IEEE 754 round-to-nearest for floating-point, and arbitrary precision for bigint.",
   "Requires a 'numeric' type-class constraint on the argument type.",
   "The operation is total. Corresponds to Haskell's (*) :: Num a => a -> a -> a."]

mulFloat64 :: PrimitiveDefinition
mulFloat64 = withProvisions [
  requirement "multipliesFloat64Values"
    "mulFloat64 computes the IEEE 754 binary64 product of its two arguments."]
  $ deprecatedSince "0.18" "hydra.core.lib.math.mul" $ define "mulFloat64" "Floating-point multiplication." (f64To2 [("x", "the first factor"), ("y", "the second factor")])
  ["IEEE 754 binary64 multiplication. The result is the value of x * y rounded to the nearest\
  \ representable float64 under the roundTiesToEven rounding-direction attribute.",
   "Multiplying 0 by \xB1\x221E (in either order) produces a NaN (with invalid-operation exception in\
  \ IEEE 754); multiplying any value by NaN produces a NaN. The result's sign is the XOR of the operand\
  \ signs (\xB10 * \xB10 = \xB10 accordingly).",
   "The operation is total: it never raises, but it may produce NaN or \xB1\x221E. Corresponds to the\
  \ IEEE 754 \xA75.4.1 multiplication operation and to Haskell's (*) :: Double -> Double -> Double."]

negate :: PrimitiveDefinition
negate = withProvisions [
  requirement "arithmeticNegationPerType"
    "negate returns the arithmetic inverse of its argument per the argument's numeric type:\
    \ two's-complement negation for fixed-width integers, sign-bit flip for floating-point,\
    \ arbitrary precision for bigint -- and dispatches correctly regardless of which numeric\
    \ type is supplied at the call site.",
  requirement "boundaryAndIeeeSpecialValues"
    "negate at a signed fixed-width integer type's minimum value wraps to itself (not\
    \ representable as its positive magnitude); negate on bigint beyond int64 range remains\
    \ exact; negate on float64/float32 flips the sign of zero and of infinity, and preserves\
    \ NaN."]
  $ define "negate" "Numeric negation." numericTo
  ["Constraint-polymorphic arithmetic negation over any type with a 'numeric' instance. The per-type\
  \ semantics live in the instance: for fixed-width integers the result is 0 - x reduced modulo 2^32\
  \ and reinterpreted as signed (total but not injective at the boundary: negate(minBound) = minBound,\
  \ because +2147483648 is not representable in int32); for floating-point it flips the sign bit per\
  \ IEEE 754 \xA75.5.1.",
   "Requires a 'numeric' type-class constraint on the argument type.",
   "Corresponds to Haskell's negate :: Num a => a -> a."]

negateFloat64 :: PrimitiveDefinition
negateFloat64 = withProvisions [
  requirement "negatesFloat64Values"
    "negateFloat64 flips the sign of its float64 argument, including zero."]
  $ deprecatedSince "0.18" "hydra.core.lib.math.negate" $ define "negateFloat64" "Negate a floating-point number." (f64To [("x", "the value to negate")])
  ["Sign reversal of a float64. Equivalent to IEEE 754 \xA75.5.1 negate: flips the sign bit, so\
  \ negate(\xB10) = \xB10 (sign flips), negate(\xB1\x221E) = \xB1\x221E (sign flips), and negate(NaN) is\
  \ a NaN (sign may flip; payload preserved).",
   "This is a bit-level operation that does not raise any floating-point exception. Corresponds to\
  \ Haskell's negate :: Double -> Double."]

odd :: PrimitiveDefinition
odd = withProvisions [
  requirement "testsNonDivisibilityByTwo"
    "odd returns true exactly when the argument is not divisible by 2, for every integral\
    \ type, including at the signed-integer minimum value and for negative values."]
  $ defineWithDefault "odd" "Test whether a value is odd." (integralToBool [("x", "the value to test for oddness")])
  ["Constraint-polymorphic parity test over any type with an 'integral' instance: true if the\
  \ argument is not divisible by 2 (i.e. x mod 2 \x2260 0), false otherwise.",
   "Total on all integral inputs including negative numbers and signed minBound. Requires an\
  \ 'integral' type-class constraint on the argument type.",
   "Corresponds to Haskell's odd :: Integral a => a -> Bool."]
  ("x" ~> Logic.not (Math.even (var "x")))

pi :: PrimitiveDefinition
pi = withProvisions [
  requirement "isPiConstant"
    "pi is the float64 value nearest to the mathematical constant pi."]
  $ define "pi" "The mathematical constant pi." f64Const
  ["The mathematical constant \x03C0 \x2248 3.141592653589793, the ratio of a circle's circumference to\
  \ its diameter, as the nearest representable float64.",
   "Corresponds to Haskell's pi :: Double."]

pow :: PrimitiveDefinition
pow = withProvisions [
  requirement "exponentiationForFiniteArguments"
    "pow(x, y) = x^y, correctly rounded, for finite x and y within pow's defined domain.",
  requirement "ieeeSpecialValueMatrix"
    "pow follows the IEEE 754 9.2 special-value table: 0^0 = 1; 0 raised to a negative exponent\
    \ is +Infinity; a negative base raised to a non-integer exponent is NaN; 1 raised to any\
    \ exponent (including NaN) is 1; any base raised to an exponent of 0 (including NaN) is 1;\
    \ and NaN/+-Infinity in either argument otherwise follow the documented limit behavior."]
  $ define "pow" "Raise the first argument to the power of the second." (f64To2 [("x", "the base"), ("y", "the exponent")])
  ["pow(x, y) = x^y.",
   "Follows the IEEE 754 \xA79.2 pow operation: pow(\xB10, y) for y < 0 is \xB1\x221E (with\
  \ division-by-zero exception); pow(\xB10, y) for y > 0 is \xB10 if y is an odd integer, else +0;\
  \ pow(1, y) = 1 for any y including NaN; pow(x, \xB10) = 1 for any x including NaN; pow(x, y) for\
  \ negative x and non-integer y is NaN (with invalid-operation exception); pow(\xB1\x221E, y) follows\
  \ the limits in the usual way. Otherwise the result is x^y rounded to the nearest representable\
  \ float64.",
   "Corresponds to Haskell's (**) :: Double -> Double -> Double."]

range :: PrimitiveDefinition
range = withProvisions [
  requirement "generatesHalfOpenAscendingSequence"
    "range(a, b) returns the list [a, a+1, ..., b-1] -- inclusive of the lower bound and\
    \ exclusive of the upper bound, with length max(0, b-a).",
  requirement "emptyWhenBoundsEqualOrDescending"
    "range(a, b) is the empty list whenever a >= b, including equal bounds and descending\
    \ bounds -- the range never counts downward."]
  $ define "range" "Construct the half-open integer range from the first argument up to (but excluding) the second." (int32To2List [("a", "the inclusive lower bound of the range"), ("b", "the exclusive upper bound of the range")])
  ["range(a, b) returns the list [a, a+1, ..., b-1]. The range is inclusive of the lower bound and\
  \ exclusive of the upper bound; if a >= b the result is the empty list (i.e. the range does not\
  \ count downward, and an empty range is the natural result of equal bounds). The length of the\
  \ result is max(0, b - a).",
   "Does not directly correspond to Haskell's enumFromTo :: Int32 -> Int32 -> [Int32] (which is\
  \ inclusive of both bounds, i.e. [a..b]); range(a, b) is equivalent to [a .. b-1]. This matches the\
  \ half-open convention of Python's range, Rust's a..b, Go, and Java's IntStream.range."]

round :: PrimitiveDefinition
round = withProvisions [
  requirement "roundsToNearestTiesToEvenAsFloat"
    "round returns the nearest integer value as a float64, with ties rounded to the nearest\
    \ even integer (banker's rounding).",
  requirement "ieeeSpecialValuesPassThrough"
    "round of +-Infinity or NaN passes the value through unchanged."]
  $ define "round" "Round a floating-point number to the nearest integer-valued float." (f64To [("x", "the value to round to the nearest integer")])
  ["Round to the nearest integer value, returned as a float64, with ties rounded to the nearest even\
  \ integer (banker's rounding). Equivalent to IEEE 754 \xA75.3.1 roundToIntegralTiesToEven:\
  \ round(\xB10) = \xB10; round(\xB1\x221E) = \xB1\x221E; round(NaN) is NaN.",
   "Note that the return type is float64, not an integer type, so the result can exceed the integer\
  \ range.",
   "Corresponds to Haskell's fromIntegral . round :: Double -> Double."]

roundFloat32 :: PrimitiveDefinition
roundFloat32 = withProvisions [
  requirement "roundsToGivenDecimalPlacesHalfToEven"
    "roundFloat32(n, x) rounds the float32 value x to n decimal places using\
    \ round-half-to-even, returning the nearest representable float32.",
  requirement "specialValuesPassThroughUnrounded"
    "roundFloat32 passes NaN and +-Infinity through unrounded, regardless of the requested\
    \ decimal-place count."]
  $ define "roundFloat32" "Round a float32 to the given number of decimal places." (int32F32ToF32 [("n", "the number of decimal places to round to"), ("x", "the float32 value to round")])
  ["roundFloat32(n, x) rounds the float32 value x to n decimal places using round-half-to-even. The\
  \ result is the nearest float32 representation of x rounded to that decimal precision; if the exact\
  \ decimal-rounded value is not representable in float32 (the usual case), the closest float32 is\
  \ returned.",
   "Special values pass through: roundFloat32(n, \xB10) = \xB10; roundFloat32(n, \xB1\x221E) =\
  \ \xB1\x221E; roundFloat32(n, NaN) is NaN.",
   "Negative n is supported in principle (rounding to powers of ten above 1); host implementations may\
  \ differ on out-of-range n."]

roundFloat64 :: PrimitiveDefinition
roundFloat64 = withProvisions [
  requirement "roundsToGivenDecimalPlacesHalfToEven"
    "roundFloat64(n, x) rounds the float64 value x to n decimal places using\
    \ round-half-to-even, returning the nearest representable float64.",
  requirement "specialValuesPassThroughUnrounded"
    "roundFloat64 passes NaN and +-Infinity through unrounded, regardless of the requested\
    \ decimal-place count."]
  $ define "roundFloat64" "Round a float64 to the given number of decimal places." (int32F64ToF64 [("n", "the number of decimal places to round to"), ("x", "the float64 value to round")])
  ["roundFloat64(n, x) rounds the float64 value x to n decimal places using round-half-to-even. The\
  \ result is the nearest float64 representation of x rounded to that decimal precision; if the exact\
  \ decimal-rounded value is not representable in float64 (the usual case), the closest float64 is\
  \ returned.",
   "Special values pass through: roundFloat64(n, \xB10) = \xB10; roundFloat64(n, \xB1\x221E) =\
  \ \xB1\x221E; roundFloat64(n, NaN) is NaN.",
   "Negative n is supported in principle (rounding to powers of ten above 1); host implementations may\
  \ differ on out-of-range n."]

signum :: PrimitiveDefinition
signum = withProvisions [
  requirement "signPerType"
    "signum returns the sign of its argument per the argument's numeric type: -1/0/1 (same\
    \ width) for integers, +-1.0 for nonzero finite or infinite floats -- satisfying\
    \ abs(x) * signum(x) = x for every integer instance except signed minBound.",
  requirement "preservesSignedZeroAndNan"
    "signum of signed zero returns that same signed zero (not collapsed to a single zero), and\
    \ signum(NaN) is NaN -- a naive positive/negative/else-zero branch implementation gets both\
    \ of these cases wrong and must not be used."]
  $ define "signum" "Numeric sign." numericTo
  ["Constraint-polymorphic sign function over any type with a 'numeric' instance. For integer\
  \ types, signum(x) returns -1 if x < 0, 0 if x = 0, and 1 if x > 0 (same width as the argument);\
  \ unsigned integers return only 0 or 1. The function satisfies abs(x) * signum(x) = x for all\
  \ integer instances except signed minBound (where the identity fails because +maxBound+1 is not\
  \ representable).",
   "For floating-point, signum(x) returns \xB11.0 for nonzero finite or infinite x (per IEEE 754\
  \ \xA75.5.1); signum(\xB10) = \xB10 (the sign of the input zero is preserved, not collapsed to a\
  \ single zero); signum(NaN) is NaN. A naive three-branch implementation (positive/negative/else-\
  \ zero) gets both of these float match wrong and must not be used.",
   "Requires a 'numeric' type-class constraint on the argument type.",
   "Corresponds to Haskell's signum :: Num a => a -> a."]

sin :: PrimitiveDefinition
sin = withProvisions [
  requirement "sineOfAngleInRadians"
    "sin returns the sine of an angle in radians, correctly rounded for finite arguments, in\
    \ the range [-1, +1].",
  requirement "undefinedAtInfinityNanPropagates"
    "sin of +-Infinity is NaN (not a finite oscillation); sin(NaN) is NaN."]
  $ define "sin" "The sine of a floating-point number." (f64To [("x", "the angle in radians")])
  ["Sine of an angle in radians, correctly rounded for finite arguments. The result is in [-1, +1];\
  \ sin(\xB10) = \xB10; sin(\xB1\x221E) is NaN (with invalid-operation exception in IEEE 754); sin(NaN)\
  \ is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 sin operation and to Haskell's sin :: Double -> Double."]

sinh :: PrimitiveDefinition
sinh = withProvisions [
  requirement "hyperbolicSineDefinedForAllFiniteReals"
    "sinh returns the hyperbolic sine, defined for every finite real argument.",
  requirement "oddSymmetryAtInfinityNanPropagates"
    "sinh(+-Infinity) is +-Infinity respectively; sinh(NaN) is NaN."]
  $ define "sinh" "The hyperbolic sine of a floating-point number." (f64To [("x", "the value whose hyperbolic sine is computed")])
  ["Hyperbolic sine. sinh(\xB10) = \xB10; sinh(\xB1\x221E) = \xB1\x221E; sinh(NaN) is NaN.\
  \ Large-magnitude arguments overflow to \xB1\x221E.",
   "Corresponds to the IEEE 754 \xA79.2 sinh operation and to Haskell's sinh :: Double -> Double."]

sqrt :: PrimitiveDefinition
sqrt = withProvisions [
  requirement "nonnegativeSquareRootOnNonnegativeDomain"
    "sqrt returns the non-negative, correctly-rounded square root for non-negative finite\
    \ arguments.",
  requirement "preservesSignOfZeroNegativeIsNan"
    "sqrt(-0) is -0 (sign preserved, distinct from sqrt(+0) = +0); sqrt of a negative argument\
    \ (including -Infinity) is NaN; sqrt(+Infinity) is +Infinity; sqrt(NaN) is NaN."]
  $ define "sqrt" "The non-negative square root of a floating-point number." (f64To [("x", "the value whose square root is computed")])
  ["IEEE 754 binary64 square root. The result is the value of \x221A\&x correctly rounded to the nearest\
  \ representable float64 under roundTiesToEven.",
   "sqrt(+0) = +0; sqrt(-0) = -0 (sign preserved); sqrt(x) for x < 0 (including -\x221E) is NaN (with\
  \ invalid-operation exception); sqrt(+\x221E) = +\x221E; sqrt(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA75.4.1 squareRoot operation and to Haskell's sqrt :: Double ->\
  \ Double."]

sub :: PrimitiveDefinition
sub = withProvisions [
  requirement "wrapsOrRoundsPerType"
    "sub computes the difference of two values of a numeric type per that type's own\
    \ arithmetic: two's-complement wraparound for fixed-width integers, arbitrary precision for\
    \ bigint, IEEE 754 round-to-nearest for floating-point -- and dispatches correctly\
    \ regardless of which numeric type is supplied at the call site.",
  requirement "boundaryWraparoundAndIeeeSpecialValues"
    "sub wraps silently at every fixed-width integer type's boundary; sub on bigint beyond\
    \ int64/uint64 range remains exact; sub on float64/float32 follows IEEE 754 special-value\
    \ rules (subtracting same-sign infinities or any NaN operand yields NaN, the difference of\
    \ two equal finite values is +0, ties round to even)."]
  $ define "sub" "Numeric subtraction." numericTo2
  ["Constraint-polymorphic subtraction over any type with a 'numeric' instance. The per-type semantics\
  \ live in the instance: two's-complement wraparound for fixed-width integers (e.g. int32 subtraction\
  \ reduces x - y modulo 2^32 and reinterprets as signed, wrapping silently on overflow), IEEE 754\
  \ round-to-nearest for floating-point, and arbitrary precision for bigint.",
   "Requires a 'numeric' type-class constraint on the argument type.",
   "The operation is total. Corresponds to Haskell's (-) :: Num a => a -> a -> a."]

subFloat64 :: PrimitiveDefinition
subFloat64 = withProvisions [
  requirement "subtractsFloat64Values"
    "subFloat64 computes the IEEE 754 binary64 difference of its two arguments."]
  $ deprecatedSince "0.18" "hydra.core.lib.math.sub" $ define "subFloat64" "Floating-point subtraction." (f64To2 [("x", "the minuend"), ("y", "the subtrahend")])
  ["IEEE 754 binary64 subtraction, defined as x + (-y). The result is correctly rounded to the nearest\
  \ representable float64 under roundTiesToEven.",
   "Subtracting infinities of the same sign (+\x221E - +\x221E or -\x221E - -\x221E) produces a NaN;\
  \ subtracting any value involving NaN produces a NaN. The difference of two equal finite values is +0\
  \ (or -0 under round-toward-negative, which is not the default).",
   "The operation is total: it never raises, but it may produce NaN. Corresponds to the IEEE 754\
  \ \xA75.4.1 subtraction operation and to Haskell's (-) :: Double -> Double -> Double."]

tan :: PrimitiveDefinition
tan = withProvisions [
  requirement "tangentOfAngleInRadians"
    "tan returns the tangent of an angle in radians, correctly rounded for finite arguments;\
    \ near odd multiples of pi/2 the result has large magnitude but remains finite (no\
    \ exception is raised).",
  requirement "undefinedAtInfinityNanPropagates"
    "tan of +-Infinity is NaN; tan(NaN) is NaN."]
  $ define "tan" "The tangent of a floating-point number." (f64To [("x", "the angle in radians")])
  ["Tangent of an angle in radians, correctly rounded for finite arguments. tan(\xB10) = \xB10; near odd\
  \ multiples of \x03C0/2 the result has large magnitude but remains finite (no exception is raised);\
  \ tan(\xB1\x221E) is NaN (with invalid-operation exception in IEEE 754); tan(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 tan operation and to Haskell's tan :: Double -> Double."]

tanh :: PrimitiveDefinition
tanh = withProvisions [
  requirement "hyperbolicTangentInRangeNegOneToOne"
    "tanh returns the hyperbolic tangent, in (-1, +1), for any finite real argument.",
  requirement "saturatesAtInfinityNanPropagates"
    "tanh(+-Infinity) saturates to +-1 respectively; tanh(NaN) is NaN."]
  $ define "tanh" "The hyperbolic tangent of a floating-point number." (f64To [("x", "the value whose hyperbolic tangent is computed")])
  ["Hyperbolic tangent. The result is in [-1, +1]; tanh(\xB10) = \xB10; tanh(\xB1\x221E) = \xB11;\
  \ tanh(NaN) is NaN.",
   "Corresponds to the IEEE 754 \xA79.2 tanh operation and to Haskell's tanh :: Double -> Double."]

truncate :: PrimitiveDefinition
truncate = withProvisions [
  requirement "roundsTowardZeroAsFloat"
    "truncate returns the argument with its fractional part discarded (rounded toward zero),\
    \ as a float64, preserving the sign of the argument.",
  requirement "ieeeSpecialValuesPassThrough"
    "truncate of +-Infinity or NaN passes the value through unchanged."]
  $ define "truncate" "Truncate a floating-point number toward zero, as a float." (f64To [("x", "the value to truncate toward zero")])
  ["Round toward zero (truncate the fractional part), returned as a float64. Equivalent to IEEE 754\
  \ \xA75.3.1 roundToIntegralTowardZero: truncate(\xB10) = \xB10; truncate(\xB1\x221E) = \xB1\x221E;\
  \ truncate(NaN) is NaN; sign of the result matches the sign of the argument (so truncate(-0.7) = -0,\
  \ truncate(+0.7) = +0).",
   "Note that the return type is float64, not an integer type, so the result can exceed the integer\
  \ range.",
   "Corresponds to Haskell's fromIntegral . truncate :: Double -> Double."]
