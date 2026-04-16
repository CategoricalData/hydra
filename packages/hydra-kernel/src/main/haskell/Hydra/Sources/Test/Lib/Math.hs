module Hydra.Sources.Test.Lib.Math where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Lib.Math as Math


optionalInt32 :: Maybe Int -> TTerm Term
optionalInt32 Nothing = Core.termMaybe nothing
optionalInt32 (Just x) = Core.termMaybe $ just (int32 x)

ns :: Namespace
ns = Namespace "hydra.test.lib.math"

module_ :: Module
module_ = Module ns definitions [Namespace "hydra.reduction", Namespace "hydra.show.core"] [] $
    Just "Test cases for hydra.lib.math primitives"
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.math primitives

mathAbs :: TTerm TestGroup
mathAbs = subgroup "abs" [
  test "positive" 5 5,
  test "negative" (-5) 5,
  test "zero" 0 0]
  where
    test name x result = primCase name _math_abs [int32 x] (int32 result)

mathAdd :: TTerm TestGroup
mathAdd = subgroup "add" [
  test "positive numbers" 3 5 8,
  test "negative numbers" (-3) (-5) (-8),
  test "mixed sign" 10 (-3) 7,
  test "with zero" 42 0 42]
  where
    test name x y result = primCase name _math_add [int32 x, int32 y] (int32 result)

mathEven :: TTerm TestGroup
mathEven = subgroup "even" [
  test "even positive" 4 true,
  test "odd positive" 5 false,
  test "even negative" (-4) true,
  test "odd negative" (-5) false,
  test "zero" 0 true]
  where
    test name x result = primCase name _math_even [int32 x] result

mathMax :: TTerm TestGroup
mathMax = subgroup "max" [
  test "first is larger" 10 5 10,
  test "second is larger" 5 10 10,
  test "equal values" 7 7 7,
  test "negative numbers" (-3) (-5) (-3),
  test "mixed sign" (-5) 5 5,
  test "with zero" 0 42 42]
  where
    test name x y result = primCase name _math_max [int32 x, int32 y] (int32 result)

mathMin :: TTerm TestGroup
mathMaybeDiv :: TTerm TestGroup
mathMaybeDiv = subgroup "maybeDiv" [
  test "basic division" 10 3 (Just 3),
  test "exact division" 10 2 (Just 5),
  test "division by zero" 10 0 Nothing,
  test "zero divided" 0 5 (Just 0),
  test "negative dividend" (-10) 3 (Just (-4)),
  test "negative divisor" 10 (-3) (Just (-4))]
  where
    test name x y result = primCase name _math_maybeDiv [int32 x, int32 y] (optionalInt32 result)

mathMin = subgroup "min" [
  test "first is smaller" 5 10 5,
  test "second is smaller" 10 5 5,
  test "equal values" 7 7 7,
  test "negative numbers" (-3) (-5) (-5),
  test "mixed sign" (-5) 5 (-5),
  test "with zero" 0 42 0]
  where
    test name x y result = primCase name _math_min [int32 x, int32 y] (int32 result)

mathMaybeMod :: TTerm TestGroup
mathMaybeMod = subgroup "maybeMod" [
  test "basic modulo" 10 3 (Just 1),
  test "exact division" 10 2 (Just 0),
  test "division by zero" 10 0 Nothing,
  test "negative dividend" (-10) 3 (Just 2),
  test "negative divisor" 10 (-3) (Just (-2))]
  where
    test name x y result = primCase name _math_maybeMod [int32 x, int32 y] (optionalInt32 result)

mathMul :: TTerm TestGroup
mathMul = subgroup "mul" [
  test "positive numbers" 3 5 15,
  test "negative numbers" (-3) (-5) 15,
  test "mixed sign" 3 (-5) (-15),
  test "with zero" 42 0 0,
  test "with one" 42 1 42]
  where
    test name x y result = primCase name _math_mul [int32 x, int32 y] (int32 result)

mathNegate :: TTerm TestGroup
mathNegate = subgroup "negate" [
  test "positive" 5 (-5),
  test "negative" (-5) 5,
  test "zero" 0 0]
  where
    test name x result = primCase name _math_negate [int32 x] (int32 result)

mathOdd :: TTerm TestGroup
mathOdd = subgroup "odd" [
  test "odd positive" 5 true,
  test "even positive" 4 false,
  test "odd negative" (-5) true,
  test "even negative" (-4) false,
  test "zero" 0 false]
  where
    test name x result = primCase name _math_odd [int32 x] result

mathMaybePred :: TTerm TestGroup
mathMaybePred = subgroup "maybePred" [
  test "positive" 5 (Just 4),
  test "zero" 0 (Just (-1)),
  test "negative" (-5) (Just (-6)),
  test "minBound" (-2147483648) Nothing]
  where
    test name x result = primCase name _math_maybePred [int32 x] (optionalInt32 result)

mathRange :: TTerm TestGroup
mathRange = subgroup "range" [
  test "ascending range" 1 5 [1, 2, 3, 4, 5],
  test "single element" 5 5 [5],
  test "two elements" 3 4 [3, 4],
  test "negative start" (-2) 2 [(-2), (-1), 0, 1, 2]]
  where
    test name start end result = primCase name _math_range [int32 start, int32 end] (list $ int32 <$> result)

mathMaybeRem :: TTerm TestGroup
mathMaybeRem = subgroup "maybeRem" [
  test "basic remainder" 10 3 (Just 1),
  test "exact division" 10 2 (Just 0),
  test "division by zero" 10 0 Nothing,
  test "negative dividend" (-10) 3 (Just (-1)),
  test "negative divisor" 10 (-3) (Just 1)]
  where
    test name x y result = primCase name _math_maybeRem [int32 x, int32 y] (optionalInt32 result)

mathSignum :: TTerm TestGroup
mathSignum = subgroup "signum" [
  test "positive" 5 1,
  test "negative" (-5) (-1),
  test "zero" 0 0]
  where
    test name x result = primCase name _math_signum [int32 x] (int32 result)

mathSub :: TTerm TestGroup
mathSub = subgroup "sub" [
  test "positive numbers" 10 3 7,
  test "negative numbers" (-10) (-3) (-7),
  test "mixed sign" 10 (-3) 13,
  test "with zero" 42 0 42]
  where
    test name x y result = primCase name _math_sub [int32 x, int32 y] (int32 result)

mathMaybeSucc :: TTerm TestGroup
mathMaybeSucc = subgroup "maybeSucc" [
  test "positive" 5 (Just 6),
  test "zero" 0 (Just 1),
  test "negative" (-5) (Just (-4)),
  test "maxBound" 2147483647 Nothing]
  where
    test name x result = primCase name _math_maybeSucc [int32 x] (optionalInt32 result)

-- Float64 tests
--
-- Note on floating-point portability (see also docs/recipes/extending-tests.md):
-- Transcendental functions (sin, cos, exp, atanh, etc.) are implemented via the platform's
-- C math library (libm), which is NOT required by IEEE 754 to produce bit-identical results
-- across platforms. Even GHC delegates to libm, so Haskell-computed expected values like
-- (sinh 1.0) can differ by 1 ULP between macOS and Linux.
--
-- When adding float64 test cases for transcendental functions:
--   * Prefer inputs that produce exact results: sin(0)=0, exp(0)=1, sqrt(4)=2
--   * If a non-trivial input is needed, use roundFloat64 (or roundFloat32/roundBigfloat)
--     on both the expected value and the test input's expected result to eliminate
--     platform-dependent rounding in the last digit.

-- | Special float64 values: positive infinity, negative infinity, and NaN.
-- These are used to test that domain-restricted primitives return IEEE 754
-- special values (NaN/Inf) rather than throwing exceptions, and that all
-- float-accepting primitives propagate NaN/Inf correctly.
nan64 :: Double
nan64 = 0/0

posInf64 :: Double
posInf64 = 1/0

negInf64 :: Double
negInf64 = -1/0

-- | Number of significant digits to use when rounding transcendental results
-- for platform-independent comparison. 12 digits is well within float64 precision
-- (which has ~15.9 significant digits) while safely absorbing 1-ULP differences.
roundDigits :: Int
roundDigits = 12

-- | Build a test case that rounds both the computed and expected results to
-- a fixed number of significant digits using roundFloat64. This makes
-- transcendental function tests portable across platforms with different libm
-- implementations.
roundedPrimCase1 :: String -> Name -> Double -> Double -> TTerm TestCaseWithMetadata
roundedPrimCase1 cname primName x result = evalCase cname input output
  where
    input = Terms.primitive _math_roundFloat64 @@ int32 roundDigits @@ (Terms.primitive primName @@ float64 x)
    output = float64 (Math.roundFloat64 roundDigits result)

roundedPrimCase2 :: String -> Name -> Double -> Double -> Double -> TTerm TestCaseWithMetadata
roundedPrimCase2 cname primName x y result = evalCase cname input output
  where
    input = Terms.primitive _math_roundFloat64 @@ int32 roundDigits @@ (Terms.primitive primName @@ float64 x @@ float64 y)
    output = float64 (Math.roundFloat64 roundDigits result)

mathAddFloat64 :: TTerm TestGroup
mathAddFloat64 = subgroup "addFloat64" [
  test "positive numbers" 3.0 5.0 8.0,
  test "negative numbers" (-3.0) (-5.0) (-8.0),
  test "mixed sign" 10.0 (-3.0) 7.0,
  test "with zero" 42.0 0.0 42.0,
  test "fractional" 1.5 2.5 4.0]
  where
    test name x y result = primCase name _math_addFloat64 [float64 x, float64 y] (float64 result)

mathMulFloat64 :: TTerm TestGroup
mathMulFloat64 = subgroup "mulFloat64" [
  test "positive numbers" 3.0 5.0 15.0,
  test "negative numbers" (-3.0) (-5.0) 15.0,
  test "mixed sign" 10.0 (-3.0) (-30.0),
  test "with zero" 42.0 0.0 0.0,
  test "with one" 42.0 1.0 42.0,
  test "fractional" 1.5 2.0 3.0]
  where
    test name x y result = primCase name _math_mulFloat64 [float64 x, float64 y] (float64 result)

mathNegateFloat64 :: TTerm TestGroup
mathNegateFloat64 = subgroup "negateFloat64" [
  test "positive" 5.0 (-5.0),
  test "negative" (-5.0) 5.0,
  test "zero" 0.0 (-0.0),
  test "fractional" 1.5 (-1.5)]
  where
    test name x result = primCase name _math_negateFloat64 [float64 x] (float64 result)

mathSubFloat64 :: TTerm TestGroup
mathSubFloat64 = subgroup "subFloat64" [
  test "positive numbers" 5.0 3.0 2.0,
  test "negative result" 3.0 5.0 (-2.0),
  test "negative numbers" (-3.0) (-5.0) 2.0,
  test "with zero" 42.0 0.0 42.0,
  test "same value" 42.0 42.0 0.0,
  test "fractional" 2.5 1.5 1.0]
  where
    test name x y result = primCase name _math_subFloat64 [float64 x, float64 y] (float64 result)

mathE :: TTerm TestGroup
mathE = subgroup "e" [
  evalCase "Euler's number"
    (Terms.primitive _math_roundFloat64 @@ int32 roundDigits @@ Terms.primitive _math_e)
    (float64 (Math.roundFloat64 roundDigits (exp 1)))]

mathPi :: TTerm TestGroup
mathPi = subgroup "pi" [
  evalCase "pi constant"
    (Terms.primitive _math_roundFloat64 @@ int32 roundDigits @@ Terms.primitive _math_pi)
    (float64 (Math.roundFloat64 roundDigits pi))]

mathSin :: TTerm TestGroup
mathSin = subgroup "sin" [
  test "sin 0" 0.0 0.0,
  roundedPrimCase1 "sin pi/2" _math_sin (pi / 2) 1.0,
  roundedPrimCase1 "sin pi" _math_sin pi (sin pi),
  roundedPrimCase1 "sin 1" _math_sin 1.0 (sin 1.0),
  roundedPrimCase1 "sin 0.5" _math_sin 0.5 (sin 0.5),
  -- Special values
  test "sin NaN" nan64 nan64,
  test "sin +Inf" posInf64 nan64,
  test "sin -Inf" negInf64 nan64]
  where
    test name x result = primCase name _math_sin [float64 x] (float64 result)

mathCos :: TTerm TestGroup
mathCos = subgroup "cos" [
  test "cos 0" 0.0 1.0,
  roundedPrimCase1 "cos pi/2" _math_cos (pi / 2) (cos (pi / 2)),
  test "cos pi" pi (-1.0),
  roundedPrimCase1 "cos 1" _math_cos 1.0 (cos 1.0),
  roundedPrimCase1 "cos 0.5" _math_cos 0.5 (cos 0.5),
  -- Special values
  test "cos NaN" nan64 nan64,
  test "cos +Inf" posInf64 nan64,
  test "cos -Inf" negInf64 nan64]
  where
    test name x result = primCase name _math_cos [float64 x] (float64 result)

mathTan :: TTerm TestGroup
mathTan = subgroup "tan" [
  test "tan 0" 0.0 0.0,
  roundedPrimCase1 "tan pi/4" _math_tan (pi / 4) (tan (pi / 4)),
  roundedPrimCase1 "tan 1" _math_tan 1.0 (tan 1.0),
  roundedPrimCase1 "tan 0.5" _math_tan 0.5 (tan 0.5),
  -- Special values
  test "tan NaN" nan64 nan64,
  test "tan +Inf" posInf64 nan64,
  test "tan -Inf" negInf64 nan64]
  where
    test name x result = primCase name _math_tan [float64 x] (float64 result)

mathAsin :: TTerm TestGroup
mathAsin = subgroup "asin" [
  test "asin 0" 0.0 0.0,
  roundedPrimCase1 "asin 1" _math_asin 1.0 (pi / 2),
  roundedPrimCase1 "asin -1" _math_asin (-1.0) (-(pi / 2)),
  roundedPrimCase1 "asin 0.5" _math_asin 0.5 (asin 0.5),
  -- Out-of-domain: returns NaN
  test "asin below domain" (-2.0) nan64,
  test "asin above domain" 2.0 nan64,
  -- Special values
  test "asin NaN" nan64 nan64,
  test "asin +Inf" posInf64 nan64,
  test "asin -Inf" negInf64 nan64]
  where
    test name x result = primCase name _math_asin [float64 x] (float64 result)

mathAcos :: TTerm TestGroup
mathAcos = subgroup "acos" [
  test "acos 1" 1.0 0.0,
  roundedPrimCase1 "acos 0" _math_acos 0.0 (pi / 2),
  roundedPrimCase1 "acos -1" _math_acos (-1.0) pi,
  roundedPrimCase1 "acos 0.5" _math_acos 0.5 (acos 0.5),
  -- Out-of-domain: returns NaN
  test "acos below domain" (-2.0) nan64,
  test "acos above domain" 2.0 nan64,
  -- Special values
  test "acos NaN" nan64 nan64,
  test "acos +Inf" posInf64 nan64,
  test "acos -Inf" negInf64 nan64]
  where
    test name x result = primCase name _math_acos [float64 x] (float64 result)

mathAtan :: TTerm TestGroup
mathAtan = subgroup "atan" [
  test "atan 0" 0.0 0.0,
  roundedPrimCase1 "atan 1" _math_atan 1.0 (pi / 4),
  roundedPrimCase1 "atan 0.5" _math_atan 0.5 (atan 0.5),
  -- Special values: atan's range is (-pi/2, pi/2); saturates at infinities
  test "atan NaN" nan64 nan64,
  roundedPrimCase1 "atan +Inf" _math_atan posInf64 (pi / 2),
  roundedPrimCase1 "atan -Inf" _math_atan negInf64 (-(pi / 2))]
  where
    test name x result = primCase name _math_atan [float64 x] (float64 result)

mathAtan2 :: TTerm TestGroup
mathAtan2 = subgroup "atan2" [
  roundedPrimCase2 "atan2 1 1" _math_atan2 1.0 1.0 (pi / 4),
  roundedPrimCase2 "atan2 1 0" _math_atan2 1.0 0.0 (pi / 2),
  test "atan2 0 1" 0.0 1.0 0.0,
  roundedPrimCase2 "atan2 3 4" _math_atan2 3.0 4.0 (atan2 3.0 4.0),
  -- Special values in y (first argument)
  test "atan2 NaN 1" nan64 1.0 nan64,
  roundedPrimCase2 "atan2 +Inf 1" _math_atan2 posInf64 1.0 (pi / 2),
  roundedPrimCase2 "atan2 -Inf 1" _math_atan2 negInf64 1.0 (-(pi / 2)),
  -- Special values in x (second argument)
  test "atan2 1 NaN" 1.0 nan64 nan64,
  test "atan2 1 +Inf" 1.0 posInf64 0.0,
  roundedPrimCase2 "atan2 1 -Inf" _math_atan2 1.0 negInf64 pi,
  -- Cross-infinity combinations: Haskell returns NaN; other languages' native atan2
  -- returns ±pi/4 or ±3pi/4, so each implementation must special-case these.
  test "atan2 +Inf +Inf" posInf64 posInf64 nan64,
  test "atan2 +Inf -Inf" posInf64 negInf64 nan64,
  test "atan2 -Inf +Inf" negInf64 posInf64 nan64,
  test "atan2 -Inf -Inf" negInf64 negInf64 nan64]
  where
    test name y x result = primCase name _math_atan2 [float64 y, float64 x] (float64 result)

mathSinh :: TTerm TestGroup
mathSinh = subgroup "sinh" [
  test "sinh 0" 0.0 0.0,
  roundedPrimCase1 "sinh 1" _math_sinh 1.0 (sinh 1.0),
  roundedPrimCase1 "sinh 2" _math_sinh 2.0 (sinh 2.0),
  -- Special values
  test "sinh NaN" nan64 nan64,
  test "sinh +Inf" posInf64 posInf64,
  test "sinh -Inf" negInf64 negInf64]
  where
    test name x result = primCase name _math_sinh [float64 x] (float64 result)

mathCosh :: TTerm TestGroup
mathCosh = subgroup "cosh" [
  test "cosh 0" 0.0 1.0,
  roundedPrimCase1 "cosh 1" _math_cosh 1.0 (cosh 1.0),
  roundedPrimCase1 "cosh 2" _math_cosh 2.0 (cosh 2.0),
  -- Special values
  test "cosh NaN" nan64 nan64,
  test "cosh +Inf" posInf64 posInf64,
  test "cosh -Inf" negInf64 posInf64]
  where
    test name x result = primCase name _math_cosh [float64 x] (float64 result)

mathTanh :: TTerm TestGroup
mathTanh = subgroup "tanh" [
  test "tanh 0" 0.0 0.0,
  roundedPrimCase1 "tanh 1" _math_tanh 1.0 (tanh 1.0),
  roundedPrimCase1 "tanh 0.5" _math_tanh 0.5 (tanh 0.5),
  -- Special values: tanh's range is (-1, 1); saturates at infinities
  test "tanh NaN" nan64 nan64,
  test "tanh +Inf" posInf64 1.0,
  test "tanh -Inf" negInf64 (-1.0)]
  where
    test name x result = primCase name _math_tanh [float64 x] (float64 result)

mathAsinh :: TTerm TestGroup
mathAsinh = subgroup "asinh" [
  test "asinh 0" 0.0 0.0,
  roundedPrimCase1 "asinh 1" _math_asinh 1.0 (asinh 1.0),
  roundedPrimCase1 "asinh 0.5" _math_asinh 0.5 (asinh 0.5),
  -- Special values
  test "asinh NaN" nan64 nan64,
  test "asinh +Inf" posInf64 posInf64,
  test "asinh -Inf" negInf64 negInf64]
  where
    test name x result = primCase name _math_asinh [float64 x] (float64 result)

mathAcosh :: TTerm TestGroup
mathAcosh = subgroup "acosh" [
  test "acosh 1" 1.0 0.0,
  roundedPrimCase1 "acosh 2" _math_acosh 2.0 (acosh 2.0),
  roundedPrimCase1 "acosh 3" _math_acosh 3.0 (acosh 3.0),
  -- Out-of-domain: returns NaN (lower limit is 1)
  test "acosh below domain" 0.5 nan64,
  test "acosh negative" (-1.0) nan64,
  -- Special values
  test "acosh NaN" nan64 nan64,
  test "acosh +Inf" posInf64 posInf64,
  test "acosh -Inf" negInf64 nan64]
  where
    test name x result = primCase name _math_acosh [float64 x] (float64 result)

mathAtanh :: TTerm TestGroup
mathAtanh = subgroup "atanh" [
  test "atanh 0" 0.0 0.0,
  roundedPrimCase1 "atanh 0.5" _math_atanh 0.5 (atanh 0.5),
  roundedPrimCase1 "atanh 0.1" _math_atanh 0.1 (atanh 0.1),
  -- Boundary: open interval (-1, 1); at the boundary atanh returns infinity
  test "atanh upper boundary" 1.0 posInf64,
  test "atanh lower boundary" (-1.0) negInf64,
  -- Out-of-domain: returns NaN
  test "atanh above domain" 2.0 nan64,
  test "atanh below domain" (-2.0) nan64,
  -- Special values
  test "atanh NaN" nan64 nan64,
  test "atanh +Inf" posInf64 nan64,
  test "atanh -Inf" negInf64 nan64]
  where
    test name x result = primCase name _math_atanh [float64 x] (float64 result)

mathExp :: TTerm TestGroup
mathExp = subgroup "exp" [
  test "exp 0" 0.0 1.0,
  roundedPrimCase1 "exp 1" _math_exp 1.0 (exp 1.0),
  roundedPrimCase1 "exp -1" _math_exp (-1.0) (exp (-1.0)),
  roundedPrimCase1 "exp 2" _math_exp 2.0 (exp 2.0),
  roundedPrimCase1 "exp 0.5" _math_exp 0.5 (exp 0.5),
  -- Special values
  test "exp NaN" nan64 nan64,
  test "exp +Inf" posInf64 posInf64,
  test "exp -Inf" negInf64 0.0]
  where
    test name x result = primCase name _math_exp [float64 x] (float64 result)

mathLog :: TTerm TestGroup
mathLog = subgroup "log" [
  test "log 1" 1.0 0.0,
  roundedPrimCase1 "log e" _math_log (exp 1.0) 1.0,
  roundedPrimCase1 "log 2" _math_log 2.0 (log 2.0),
  roundedPrimCase1 "log 10" _math_log 10.0 (log 10.0),
  -- Boundary: domain is (0, inf); at the boundary log returns -Inf
  test "log 0" 0.0 negInf64,
  -- Out-of-domain: returns NaN
  test "log negative" (-1.0) nan64,
  -- Special values
  test "log NaN" nan64 nan64,
  test "log +Inf" posInf64 posInf64,
  test "log -Inf" negInf64 nan64]
  where
    test name x result = primCase name _math_log [float64 x] (float64 result)

mathLogBase :: TTerm TestGroup
mathLogBase = subgroup "logBase" [
  test "log10 1" 10.0 1.0 0.0,
  test "log10 10" 10.0 10.0 1.0,
  test "log10 100" 10.0 100.0 2.0,
  test "log2 8" 2.0 8.0 3.0,
  roundedPrimCase2 "log2 10" _math_logBase 2.0 10.0 (logBase 2.0 10.0),
  -- Boundary/out-of-domain in x (second argument): x=0 -> -Inf, x<0 -> NaN
  test "logBase 10 0" 10.0 0.0 negInf64,
  test "logBase 10 negative" 10.0 (-1.0) nan64,
  -- Out-of-domain in base (first argument)
  test "logBase negative 10" (-1.0) 10.0 nan64,
  -- Special values in x (second argument)
  test "logBase 10 NaN" 10.0 nan64 nan64,
  test "logBase 10 +Inf" 10.0 posInf64 posInf64,
  test "logBase 10 -Inf" 10.0 negInf64 nan64,
  -- Special values in base (first argument)
  test "logBase NaN 10" nan64 10.0 nan64,
  test "logBase +Inf 10" posInf64 10.0 0.0,
  test "logBase -Inf 10" negInf64 10.0 nan64]
  where
    test name base x result = primCase name _math_logBase [float64 base, float64 x] (float64 result)

mathPow :: TTerm TestGroup
mathPow = subgroup "pow" [
  test "2^3" 2.0 3.0 8.0,
  test "10^0" 10.0 0.0 1.0,
  test "2^-1" 2.0 (-1.0) 0.5,
  roundedPrimCase2 "2^0.5" _math_pow 2.0 0.5 (2.0 ** 0.5),
  -- Boundaries: 0^0 = 1, 0^(-1) = +Inf, negative^fractional = NaN
  test "0^0" 0.0 0.0 1.0,
  test "0^-1" 0.0 (-1.0) posInf64,
  test "(-1)^0.5" (-1.0) 0.5 nan64,
  -- Special values in base (first argument)
  test "NaN^2" nan64 2.0 nan64,
  test "+Inf^2" posInf64 2.0 posInf64,
  test "-Inf^2" negInf64 2.0 posInf64,
  test "+Inf^-1" posInf64 (-1.0) 0.0,
  -- Special values in exponent (second argument)
  test "2^NaN" 2.0 nan64 nan64,
  test "2^+Inf" 2.0 posInf64 posInf64,
  test "2^-Inf" 2.0 negInf64 0.0]
  where
    test name base exp result = primCase name _math_pow [float64 base, float64 exp] (float64 result)

mathSqrt :: TTerm TestGroup
mathSqrt = subgroup "sqrt" [
  test "sqrt 4" 4.0 2.0,
  test "sqrt 9" 9.0 3.0,
  test "sqrt 2" 2.0 (sqrt 2.0),
  test "sqrt 0" 0.0 0.0,
  roundedPrimCase1 "sqrt 3" _math_sqrt 3.0 (sqrt 3.0),
  -- Out-of-domain: returns NaN (domain [0, inf))
  test "sqrt negative" (-1.0) nan64,
  -- Special values
  test "sqrt NaN" nan64 nan64,
  test "sqrt +Inf" posInf64 posInf64,
  test "sqrt -Inf" negInf64 nan64]
  where
    test name x result = primCase name _math_sqrt [float64 x] (float64 result)

mathCeiling :: TTerm TestGroup
mathCeiling = subgroup "ceiling" [
  test "ceiling 3.2" 3.2 4.0,
  test "ceiling 3.0" 3.0 3.0,
  test "ceiling -3.2" (-3.2) (-3.0),
  test "ceiling -3.0" (-3.0) (-3.0),
  -- Special values propagate per IEEE 754 (see divergence comment on Math.ceiling)
  test "ceiling NaN" nan64 nan64,
  test "ceiling +Inf" posInf64 posInf64,
  test "ceiling -Inf" negInf64 negInf64]
  where
    test name x result = primCase name _math_ceiling [float64 x] (float64 result)

mathFloor :: TTerm TestGroup
mathFloor = subgroup "floor" [
  test "floor 3.8" 3.8 3.0,
  test "floor 3.0" 3.0 3.0,
  test "floor -3.2" (-3.2) (-4.0),
  test "floor -3.0" (-3.0) (-3.0),
  -- Special values propagate per IEEE 754 (see divergence comment on Math.floor)
  test "floor NaN" nan64 nan64,
  test "floor +Inf" posInf64 posInf64,
  test "floor -Inf" negInf64 negInf64]
  where
    test name x result = primCase name _math_floor [float64 x] (float64 result)

mathRound :: TTerm TestGroup
mathRound = subgroup "round" [
  test "round 3.4" 3.4 3.0,
  test "round 3.5" 3.5 4.0,
  test "round 3.6" 3.6 4.0,
  test "round -3.4" (-3.4) (-3.0),
  test "round -3.5" (-3.5) (-4.0),
  -- Special values propagate per IEEE 754 (see divergence comment on Math.round)
  test "round NaN" nan64 nan64,
  test "round +Inf" posInf64 posInf64,
  test "round -Inf" negInf64 negInf64]
  where
    test name x result = primCase name _math_round [float64 x] (float64 result)

mathRoundBigfloat :: TTerm TestGroup
mathRoundBigfloat = subgroup "roundBigfloat" [
  test "zero" 5 0.0 0.0,
  test "round pi to 4 digits" 4 3.141592653589793 3.142,
  test "round 1234.5 to 3 digits" 3 1234.5 1230.0,
  test "round 0.001234 to 2 digits" 2 0.001234 0.0012,
  test "negative" 3 (-1234.5) (-1230.0)]
  -- NOTE: bigfloat (Java BigDecimal / Python Decimal) cannot represent NaN or Inf,
  -- so roundBigfloat cannot be tested with those special values.]
  where
    test name n x result = primCase name _math_roundBigfloat [int32 n, bigfloat x] (bigfloat result)

mathRoundFloat32 :: TTerm TestGroup
mathRoundFloat32 = subgroup "roundFloat32" [
  test "zero" 5 0.0 0.0,
  test "round pi to 4 digits" 4 3.1415927 3.142,
  test "round 1234.5 to 3 digits" 3 1234.5 1230.0,
  test "negative" 3 (-1234.5) (-1230.0),
  -- Special values propagate unchanged (logBase 10 (abs x) is undefined for these)
  test "NaN" 3 (0/0) (0/0),
  test "+Inf" 3 (1/0) (1/0),
  test "-Inf" 3 (-1/0) (-1/0)]
  where
    test name n x result = primCase name _math_roundFloat32 [int32 n, float32 x] (float32 result)

mathRoundFloat64 :: TTerm TestGroup
mathRoundFloat64 = subgroup "roundFloat64" [
  test "zero" 5 0.0 0.0,
  test "round pi to 4 digits" 4 3.141592653589793 3.142,
  test "round pi to 10 digits" 10 3.141592653589793 3.141592654,
  test "round 1234.5 to 3 digits" 3 1234.5 1230.0,
  test "round 0.001234 to 2 digits" 2 0.001234 0.0012,
  test "negative" 3 (-1234.5) (-1230.0),
  test "round 1 digit" 1 9.876 10.0,
  -- Special values propagate unchanged (logBase 10 (abs x) is undefined for these)
  test "NaN" 3 nan64 nan64,
  test "+Inf" 3 posInf64 posInf64,
  test "-Inf" 3 negInf64 negInf64]
  where
    test name n x result = primCase name _math_roundFloat64 [int32 n, float64 x] (float64 result)

mathTruncate :: TTerm TestGroup
mathTruncate = subgroup "truncate" [
  test "truncate 3.8" 3.8 3.0,
  test "truncate 3.2" 3.2 3.0,
  test "truncate -3.8" (-3.8) (-3.0),
  test "truncate -3.2" (-3.2) (-3.0),
  -- Special values propagate per IEEE 754 (see divergence comment on Math.truncate)
  test "truncate NaN" nan64 nan64,
  test "truncate +Inf" posInf64 posInf64,
  test "truncate -Inf" negInf64 negInf64]
  where
    test name x result = primCase name _math_truncate [float64 x] (float64 result)

allTests :: TTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.math primitives" $
    supergroup "hydra.lib.math primitives" [
      -- Int32 primitives
      mathAbs,
      mathAdd,
      mathEven,
      mathMax,
      mathMaybeDiv,
      mathMin,
      mathMaybeMod,
      mathMul,
      mathNegate,
      mathOdd,
      mathMaybePred,
      mathRange,
      mathMaybeRem,
      mathSignum,
      mathSub,
      mathMaybeSucc,
      -- Float64 primitives
      mathAddFloat64,
      mathMulFloat64,
      mathNegateFloat64,
      mathSubFloat64,
      mathE,
      mathPi,
      mathSin,
      mathCos,
      mathTan,
      mathAsin,
      mathAcos,
      mathAtan,
      mathAtan2,
      mathSinh,
      mathCosh,
      mathTanh,
      mathAsinh,
      mathAcosh,
      mathAtanh,
      mathExp,
      mathLog,
      mathLogBase,
      mathPow,
      mathSqrt,
      mathCeiling,
      mathFloor,
      mathRound,
      mathRoundBigfloat,
      mathRoundFloat32,
      mathRoundFloat64,
      mathTruncate]
