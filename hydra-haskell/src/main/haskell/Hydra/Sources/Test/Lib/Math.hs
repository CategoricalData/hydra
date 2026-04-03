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
module_ = Module ns elements [Namespace "hydra.reduction", Namespace "hydra.show.core"] [] $
    Just "Test cases for hydra.lib.math primitives"
  where
    elements = [Phantoms.toDefinition allTests]

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

mathDiv :: TTerm TestGroup
mathDiv = subgroup "div" [
  test "exact division" 10 2 5,
  test "truncates toward negative infinity" 10 3 3,
  test "negative dividend" (-10) 3 (-4),
  test "negative divisor" 10 (-3) (-4)]
  where
    test name x y result = primCase name _math_div [int32 x, int32 y] (int32 result)

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

mathMod :: TTerm TestGroup
mathMod = subgroup "mod" [
  test "basic modulo" 10 3 1,
  test "exact division" 10 2 0,
  test "negative dividend" (-10) 3 2,
  test "negative divisor" 10 (-3) (-2)]
  where
    test name x y result = primCase name _math_mod [int32 x, int32 y] (int32 result)

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

mathPred :: TTerm TestGroup
mathPred = subgroup "pred" [
  test "positive" 5 4,
  test "zero" 0 (-1),
  test "negative" (-5) (-6)]
  where
    test name x result = primCase name _math_pred [int32 x] (int32 result)

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

mathRem :: TTerm TestGroup
mathRem = subgroup "rem" [
  test "basic remainder" 10 3 1,
  test "exact division" 10 2 0,
  test "negative dividend" (-10) 3 (-1),
  test "negative divisor" 10 (-3) 1]
  where
    test name x y result = primCase name _math_rem [int32 x, int32 y] (int32 result)

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

mathSucc :: TTerm TestGroup
mathSucc = subgroup "succ" [
  test "positive" 5 6,
  test "zero" 0 1,
  test "negative" (-5) (-4)]
  where
    test name x result = primCase name _math_succ [int32 x] (int32 result)

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
  roundedPrimCase1 "sin 0.5" _math_sin 0.5 (sin 0.5)]
  where
    test name x result = primCase name _math_sin [float64 x] (float64 result)

mathCos :: TTerm TestGroup
mathCos = subgroup "cos" [
  test "cos 0" 0.0 1.0,
  roundedPrimCase1 "cos pi/2" _math_cos (pi / 2) (cos (pi / 2)),
  test "cos pi" pi (-1.0),
  roundedPrimCase1 "cos 1" _math_cos 1.0 (cos 1.0),
  roundedPrimCase1 "cos 0.5" _math_cos 0.5 (cos 0.5)]
  where
    test name x result = primCase name _math_cos [float64 x] (float64 result)

mathTan :: TTerm TestGroup
mathTan = subgroup "tan" [
  test "tan 0" 0.0 0.0,
  roundedPrimCase1 "tan pi/4" _math_tan (pi / 4) (tan (pi / 4)),
  roundedPrimCase1 "tan 1" _math_tan 1.0 (tan 1.0),
  roundedPrimCase1 "tan 0.5" _math_tan 0.5 (tan 0.5)]
  where
    test name x result = primCase name _math_tan [float64 x] (float64 result)

mathAsin :: TTerm TestGroup
mathAsin = subgroup "asin" [
  test "asin 0" 0.0 0.0,
  roundedPrimCase1 "asin 1" _math_asin 1.0 (pi / 2),
  roundedPrimCase1 "asin -1" _math_asin (-1.0) (-(pi / 2)),
  roundedPrimCase1 "asin 0.5" _math_asin 0.5 (asin 0.5)]
  where
    test name x result = primCase name _math_asin [float64 x] (float64 result)

mathAcos :: TTerm TestGroup
mathAcos = subgroup "acos" [
  test "acos 1" 1.0 0.0,
  roundedPrimCase1 "acos 0" _math_acos 0.0 (pi / 2),
  roundedPrimCase1 "acos -1" _math_acos (-1.0) pi,
  roundedPrimCase1 "acos 0.5" _math_acos 0.5 (acos 0.5)]
  where
    test name x result = primCase name _math_acos [float64 x] (float64 result)

mathAtan :: TTerm TestGroup
mathAtan = subgroup "atan" [
  test "atan 0" 0.0 0.0,
  roundedPrimCase1 "atan 1" _math_atan 1.0 (pi / 4),
  roundedPrimCase1 "atan 0.5" _math_atan 0.5 (atan 0.5)]
  where
    test name x result = primCase name _math_atan [float64 x] (float64 result)

mathAtan2 :: TTerm TestGroup
mathAtan2 = subgroup "atan2" [
  roundedPrimCase2 "atan2 1 1" _math_atan2 1.0 1.0 (pi / 4),
  roundedPrimCase2 "atan2 1 0" _math_atan2 1.0 0.0 (pi / 2),
  test "atan2 0 1" 0.0 1.0 0.0,
  roundedPrimCase2 "atan2 3 4" _math_atan2 3.0 4.0 (atan2 3.0 4.0)]
  where
    test name y x result = primCase name _math_atan2 [float64 y, float64 x] (float64 result)

mathSinh :: TTerm TestGroup
mathSinh = subgroup "sinh" [
  test "sinh 0" 0.0 0.0,
  roundedPrimCase1 "sinh 1" _math_sinh 1.0 (sinh 1.0),
  roundedPrimCase1 "sinh 2" _math_sinh 2.0 (sinh 2.0)]
  where
    test name x result = primCase name _math_sinh [float64 x] (float64 result)

mathCosh :: TTerm TestGroup
mathCosh = subgroup "cosh" [
  test "cosh 0" 0.0 1.0,
  roundedPrimCase1 "cosh 1" _math_cosh 1.0 (cosh 1.0),
  roundedPrimCase1 "cosh 2" _math_cosh 2.0 (cosh 2.0)]
  where
    test name x result = primCase name _math_cosh [float64 x] (float64 result)

mathTanh :: TTerm TestGroup
mathTanh = subgroup "tanh" [
  test "tanh 0" 0.0 0.0,
  roundedPrimCase1 "tanh 1" _math_tanh 1.0 (tanh 1.0),
  roundedPrimCase1 "tanh 0.5" _math_tanh 0.5 (tanh 0.5)]
  where
    test name x result = primCase name _math_tanh [float64 x] (float64 result)

mathAsinh :: TTerm TestGroup
mathAsinh = subgroup "asinh" [
  test "asinh 0" 0.0 0.0,
  roundedPrimCase1 "asinh 1" _math_asinh 1.0 (asinh 1.0),
  roundedPrimCase1 "asinh 0.5" _math_asinh 0.5 (asinh 0.5)]
  where
    test name x result = primCase name _math_asinh [float64 x] (float64 result)

mathAcosh :: TTerm TestGroup
mathAcosh = subgroup "acosh" [
  test "acosh 1" 1.0 0.0,
  roundedPrimCase1 "acosh 2" _math_acosh 2.0 (acosh 2.0),
  roundedPrimCase1 "acosh 3" _math_acosh 3.0 (acosh 3.0)]
  where
    test name x result = primCase name _math_acosh [float64 x] (float64 result)

mathAtanh :: TTerm TestGroup
mathAtanh = subgroup "atanh" [
  test "atanh 0" 0.0 0.0,
  roundedPrimCase1 "atanh 0.5" _math_atanh 0.5 (atanh 0.5),
  roundedPrimCase1 "atanh 0.1" _math_atanh 0.1 (atanh 0.1)]
  where
    test name x result = primCase name _math_atanh [float64 x] (float64 result)

mathExp :: TTerm TestGroup
mathExp = subgroup "exp" [
  test "exp 0" 0.0 1.0,
  roundedPrimCase1 "exp 1" _math_exp 1.0 (exp 1.0),
  roundedPrimCase1 "exp -1" _math_exp (-1.0) (exp (-1.0)),
  roundedPrimCase1 "exp 2" _math_exp 2.0 (exp 2.0),
  roundedPrimCase1 "exp 0.5" _math_exp 0.5 (exp 0.5)]
  where
    test name x result = primCase name _math_exp [float64 x] (float64 result)

mathLog :: TTerm TestGroup
mathLog = subgroup "log" [
  test "log 1" 1.0 0.0,
  roundedPrimCase1 "log e" _math_log (exp 1.0) 1.0,
  roundedPrimCase1 "log 2" _math_log 2.0 (log 2.0),
  roundedPrimCase1 "log 10" _math_log 10.0 (log 10.0)]
  where
    test name x result = primCase name _math_log [float64 x] (float64 result)

mathLogBase :: TTerm TestGroup
mathLogBase = subgroup "logBase" [
  test "log10 1" 10.0 1.0 0.0,
  test "log10 10" 10.0 10.0 1.0,
  test "log10 100" 10.0 100.0 2.0,
  test "log2 8" 2.0 8.0 3.0,
  roundedPrimCase2 "log2 10" _math_logBase 2.0 10.0 (logBase 2.0 10.0)]
  where
    test name base x result = primCase name _math_logBase [float64 base, float64 x] (float64 result)

mathPow :: TTerm TestGroup
mathPow = subgroup "pow" [
  test "2^3" 2.0 3.0 8.0,
  test "10^0" 10.0 0.0 1.0,
  test "2^-1" 2.0 (-1.0) 0.5,
  roundedPrimCase2 "2^0.5" _math_pow 2.0 0.5 (2.0 ** 0.5)]
  where
    test name base exp result = primCase name _math_pow [float64 base, float64 exp] (float64 result)

mathSqrt :: TTerm TestGroup
mathSqrt = subgroup "sqrt" [
  test "sqrt 4" 4.0 2.0,
  test "sqrt 9" 9.0 3.0,
  test "sqrt 2" 2.0 (sqrt 2.0),
  test "sqrt 0" 0.0 0.0,
  roundedPrimCase1 "sqrt 3" _math_sqrt 3.0 (sqrt 3.0)]
  where
    test name x result = primCase name _math_sqrt [float64 x] (float64 result)

mathCeiling :: TTerm TestGroup
mathCeiling = subgroup "ceiling" [
  test "ceiling 3.2" 3.2 4,
  test "ceiling 3.0" 3.0 3,
  test "ceiling -3.2" (-3.2) (-3),
  test "ceiling -3.0" (-3.0) (-3)]
  where
    test name x result = primCase name _math_ceiling [float64 x] (bigint result)

mathFloor :: TTerm TestGroup
mathFloor = subgroup "floor" [
  test "floor 3.8" 3.8 3,
  test "floor 3.0" 3.0 3,
  test "floor -3.2" (-3.2) (-4),
  test "floor -3.0" (-3.0) (-3)]
  where
    test name x result = primCase name _math_floor [float64 x] (bigint result)

mathRound :: TTerm TestGroup
mathRound = subgroup "round" [
  test "round 3.4" 3.4 3,
  test "round 3.5" 3.5 4,
  test "round 3.6" 3.6 4,
  test "round -3.4" (-3.4) (-3),
  test "round -3.5" (-3.5) (-4)]
  where
    test name x result = primCase name _math_round [float64 x] (bigint result)

mathRoundBigfloat :: TTerm TestGroup
mathRoundBigfloat = subgroup "roundBigfloat" [
  test "zero" 5 0.0 0.0,
  test "round pi to 4 digits" 4 3.141592653589793 3.142,
  test "round 1234.5 to 3 digits" 3 1234.5 1230.0,
  test "round 0.001234 to 2 digits" 2 0.001234 0.0012,
  test "negative" 3 (-1234.5) (-1230.0)]
  where
    test name n x result = primCase name _math_roundBigfloat [int32 n, bigfloat x] (bigfloat result)

mathRoundFloat32 :: TTerm TestGroup
mathRoundFloat32 = subgroup "roundFloat32" [
  test "zero" 5 0.0 0.0,
  test "round pi to 4 digits" 4 3.1415927 3.142,
  test "round 1234.5 to 3 digits" 3 1234.5 1230.0,
  test "negative" 3 (-1234.5) (-1230.0)]
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
  test "round 1 digit" 1 9.876 10.0]
  where
    test name n x result = primCase name _math_roundFloat64 [int32 n, float64 x] (float64 result)

mathTruncate :: TTerm TestGroup
mathTruncate = subgroup "truncate" [
  test "truncate 3.8" 3.8 3,
  test "truncate 3.2" 3.2 3,
  test "truncate -3.8" (-3.8) (-3),
  test "truncate -3.2" (-3.2) (-3)]
  where
    test name x result = primCase name _math_truncate [float64 x] (bigint result)

allTests :: TTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.math primitives" $
    supergroup "hydra.lib.math primitives" [
      -- Int32 primitives
      mathAbs,
      mathAdd,
      mathDiv,
      mathEven,
      mathMax,
      mathMaybeDiv,
      mathMin,
      mathMaybeMod,
      mathMod,
      mathMul,
      mathNegate,
      mathOdd,
      mathMaybePred,
      mathPred,
      mathRange,
      mathMaybeRem,
      mathRem,
      mathSignum,
      mathSub,
      mathMaybeSucc,
      mathSucc,
      -- Float64 primitives
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
