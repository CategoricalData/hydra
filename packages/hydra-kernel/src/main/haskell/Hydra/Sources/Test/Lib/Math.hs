{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Lib.Math where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import Hydra.Overlay.Haskell.Dsl.Terms (ToPrimName)
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Hydra.Overlay.Haskell.Lib.Math as Math
import qualified Hydra.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Lib.Math as DefMath


ns :: ModuleName
ns = ModuleName "hydra.test.lib.math"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.reduction", ModuleName "hydra.print.core", ModuleName "hydra.core", ModuleName "hydra.errors", ModuleName "hydra.test.testGraph", ModuleName "hydra.testing"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.math primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

optionalInt32 :: Maybe Int -> TypedTerm Term
optionalInt32 Nothing = Core.termOptional nothing
optionalInt32 (Just x) = Core.termOptional $ just (int32 x)

-- | Generic optional-of-integer-literal builder, parameterized by the literal constructor
-- (int8, int16, ..., bigint), for conformance-matrix transcription across all 9 integral types.
optionalOf :: (a -> TypedTerm Term) -> Maybe a -> TypedTerm Term
optionalOf _ Nothing = Core.termOptional nothing
optionalOf ctor (Just x) = Core.termOptional $ just (ctor x)

-- Test groups for hydra.lib.math primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.math primitives" $
    supergroup "hydra.lib.math primitives" [
      -- Int32 primitives
      mathAbs,
      mathAdd,
      mathEven,
      mathDiv,
      mathMod,
      mathMul,
      mathNegate,
      mathOdd,
      mathRange,
      mathRem,
      mathSignum,
      mathSub,
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
      mathDivide,
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
      mathRoundFloat32,
      mathRoundFloat64,
      mathTruncate,
      -- Constraint-polymorphic ('numeric') dispatch: add/sub/mul/negate applied to numeric types
      -- other than int32, exercising value-level dispatch on the runtime literal variant (#566).
      mathNumericDispatch]

-- | Cases sourced from round3-317-conformance-matrix.md §5 (numeric class, all 11 types),
-- machine-verified against an independent implementation (round4-317-matrix-attack.md §1).
mathAbs :: TypedTerm TestGroup
mathAbs = subgroup "abs" [
  test8  "i8 nominal" 5 5,
  test8  "i8 neg" (-1) 1,
  test8  "i8 min" (-128) (-128),
  test8  "i8 min+1" (-127) 127,
  test16 "i16 nominal" 5 5,
  test16 "i16 neg" (-1) 1,
  test16 "i16 min" (-32768) (-32768),
  test16 "i16 min+1" (-32767) 32767,
  test32 "i32 nominal" 5 5,
  test32 "i32 neg" (-1) 1,
  test32 "i32 min" (-2147483648) (-2147483648),
  test32 "i32 min+1" (-2147483647) 2147483647,
  test64 "i64 nominal" 5 5,
  test64 "i64 neg" (-1) 1,
  test64 "i64 min" (-9223372036854775808) (-9223372036854775808),
  test64 "i64 min+1" (-9223372036854775807) 9223372036854775807,
  testU8  "u8 zero" 0 0,
  testU8  "u8 max" 255 255,
  testU16 "u16 zero" 0 0,
  testU16 "u16 max" 65535 65535,
  testU32 "u32 zero" 0 0,
  testU32 "u32 max" 4294967295 4294967295,
  testU64 "u64 zero" 0 0,
  testU64 "u64 max" 18446744073709551615 18446744073709551615,
  testBig "big nominal" 7 7,
  testBig "big neg large" (-1267650600228229401496703205376) 1267650600228229401496703205376,
  testF64 "f64 nominal" (-1.5) 1.5,
  testF64 "f64 negzero" (-0.0) 0.0,
  testF64 "f64 neginf" negInf64 posInf64,
  testF64 "f64 nan" nan64 nan64,
  testF64 "f64 subnormal" 5e-324 5e-324,
  testF32 "f32 nominal" (-1.5) 1.5,
  testF32 "f32 negzero" (-0.0) 0.0,
  testF32 "f32 neginf" negInf32 posInf32,
  testF32 "f32 nan" nan32 nan32]
  where
    test8   name x r = primCase name DefMath.abs [int8 x] (int8 r)
    test16  name x r = primCase name DefMath.abs [int16 x] (int16 r)
    test32  name x r = primCase name DefMath.abs [int32 x] (int32 r)
    test64  name x r = primCase name DefMath.abs [int64 x] (int64 r)
    testU8  name x r = primCase name DefMath.abs [uint8 x] (uint8 r)
    testU16 name x r = primCase name DefMath.abs [uint16 x] (uint16 r)
    testU32 name x r = primCase name DefMath.abs [uint32 x] (uint32 r)
    testU64 name x r = primCase name DefMath.abs [uint64 x] (uint64 r)
    testBig name x r = primCase name DefMath.abs [bigint x] (bigint r)
    testF64 name x r = primCase name DefMath.abs [float64 x] (float64 r)
    testF32 name x r = primCase name DefMath.abs [float32 x] (float32 r)

mathAcos :: TypedTerm TestGroup
mathAcos = subgroup "acos" [
  test "acos 1" 1.0 0.0,
  roundedPrimCase1 "acos 0" DefMath.acos 0.0 (pi / 2),
  roundedPrimCase1 "acos -1" DefMath.acos (-1.0) pi,
  roundedPrimCase1 "acos 0.5" DefMath.acos 0.5 (acos 0.5),
  -- Out-of-domain: returns NaN
  test "acos below domain" (-2.0) nan64,
  test "acos above domain" 2.0 nan64,
  -- Special values
  test "acos NaN" nan64 nan64,
  test "acos +Inf" posInf64 nan64,
  test "acos -Inf" negInf64 nan64]
  where
    test name x result = primCase name DefMath.acos [float64 x] (float64 result)

mathAcosh :: TypedTerm TestGroup
mathAcosh = subgroup "acosh" [
  test "acosh 1" 1.0 0.0,
  roundedPrimCase1 "acosh 2" DefMath.acosh 2.0 (acosh 2.0),
  roundedPrimCase1 "acosh 3" DefMath.acosh 3.0 (acosh 3.0),
  -- Out-of-domain: returns NaN (lower limit is 1)
  test "acosh below domain" 0.5 nan64,
  test "acosh negative" (-1.0) nan64,
  -- Special values
  test "acosh NaN" nan64 nan64,
  test "acosh +Inf" posInf64 posInf64,
  test "acosh -Inf" negInf64 nan64]
  where
    test name x result = primCase name DefMath.acosh [float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §1 (numeric class, all 11 instance
-- types), machine-verified against an independent implementation (round4-317-matrix-attack.md
-- §1). Covers wraparound at both bounds of every fixed-width type, exact bigint arithmetic
-- beyond int64/uint64 range, and IEEE 754 special values (ties-to-even in both directions,
-- signed zeros, NaN/Inf propagation) at float64 and float32.
mathAdd :: TypedTerm TestGroup
mathAdd = subgroup "add" [
  -- S(n)
  test8  "i8 nominal" 3 4 7,
  test8  "i8 wrap max" 127 1 (-128),
  test8  "i8 wrap min" (-128) (-1) 127,
  test8  "i8 max+max" 127 127 (-2),
  test8  "i8 min+min" (-128) (-128) 0,
  test16 "i16 nominal" 3 4 7,
  test16 "i16 wrap max" 32767 1 (-32768),
  test16 "i16 wrap min" (-32768) (-1) 32767,
  test16 "i16 max+max" 32767 32767 (-2),
  test16 "i16 min+min" (-32768) (-32768) 0,
  test32 "i32 nominal" 3 4 7,
  test32 "i32 wrap max" 2147483647 1 (-2147483648),
  test32 "i32 wrap min" (-2147483648) (-1) 2147483647,
  test32 "i32 max+max" 2147483647 2147483647 (-2),
  test32 "i32 min+min" (-2147483648) (-2147483648) 0,
  test64 "i64 nominal" 3 4 7,
  test64 "i64 wrap max" 9223372036854775807 1 (-9223372036854775808),
  test64 "i64 wrap min" (-9223372036854775808) (-1) 9223372036854775807,
  test64 "i64 max+max" 9223372036854775807 9223372036854775807 (-2),
  test64 "i64 min+min" (-9223372036854775808) (-9223372036854775808) 0,
  -- U(n)
  testU8  "u8 nominal" 3 4 7,
  testU8  "u8 wrap max" 255 1 0,
  testU8  "u8 max+max" 255 255 254,
  testU8  "u8 zero identity" 0 255 255,
  testU16 "u16 nominal" 3 4 7,
  testU16 "u16 wrap max" 65535 1 0,
  testU16 "u16 max+max" 65535 65535 65534,
  testU16 "u16 zero identity" 0 65535 65535,
  testU32 "u32 nominal" 3 4 7,
  testU32 "u32 wrap max" 4294967295 1 0,
  testU32 "u32 max+max" 4294967295 4294967295 4294967294,
  testU32 "u32 zero identity" 0 4294967295 4294967295,
  testU64 "u64 nominal" 3 4 7,
  testU64 "u64 wrap max" 18446744073709551615 1 0,
  testU64 "u64 max+max" 18446744073709551615 18446744073709551615 18446744073709551614,
  testU64 "u64 zero identity" 0 18446744073709551615 18446744073709551615,
  -- B (bigint)
  testBig "big nominal" 3 4 7,
  testBig "big past i64" 9223372036854775807 1 9223372036854775808,
  testBig "big past u64" 18446744073709551615 1 18446744073709551616,
  testBig "big cancel" (-1267650600228229401496703205376) 1267650600228229401496703205376 0,
  -- float64
  testF64 "f64 nominal" 1.5 2.25 3.75,
  testF64 "f64 classic" 0.1 0.2 0.30000000000000004,
  testF64 "f64 tie-even down" 9007199254740992.0 1.0 9007199254740992.0,
  testF64 "f64 tie-even up" 9007199254740994.0 1.0 9007199254740996.0,
  testF64 "f64 overflow" 1.7976931348623157e308 1.7976931348623157e308 posInf64,
  testF64 "f64 inf+neginf" posInf64 negInf64 nan64,
  testF64 "f64 nan" nan64 1.0 nan64,
  testF64 "f64 zeros mixed" (-0.0) 0.0 0.0,
  testF64 "f64 zeros neg" (-0.0) (-0.0) (-0.0),
  -- float32
  testF32 "f32 nominal" 1.5 2.25 3.75,
  testF32 "f32 tie-even" 16777216.0 1.0 16777216.0,
  testF32 "f32 exact step" 16777216.0 2.0 16777218.0,
  testF32 "f32 classic" 0.1 0.2 0.30000001192092896,
  testF32 "f32 overflow" 3.4028235e38 3.4028235e38 posInf32,
  testF32 "f32 inf+neginf" posInf32 negInf32 nan32,
  testF32 "f32 zeros mixed" (-0.0) 0.0 0.0]
  where
    test8   name x y r = primCase name DefMath.add [int8 x, int8 y] (int8 r)
    test16  name x y r = primCase name DefMath.add [int16 x, int16 y] (int16 r)
    test32  name x y r = primCase name DefMath.add [int32 x, int32 y] (int32 r)
    test64  name x y r = primCase name DefMath.add [int64 x, int64 y] (int64 r)
    testU8  name x y r = primCase name DefMath.add [uint8 x, uint8 y] (uint8 r)
    testU16 name x y r = primCase name DefMath.add [uint16 x, uint16 y] (uint16 r)
    testU32 name x y r = primCase name DefMath.add [uint32 x, uint32 y] (uint32 r)
    testU64 name x y r = primCase name DefMath.add [uint64 x, uint64 y] (uint64 r)
    testBig name x y r = primCase name DefMath.add [bigint x, bigint y] (bigint r)
    testF64 name x y r = primCase name DefMath.add [float64 x, float64 y] (float64 r)
    testF32 name x y r = primCase name DefMath.add [float32 x, float32 y] (float32 r)

mathAddFloat64 :: TypedTerm TestGroup
mathAddFloat64 = subgroup "addFloat64" [
  test "positive numbers" 3.0 5.0 8.0,
  test "negative numbers" (-3.0) (-5.0) (-8.0),
  test "mixed sign" 10.0 (-3.0) 7.0,
  test "with zero" 42.0 0.0 42.0,
  test "fractional" 1.5 2.5 4.0]
  where
    test name x y result = primCase name DefMath.addFloat64 [float64 x, float64 y] (float64 result)

mathAsin :: TypedTerm TestGroup
mathAsin = subgroup "asin" [
  test "asin 0" 0.0 0.0,
  roundedPrimCase1 "asin 1" DefMath.asin 1.0 (pi / 2),
  roundedPrimCase1 "asin -1" DefMath.asin (-1.0) (-(pi / 2)),
  roundedPrimCase1 "asin 0.5" DefMath.asin 0.5 (asin 0.5),
  -- Out-of-domain: returns NaN
  test "asin below domain" (-2.0) nan64,
  test "asin above domain" 2.0 nan64,
  -- Special values
  test "asin NaN" nan64 nan64,
  test "asin +Inf" posInf64 nan64,
  test "asin -Inf" negInf64 nan64]
  where
    test name x result = primCase name DefMath.asin [float64 x] (float64 result)

mathAsinh :: TypedTerm TestGroup
mathAsinh = subgroup "asinh" [
  test "asinh 0" 0.0 0.0,
  roundedPrimCase1 "asinh 1" DefMath.asinh 1.0 (asinh 1.0),
  roundedPrimCase1 "asinh 0.5" DefMath.asinh 0.5 (asinh 0.5),
  -- Special values
  test "asinh NaN" nan64 nan64,
  test "asinh +Inf" posInf64 posInf64,
  test "asinh -Inf" negInf64 negInf64]
  where
    test name x result = primCase name DefMath.asinh [float64 x] (float64 result)

mathAtan :: TypedTerm TestGroup
mathAtan = subgroup "atan" [
  test "atan 0" 0.0 0.0,
  roundedPrimCase1 "atan 1" DefMath.atan 1.0 (pi / 4),
  roundedPrimCase1 "atan 0.5" DefMath.atan 0.5 (atan 0.5),
  -- Special values: atan's range is (-pi/2, pi/2); saturates at infinities
  test "atan NaN" nan64 nan64,
  roundedPrimCase1 "atan +Inf" DefMath.atan posInf64 (pi / 2),
  roundedPrimCase1 "atan -Inf" DefMath.atan negInf64 (-(pi / 2))]
  where
    test name x result = primCase name DefMath.atan [float64 x] (float64 result)

mathAtan2 :: TypedTerm TestGroup
mathAtan2 = subgroup "atan2" [
  roundedPrimCase2 "atan2 1 1" DefMath.atan2 1.0 1.0 (pi / 4),
  roundedPrimCase2 "atan2 1 0" DefMath.atan2 1.0 0.0 (pi / 2),
  test "atan2 0 1" 0.0 1.0 0.0,
  roundedPrimCase2 "atan2 3 4" DefMath.atan2 3.0 4.0 (atan2 3.0 4.0),
  -- Special values in y (first argument)
  test "atan2 NaN 1" nan64 1.0 nan64,
  roundedPrimCase2 "atan2 +Inf 1" DefMath.atan2 posInf64 1.0 (pi / 2),
  roundedPrimCase2 "atan2 -Inf 1" DefMath.atan2 negInf64 1.0 (-(pi / 2)),
  -- Special values in x (second argument)
  test "atan2 1 NaN" 1.0 nan64 nan64,
  test "atan2 1 +Inf" 1.0 posInf64 0.0,
  roundedPrimCase2 "atan2 1 -Inf" DefMath.atan2 1.0 negInf64 pi,
  -- Cross-infinity combinations: Haskell returns NaN; other languages' native atan2
  -- returns ±pi/4 or ±3pi/4, so each implementation must special-case these.
  test "atan2 +Inf +Inf" posInf64 posInf64 nan64,
  test "atan2 +Inf -Inf" posInf64 negInf64 nan64,
  test "atan2 -Inf +Inf" negInf64 posInf64 nan64,
  test "atan2 -Inf -Inf" negInf64 negInf64 nan64]
  where
    test name y x result = primCase name DefMath.atan2 [float64 y, float64 x] (float64 result)

mathAtanh :: TypedTerm TestGroup
mathAtanh = subgroup "atanh" [
  test "atanh 0" 0.0 0.0,
  roundedPrimCase1 "atanh 0.5" DefMath.atanh 0.5 (atanh 0.5),
  roundedPrimCase1 "atanh 0.1" DefMath.atanh 0.1 (atanh 0.1),
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
    test name x result = primCase name DefMath.atanh [float64 x] (float64 result)

mathCeiling :: TypedTerm TestGroup
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
    test name x result = primCase name DefMath.ceiling [float64 x] (float64 result)

mathCos :: TypedTerm TestGroup
mathCos = subgroup "cos" [
  test "cos 0" 0.0 1.0,
  roundedPrimCase1 "cos pi/2" DefMath.cos (pi / 2) (cos (pi / 2)),
  test "cos pi" pi (-1.0),
  roundedPrimCase1 "cos 1" DefMath.cos 1.0 (cos 1.0),
  roundedPrimCase1 "cos 0.5" DefMath.cos 0.5 (cos 0.5),
  -- Special values
  test "cos NaN" nan64 nan64,
  test "cos +Inf" posInf64 nan64,
  test "cos -Inf" negInf64 nan64]
  where
    test name x result = primCase name DefMath.cos [float64 x] (float64 result)

mathCosh :: TypedTerm TestGroup
mathCosh = subgroup "cosh" [
  test "cosh 0" 0.0 1.0,
  roundedPrimCase1 "cosh 1" DefMath.cosh 1.0 (cosh 1.0),
  roundedPrimCase1 "cosh 2" DefMath.cosh 2.0 (cosh 2.0),
  -- Special values
  test "cosh NaN" nan64 nan64,
  test "cosh +Inf" posInf64 posInf64,
  test "cosh -Inf" negInf64 posInf64]
  where
    test name x result = primCase name DefMath.cosh [float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §12, machine-verified against an
-- independent IEEE-754 implementation (round4-317-matrix-attack.md §1). Includes the seven
-- -0.0-bearing rows originally withheld pending confirmation that the -0.0 literal round-trips
-- through code generation — CONFIRMED (see the plan doc's "-0.0 blocker resolved" note): the
-- JSON string sentinel survives the full DSL->JSON->Haskell pipeline intact.
mathDivide :: TypedTerm TestGroup
mathDivide = subgroup "divide" [
  test64 "f64 nominal" 7.0 2.0 3.5,
  test64 "f64 round" 1.0 3.0 (1.0 / 3.0),
  test64 "f64 pos over pos zero" 1.0 0.0 posInf64,
  test64 "f64 pos over neg zero" 1.0 (-0.0) negInf64,
  test64 "f64 neg over pos zero" (-1.0) 0.0 negInf64,
  test64 "f64 neg over neg zero" (-1.0) (-0.0) posInf64,
  test64 "f64 zero over zero" 0.0 0.0 nan64,
  test64 "f64 negzero over zero" (-0.0) 0.0 nan64,
  test64 "f64 inf over inf" posInf64 posInf64 nan64,
  test64 "f64 inf over finite" posInf64 2.0 posInf64,
  test64 "f64 inf over neg finite" posInf64 (-2.0) negInf64,
  test64 "f64 finite over inf" 1.0 posInf64 0.0,
  test64 "f64 finite over neg inf" 1.0 negInf64 (-0.0),
  test64 "f64 NaN dividend" nan64 1.0 nan64,
  test64 "f64 overflow" 1e308 1e-10 posInf64,
  test64 "f64 subnormal" 2.2250738585072014e-308 2.0 1.1125369292536007e-308,
  test64 "f64 underflow" 5e-324 2.0 0.0,
  test32 "f32 nominal" 7.0 2.0 3.5,
  test32 "f32 round" 1.0 3.0 (1.0 / 3.0),
  test32 "f32 pos over pos zero" 1.0 0.0 posInf32,
  test32 "f32 neg over pos zero" (-1.0) 0.0 negInf32,
  test32 "f32 pos over neg zero" 1.0 (-0.0) negInf32,
  test32 "f32 zero over zero" 0.0 0.0 nan32,
  test32 "f32 inf over inf" posInf32 posInf32 nan32,
  test32 "f32 finite over inf" 1.0 posInf32 0.0,
  test32 "f32 overflow" 3.4028235e38 1e-10 posInf32,
  test32 "f32 subnormal" 1.1754943508222875e-38 2.0 5.877471754111438e-39,
  test32 "f32 underflow" 1.401298464324817e-45 2.0 0.0]
  where
    test64 name x y result = primCase name DefMath.divide [float64 x, float64 y] (float64 result)
    test32 name x y result = primCase name DefMath.divide [float32 x, float32 y] (float32 result)

mathE :: TypedTerm TestGroup
mathE = subgroup "e" [
  evalCase "Euler's number"
    (Terms.primitive DefMath.roundFloat64 @@ int32 roundDigits @@ Terms.primitive DefMath.e)
    (float64 (Math.roundFloat64 roundDigits (exp 1)))]

-- | Cases sourced from round3-317-conformance-matrix.md §10 (integral class, all 9 int types).
mathEven :: TypedTerm TestGroup
mathEven = subgroup "even" [
  test8  "i8 min" (-128) true,
  test8  "i8 max" 127 false,
  test8  "i8 zero" 0 true,
  test8  "i8 neg" (-1) false,
  test16 "i16 min" (-32768) true,
  test16 "i16 max" 32767 false,
  test16 "i16 zero" 0 true,
  test16 "i16 neg" (-1) false,
  test32 "i32 min" (-2147483648) true,
  test32 "i32 max" 2147483647 false,
  test32 "i32 zero" 0 true,
  test32 "i32 neg" (-1) false,
  test64 "i64 min" (-9223372036854775808) true,
  test64 "i64 max" 9223372036854775807 false,
  test64 "i64 zero" 0 true,
  test64 "i64 neg" (-1) false,
  testU8  "u8 zero" 0 true,
  testU8  "u8 max" 255 false,
  testU8  "u8 two" 2 true,
  testU16 "u16 zero" 0 true,
  testU16 "u16 max" 65535 false,
  testU16 "u16 two" 2 true,
  testU32 "u32 zero" 0 true,
  testU32 "u32 max" 4294967295 false,
  testU32 "u32 two" 2 true,
  testU64 "u64 zero" 0 true,
  testU64 "u64 max" 18446744073709551615 false,
  testU64 "u64 two" 2 true,
  testBig "big large" 1267650600228229401496703205376 true,
  testBig "big large1" 1267650600228229401496703205377 false,
  testBig "big neg" (-2) true]
  where
    test8   name x r = primCase name DefMath.even [int8 x] r
    test16  name x r = primCase name DefMath.even [int16 x] r
    test32  name x r = primCase name DefMath.even [int32 x] r
    test64  name x r = primCase name DefMath.even [int64 x] r
    testU8  name x r = primCase name DefMath.even [uint8 x] r
    testU16 name x r = primCase name DefMath.even [uint16 x] r
    testU32 name x r = primCase name DefMath.even [uint32 x] r
    testU64 name x r = primCase name DefMath.even [uint64 x] r
    testBig name x r = primCase name DefMath.even [bigint x] r

mathExp :: TypedTerm TestGroup
mathExp = subgroup "exp" [
  test "exp 0" 0.0 1.0,
  roundedPrimCase1 "exp 1" DefMath.exp 1.0 (exp 1.0),
  roundedPrimCase1 "exp -1" DefMath.exp (-1.0) (exp (-1.0)),
  roundedPrimCase1 "exp 2" DefMath.exp 2.0 (exp 2.0),
  roundedPrimCase1 "exp 0.5" DefMath.exp 0.5 (exp 0.5),
  -- Special values
  test "exp NaN" nan64 nan64,
  test "exp +Inf" posInf64 posInf64,
  test "exp -Inf" negInf64 0.0]
  where
    test name x result = primCase name DefMath.exp [float64 x] (float64 result)

mathFloor :: TypedTerm TestGroup
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
    test name x result = primCase name DefMath.floor [float64 x] (float64 result)

mathLog :: TypedTerm TestGroup
mathLog = subgroup "log" [
  test "log 1" 1.0 0.0,
  roundedPrimCase1 "log e" DefMath.log (exp 1.0) 1.0,
  roundedPrimCase1 "log 2" DefMath.log 2.0 (log 2.0),
  roundedPrimCase1 "log 10" DefMath.log 10.0 (log 10.0),
  -- Boundary: domain is (0, inf); at the boundary log returns -Inf
  test "log 0" 0.0 negInf64,
  -- Out-of-domain: returns NaN
  test "log negative" (-1.0) nan64,
  -- Special values
  test "log NaN" nan64 nan64,
  test "log +Inf" posInf64 posInf64,
  test "log -Inf" negInf64 nan64]
  where
    test name x result = primCase name DefMath.log [float64 x] (float64 result)

mathLogBase :: TypedTerm TestGroup
mathLogBase = subgroup "logBase" [
  test "log10 1" 10.0 1.0 0.0,
  test "log10 10" 10.0 10.0 1.0,
  test "log10 100" 10.0 100.0 2.0,
  test "log2 8" 2.0 8.0 3.0,
  roundedPrimCase2 "log2 10" DefMath.logBase 2.0 10.0 (logBase 2.0 10.0),
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
    test name base x result = primCase name DefMath.logBase [float64 base, float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §7 (integral class, all 9 int types)
-- plus round4-317-matrix-attack.md §3.1's exact-negative-division addition (div(-6,2)=just -3,
-- the floor-adjust bug detector — no quadrant row above has a zero remainder to catch an
-- unconditional truncate-then-adjust bug). Machine-verified against an independent
-- implementation (round4 §1).
mathDiv :: TypedTerm TestGroup
mathDiv = subgroup "div" [
  test8  "i8 pp" 7 2 (Just 3),
  test8  "i8 np" (-7) 2 (Just (-4)),
  test8  "i8 pn" 7 (-2) (Just (-4)),
  test8  "i8 nn" (-7) (-2) (Just 3),
  test8  "i8 zero" 7 0 Nothing,
  test8  "i8 min-neg1" (-128) (-1) (Just (-128)),
  test8  "i8 np-exact" (-6) 2 (Just (-3)),
  test16 "i16 pp" 7 2 (Just 3),
  test16 "i16 np" (-7) 2 (Just (-4)),
  test16 "i16 pn" 7 (-2) (Just (-4)),
  test16 "i16 nn" (-7) (-2) (Just 3),
  test16 "i16 zero" 7 0 Nothing,
  test16 "i16 min-neg1" (-32768) (-1) (Just (-32768)),
  test16 "i16 np-exact" (-6) 2 (Just (-3)),
  test32 "i32 pp" 7 2 (Just 3),
  test32 "i32 np" (-7) 2 (Just (-4)),
  test32 "i32 pn" 7 (-2) (Just (-4)),
  test32 "i32 nn" (-7) (-2) (Just 3),
  test32 "i32 zero" 7 0 Nothing,
  test32 "i32 min-neg1" (-2147483648) (-1) (Just (-2147483648)),
  test32 "i32 np-exact" (-6) 2 (Just (-3)),
  test32 "i32 pn-exact" 6 (-2) (Just (-3)),
  test64 "i64 pp" 7 2 (Just 3),
  test64 "i64 np" (-7) 2 (Just (-4)),
  test64 "i64 pn" 7 (-2) (Just (-4)),
  test64 "i64 nn" (-7) (-2) (Just 3),
  test64 "i64 zero" 7 0 Nothing,
  test64 "i64 min-neg1" (-9223372036854775808) (-1) (Just (-9223372036854775808)),
  test64 "i64 np-exact" (-6) 2 (Just (-3)),
  testU8  "u8 nominal" 7 2 (Just 3),
  testU8  "u8 zero" 7 0 Nothing,
  testU8  "u8 max" 255 1 (Just 255),
  testU16 "u16 nominal" 7 2 (Just 3),
  testU16 "u16 zero" 7 0 Nothing,
  testU16 "u16 max" 65535 1 (Just 65535),
  testU32 "u32 nominal" 7 2 (Just 3),
  testU32 "u32 zero" 7 0 Nothing,
  testU32 "u32 max" 4294967295 1 (Just 4294967295),
  testU64 "u64 nominal" 7 2 (Just 3),
  testU64 "u64 zero" 7 0 Nothing,
  testU64 "u64 max" 18446744073709551615 1 (Just 18446744073709551615),
  testBig "big pp" 7 2 (Just 3),
  testBig "big np" (-7) 2 (Just (-4)),
  testBig "big nn" (-7) (-2) (Just 3),
  testBig "big large" 1267650600228229401496703205376 1125899906842624 (Just 1125899906842624),
  testBig "big zero" 7 0 Nothing]
  where
    test8   name x y r = primCase name DefMath.div [int8 x, int8 y] (optionalOf int8 r)
    test16  name x y r = primCase name DefMath.div [int16 x, int16 y] (optionalOf int16 r)
    test32  name x y r = primCase name DefMath.div [int32 x, int32 y] (optionalOf int32 r)
    test64  name x y r = primCase name DefMath.div [int64 x, int64 y] (optionalOf int64 r)
    testU8  name x y r = primCase name DefMath.div [uint8 x, uint8 y] (optionalOf uint8 r)
    testU16 name x y r = primCase name DefMath.div [uint16 x, uint16 y] (optionalOf uint16 r)
    testU32 name x y r = primCase name DefMath.div [uint32 x, uint32 y] (optionalOf uint32 r)
    testU64 name x y r = primCase name DefMath.div [uint64 x, uint64 y] (optionalOf uint64 r)
    testBig name x y r = primCase name DefMath.div [bigint x, bigint y] (optionalOf bigint r)


-- | Cases sourced from round3-317-conformance-matrix.md §8 (integral class, all 9 int types;
-- sign of result follows the divisor) plus round4-317-matrix-attack.md §3.1's exact-negative
-- addition (mod(-6,2)=just 0, the floor-adjust bug detector for mod).
mathMod :: TypedTerm TestGroup
mathMod = subgroup "mod" [
  test8  "i8 pp" 7 2 (Just 1),
  test8  "i8 np" (-7) 2 (Just 1),
  test8  "i8 pn" 7 (-2) (Just (-1)),
  test8  "i8 nn" (-7) (-2) (Just (-1)),
  test8  "i8 zero" 7 0 Nothing,
  test8  "i8 min-neg1" (-128) (-1) (Just 0),
  test8  "i8 np-exact" (-6) 2 (Just 0),
  test16 "i16 pp" 7 2 (Just 1),
  test16 "i16 np" (-7) 2 (Just 1),
  test16 "i16 pn" 7 (-2) (Just (-1)),
  test16 "i16 nn" (-7) (-2) (Just (-1)),
  test16 "i16 zero" 7 0 Nothing,
  test16 "i16 min-neg1" (-32768) (-1) (Just 0),
  test16 "i16 np-exact" (-6) 2 (Just 0),
  test32 "i32 pp" 7 2 (Just 1),
  test32 "i32 np" (-7) 2 (Just 1),
  test32 "i32 pn" 7 (-2) (Just (-1)),
  test32 "i32 nn" (-7) (-2) (Just (-1)),
  test32 "i32 zero" 7 0 Nothing,
  test32 "i32 min-neg1" (-2147483648) (-1) (Just 0),
  test32 "i32 np-exact" (-6) 2 (Just 0),
  test64 "i64 pp" 7 2 (Just 1),
  test64 "i64 np" (-7) 2 (Just 1),
  test64 "i64 pn" 7 (-2) (Just (-1)),
  test64 "i64 nn" (-7) (-2) (Just (-1)),
  test64 "i64 zero" 7 0 Nothing,
  test64 "i64 min-neg1" (-9223372036854775808) (-1) (Just 0),
  test64 "i64 np-exact" (-6) 2 (Just 0),
  testU8  "u8 nominal" 7 2 (Just 1),
  testU8  "u8 zero" 7 0 Nothing,
  testU8  "u8 max" 255 2 (Just 1),
  testU16 "u16 nominal" 7 2 (Just 1),
  testU16 "u16 zero" 7 0 Nothing,
  testU16 "u16 max" 65535 2 (Just 1),
  testU32 "u32 nominal" 7 2 (Just 1),
  testU32 "u32 zero" 7 0 Nothing,
  testU32 "u32 max" 4294967295 2 (Just 1),
  testU64 "u64 nominal" 7 2 (Just 1),
  testU64 "u64 zero" 7 0 Nothing,
  testU64 "u64 max" 18446744073709551615 2 (Just 1),
  testBig "big pp" 7 2 (Just 1),
  testBig "big np" (-7) 2 (Just 1),
  testBig "big pn" 7 (-2) (Just (-1)),
  testBig "big large" 1267650600228229401496703205381 1125899906842624 (Just 5),
  testBig "big zero" 7 0 Nothing]
  where
    test8   name x y r = primCase name DefMath.mod [int8 x, int8 y] (optionalOf int8 r)
    test16  name x y r = primCase name DefMath.mod [int16 x, int16 y] (optionalOf int16 r)
    test32  name x y r = primCase name DefMath.mod [int32 x, int32 y] (optionalOf int32 r)
    test64  name x y r = primCase name DefMath.mod [int64 x, int64 y] (optionalOf int64 r)
    testU8  name x y r = primCase name DefMath.mod [uint8 x, uint8 y] (optionalOf uint8 r)
    testU16 name x y r = primCase name DefMath.mod [uint16 x, uint16 y] (optionalOf uint16 r)
    testU32 name x y r = primCase name DefMath.mod [uint32 x, uint32 y] (optionalOf uint32 r)
    testU64 name x y r = primCase name DefMath.mod [uint64 x, uint64 y] (optionalOf uint64 r)
    testBig name x y r = primCase name DefMath.mod [bigint x, bigint y] (optionalOf bigint r)


-- | Cases sourced from round3-317-conformance-matrix.md §9 (integral class; sign of result
-- follows the dividend — the §8/§9 pairs share inputs but differ in expected sign, catching
-- any host that wires both names to the same native operator) plus round4-317-matrix-attack.md
-- §3.1's exact-negative addition (rem(-6,2)=just 0).
mathRem :: TypedTerm TestGroup
mathRem = subgroup "rem" [
  test8  "i8 pp" 7 2 (Just 1),
  test8  "i8 np" (-7) 2 (Just (-1)),
  test8  "i8 pn" 7 (-2) (Just 1),
  test8  "i8 nn" (-7) (-2) (Just (-1)),
  test8  "i8 zero" 7 0 Nothing,
  test8  "i8 min-neg1" (-128) (-1) (Just 0),
  test16 "i16 pp" 7 2 (Just 1),
  test16 "i16 np" (-7) 2 (Just (-1)),
  test16 "i16 pn" 7 (-2) (Just 1),
  test16 "i16 nn" (-7) (-2) (Just (-1)),
  test16 "i16 zero" 7 0 Nothing,
  test16 "i16 min-neg1" (-32768) (-1) (Just 0),
  test32 "i32 pp" 7 2 (Just 1),
  test32 "i32 np" (-7) 2 (Just (-1)),
  test32 "i32 pn" 7 (-2) (Just 1),
  test32 "i32 nn" (-7) (-2) (Just (-1)),
  test32 "i32 zero" 7 0 Nothing,
  test32 "i32 min-neg1" (-2147483648) (-1) (Just 0),
  test32 "i32 np-exact" (-6) 2 (Just 0),
  test64 "i64 pp" 7 2 (Just 1),
  test64 "i64 np" (-7) 2 (Just (-1)),
  test64 "i64 pn" 7 (-2) (Just 1),
  test64 "i64 nn" (-7) (-2) (Just (-1)),
  test64 "i64 zero" 7 0 Nothing,
  test64 "i64 min-neg1" (-9223372036854775808) (-1) (Just 0),
  testU8  "u8 nominal" 7 2 (Just 1),
  testU8  "u8 zero" 7 0 Nothing,
  testU16 "u16 nominal" 7 2 (Just 1),
  testU16 "u16 zero" 7 0 Nothing,
  testU32 "u32 nominal" 7 2 (Just 1),
  testU32 "u32 zero" 7 0 Nothing,
  testU64 "u64 nominal" 7 2 (Just 1),
  testU64 "u64 zero" 7 0 Nothing,
  testBig "big pp" 7 2 (Just 1),
  testBig "big np" (-7) 2 (Just (-1)),
  testBig "big pn" 7 (-2) (Just 1),
  testBig "big nn" (-7) (-2) (Just (-1)),
  testBig "big zero" 7 0 Nothing]
  where
    test8   name x y r = primCase name DefMath.rem [int8 x, int8 y] (optionalOf int8 r)
    test16  name x y r = primCase name DefMath.rem [int16 x, int16 y] (optionalOf int16 r)
    test32  name x y r = primCase name DefMath.rem [int32 x, int32 y] (optionalOf int32 r)
    test64  name x y r = primCase name DefMath.rem [int64 x, int64 y] (optionalOf int64 r)
    testU8  name x y r = primCase name DefMath.rem [uint8 x, uint8 y] (optionalOf uint8 r)
    testU16 name x y r = primCase name DefMath.rem [uint16 x, uint16 y] (optionalOf uint16 r)
    testU32 name x y r = primCase name DefMath.rem [uint32 x, uint32 y] (optionalOf uint32 r)
    testU64 name x y r = primCase name DefMath.rem [uint64 x, uint64 y] (optionalOf uint64 r)
    testBig name x y r = primCase name DefMath.rem [bigint x, bigint y] (optionalOf bigint r)


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
--   * If a non-trivial input is needed, use roundFloat64 (or roundFloat32)
--     on both the expected value and the test input's expected result to eliminate
--     platform-dependent rounding in the last digit.

-- | Cases sourced from round3-317-conformance-matrix.md §3 (numeric class, all 11 types),
-- machine-verified against an independent implementation (round4-317-matrix-attack.md §1).
mathMul :: TypedTerm TestGroup
mathMul = subgroup "mul" [
  test8  "i8 nominal" 6 7 42,
  test8  "i8 min*neg1" (-128) (-1) (-128),
  test8  "i8 max*2" 127 2 (-2),
  test8  "i8 min*2" (-128) 2 0,
  test8  "i8 max*max" 127 127 1,
  test16 "i16 nominal" 6 7 42,
  test16 "i16 min*neg1" (-32768) (-1) (-32768),
  test16 "i16 max*2" 32767 2 (-2),
  test16 "i16 min*2" (-32768) 2 0,
  test16 "i16 max*max" 32767 32767 1,
  test32 "i32 nominal" 6 7 42,
  test32 "i32 min*neg1" (-2147483648) (-1) (-2147483648),
  test32 "i32 max*2" 2147483647 2 (-2),
  test32 "i32 min*2" (-2147483648) 2 0,
  test32 "i32 max*max" 2147483647 2147483647 1,
  test64 "i64 nominal" 6 7 42,
  test64 "i64 min*neg1" (-9223372036854775808) (-1) (-9223372036854775808),
  test64 "i64 max*2" 9223372036854775807 2 (-2),
  test64 "i64 min*2" (-9223372036854775808) 2 0,
  test64 "i64 max*max" 9223372036854775807 9223372036854775807 1,
  testU8  "u8 nominal" 6 7 42,
  testU8  "u8 max*2" 255 2 254,
  testU8  "u8 max*max" 255 255 1,
  testU8  "u8 half*half" 16 16 0,
  testU16 "u16 nominal" 6 7 42,
  testU16 "u16 max*2" 65535 2 65534,
  testU16 "u16 max*max" 65535 65535 1,
  testU16 "u16 half*half" 256 256 0,
  testU32 "u32 nominal" 6 7 42,
  testU32 "u32 max*2" 4294967295 2 4294967294,
  testU32 "u32 max*max" 4294967295 4294967295 1,
  testU32 "u32 half*half" 65536 65536 0,
  testU64 "u64 nominal" 6 7 42,
  testU64 "u64 max*2" 18446744073709551615 2 18446744073709551614,
  testU64 "u64 max*max" 18446744073709551615 18446744073709551615 1,
  testU64 "u64 half*half" 4294967296 4294967296 0,
  testBig "big nominal" 6 7 42,
  testBig "big exact 2^64" 4294967296 4294967296 18446744073709551616,
  testBig "big neg large" (-1125899906842624) 1125899906842624 (-1267650600228229401496703205376),
  testF64 "f64 nominal" 1.5 2.0 3.0,
  testF64 "f64 round" 0.1 0.1 0.010000000000000002,
  testF64 "f64 overflow" 1e200 1e200 posInf64,
  testF64 "f64 underflow" 1e-200 1e-200 0.0,
  testF64 "f64 underflow neg" (-1e-200) 1e-200 (-0.0),
  testF64 "f64 inf*zero" posInf64 0.0 nan64,
  testF64 "f64 negzero" (-1.0) 0.0 (-0.0),
  testF64 "f64 nan" nan64 0.0 nan64,
  testF32 "f32 nominal" 1.5 2.0 3.0,
  testF32 "f32 round down" 16777215.0 16777215.0 281474943156224.0,
  testF32 "f32 overflow" 1e20 1e20 posInf32,
  testF32 "f32 underflow" 1e-30 1e-30 0.0,
  testF32 "f32 negzero" (-1.0) 0.0 (-0.0)]
  where
    test8   name x y r = primCase name DefMath.mul [int8 x, int8 y] (int8 r)
    test16  name x y r = primCase name DefMath.mul [int16 x, int16 y] (int16 r)
    test32  name x y r = primCase name DefMath.mul [int32 x, int32 y] (int32 r)
    test64  name x y r = primCase name DefMath.mul [int64 x, int64 y] (int64 r)
    testU8  name x y r = primCase name DefMath.mul [uint8 x, uint8 y] (uint8 r)
    testU16 name x y r = primCase name DefMath.mul [uint16 x, uint16 y] (uint16 r)
    testU32 name x y r = primCase name DefMath.mul [uint32 x, uint32 y] (uint32 r)
    testU64 name x y r = primCase name DefMath.mul [uint64 x, uint64 y] (uint64 r)
    testBig name x y r = primCase name DefMath.mul [bigint x, bigint y] (bigint r)
    testF64 name x y r = primCase name DefMath.mul [float64 x, float64 y] (float64 r)
    testF32 name x y r = primCase name DefMath.mul [float32 x, float32 y] (float32 r)

mathMulFloat64 :: TypedTerm TestGroup
mathMulFloat64 = subgroup "mulFloat64" [
  test "positive numbers" 3.0 5.0 15.0,
  test "negative numbers" (-3.0) (-5.0) 15.0,
  test "mixed sign" 10.0 (-3.0) (-30.0),
  test "with zero" 42.0 0.0 0.0,
  test "with one" 42.0 1.0 42.0,
  test "fractional" 1.5 2.0 3.0]
  where
    test name x y result = primCase name DefMath.mulFloat64 [float64 x, float64 y] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §4 (numeric class, all 11 types),
-- machine-verified against an independent implementation (round4-317-matrix-attack.md §1).
mathNegate :: TypedTerm TestGroup
mathNegate = subgroup "negate" [
  test8  "i8 nominal" 5 (-5),
  test8  "i8 min" (-128) (-128),
  test8  "i8 max" 127 (-127),
  test16 "i16 nominal" 5 (-5),
  test16 "i16 min" (-32768) (-32768),
  test16 "i16 max" 32767 (-32767),
  test32 "i32 nominal" 5 (-5),
  test32 "i32 min" (-2147483648) (-2147483648),
  test32 "i32 max" 2147483647 (-2147483647),
  test64 "i64 nominal" 5 (-5),
  test64 "i64 min" (-9223372036854775808) (-9223372036854775808),
  test64 "i64 max" 9223372036854775807 (-9223372036854775807),
  testU8  "u8 zero" 0 0,
  testU8  "u8 one" 1 255,
  testU8  "u8 max" 255 1,
  testU8  "u8 nominal" 100 156,
  testU16 "u16 zero" 0 0,
  testU16 "u16 one" 1 65535,
  testU16 "u16 max" 65535 1,
  testU16 "u16 nominal" 100 65436,
  testU32 "u32 zero" 0 0,
  testU32 "u32 one" 1 4294967295,
  testU32 "u32 max" 4294967295 1,
  testU32 "u32 nominal" 100 4294967196,
  testU64 "u64 zero" 0 0,
  testU64 "u64 one" 1 18446744073709551615,
  testU64 "u64 max" 18446744073709551615 1,
  testU64 "u64 nominal" 100 18446744073709551516,
  testBig "big nominal" (-5) 5,
  testBig "big large" 1267650600228229401496703205376 (-1267650600228229401496703205376),
  testF64 "f64 nominal" 1.5 (-1.5),
  testF64 "f64 poszero" 0.0 (-0.0),
  testF64 "f64 negzero" (-0.0) 0.0,
  testF64 "f64 inf" posInf64 negInf64,
  testF64 "f64 nan" nan64 nan64,
  testF32 "f32 nominal" 1.5 (-1.5),
  testF32 "f32 poszero" 0.0 (-0.0),
  testF32 "f32 negzero" (-0.0) 0.0,
  testF32 "f32 neginf" negInf32 posInf32]
  where
    test8   name x r = primCase name DefMath.negate [int8 x] (int8 r)
    test16  name x r = primCase name DefMath.negate [int16 x] (int16 r)
    test32  name x r = primCase name DefMath.negate [int32 x] (int32 r)
    test64  name x r = primCase name DefMath.negate [int64 x] (int64 r)
    testU8  name x r = primCase name DefMath.negate [uint8 x] (uint8 r)
    testU16 name x r = primCase name DefMath.negate [uint16 x] (uint16 r)
    testU32 name x r = primCase name DefMath.negate [uint32 x] (uint32 r)
    testU64 name x r = primCase name DefMath.negate [uint64 x] (uint64 r)
    testBig name x r = primCase name DefMath.negate [bigint x] (bigint r)
    testF64 name x r = primCase name DefMath.negate [float64 x] (float64 r)
    testF32 name x r = primCase name DefMath.negate [float32 x] (float32 r)

mathNegateFloat64 :: TypedTerm TestGroup
mathNegateFloat64 = subgroup "negateFloat64" [
  test "positive" 5.0 (-5.0),
  test "negative" (-5.0) 5.0,
  test "zero" 0.0 (-0.0),
  test "fractional" 1.5 (-1.5)]
  where
    test name x result = primCase name DefMath.negateFloat64 [float64 x] (float64 result)

-- | Value-level polymorphic dispatch of the 'numeric'-constrained primitives add/sub/mul/negate
--   on numeric types other than int32. The int32 cases are already covered by mathAdd/mathSub/
--   mathMul/mathNegate above; these exercise the runtime dispatch on float64, int64, and bigint
--   literals, confirming that the single polymorphic primitive computes with the correct per-type
--   semantics (#566).
mathNumericDispatch :: TypedTerm TestGroup
mathNumericDispatch = supergroup "polymorphic numeric dispatch" [
  subgroup "add" [
    primCase "float64" DefMath.add [float64 1.5, float64 2.5] (float64 4.0),
    primCase "int64"   DefMath.add [int64 1000000000000, int64 1] (int64 1000000000001),
    primCase "bigint"  DefMath.add [bigint 100000000000000000000, bigint 1] (bigint 100000000000000000001)],
  subgroup "sub" [
    primCase "float64" DefMath.sub [float64 5.0, float64 1.5] (float64 3.5),
    primCase "int64"   DefMath.sub [int64 1000000000001, int64 1] (int64 1000000000000),
    primCase "bigint"  DefMath.sub [bigint 100000000000000000001, bigint 1] (bigint 100000000000000000000)],
  subgroup "mul" [
    primCase "float64" DefMath.mul [float64 2.0, float64 2.5] (float64 5.0),
    primCase "int64"   DefMath.mul [int64 1000000, int64 1000000] (int64 1000000000000),
    primCase "bigint"  DefMath.mul [bigint 10000000000, bigint 10000000000] (bigint 100000000000000000000)],
  subgroup "negate" [
    primCase "float64" DefMath.negate [float64 2.5] (float64 (-2.5)),
    primCase "int64"   DefMath.negate [int64 1000000000000] (int64 (-1000000000000)),
    primCase "bigint"  DefMath.negate [bigint 100000000000000000000] (bigint (-100000000000000000000))]]

-- | Cases sourced from round3-317-conformance-matrix.md §11 (integral class, all 9 int types).
mathOdd :: TypedTerm TestGroup
mathOdd = subgroup "odd" [
  test8  "i8 min" (-128) false,
  test8  "i8 max" 127 true,
  test8  "i8 zero" 0 false,
  test8  "i8 neg" (-1) true,
  test16 "i16 min" (-32768) false,
  test16 "i16 max" 32767 true,
  test16 "i16 zero" 0 false,
  test16 "i16 neg" (-1) true,
  test32 "i32 min" (-2147483648) false,
  test32 "i32 max" 2147483647 true,
  test32 "i32 zero" 0 false,
  test32 "i32 neg" (-1) true,
  test64 "i64 min" (-9223372036854775808) false,
  test64 "i64 max" 9223372036854775807 true,
  test64 "i64 zero" 0 false,
  test64 "i64 neg" (-1) true,
  testU8  "u8 zero" 0 false,
  testU8  "u8 one" 1 true,
  testU8  "u8 max" 255 true,
  testU16 "u16 zero" 0 false,
  testU16 "u16 one" 1 true,
  testU16 "u16 max" 65535 true,
  testU32 "u32 zero" 0 false,
  testU32 "u32 one" 1 true,
  testU32 "u32 max" 4294967295 true,
  testU64 "u64 zero" 0 false,
  testU64 "u64 one" 1 true,
  testU64 "u64 max" 18446744073709551615 true,
  testBig "big large" 1267650600228229401496703205376 false,
  testBig "big large1" 1267650600228229401496703205377 true,
  testBig "big neg" (-3) true]
  where
    test8   name x r = primCase name DefMath.odd [int8 x] r
    test16  name x r = primCase name DefMath.odd [int16 x] r
    test32  name x r = primCase name DefMath.odd [int32 x] r
    test64  name x r = primCase name DefMath.odd [int64 x] r
    testU8  name x r = primCase name DefMath.odd [uint8 x] r
    testU16 name x r = primCase name DefMath.odd [uint16 x] r
    testU32 name x r = primCase name DefMath.odd [uint32 x] r
    testU64 name x r = primCase name DefMath.odd [uint64 x] r
    testBig name x r = primCase name DefMath.odd [bigint x] r

mathPi :: TypedTerm TestGroup
mathPi = subgroup "pi" [
  evalCase "pi constant"
    (Terms.primitive DefMath.roundFloat64 @@ int32 roundDigits @@ Terms.primitive DefMath.pi)
    (float64 (Math.roundFloat64 roundDigits pi))]

mathPow :: TypedTerm TestGroup
mathPow = subgroup "pow" [
  test "2^3" 2.0 3.0 8.0,
  test "10^0" 10.0 0.0 1.0,
  test "2^-1" 2.0 (-1.0) 0.5,
  roundedPrimCase2 "2^0.5" DefMath.pow 2.0 0.5 (2.0 ** 0.5),
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
    test name base exp result = primCase name DefMath.pow [float64 base, float64 exp] (float64 result)

mathRange :: TypedTerm TestGroup
mathRange = subgroup "range" [
  test "ascending range" 1 5 [1, 2, 3, 4, 5],
  test "single element" 5 5 [5],
  test "two elements" 3 4 [3, 4],
  test "negative start" (-2) 2 [(-2), (-1), 0, 1, 2]]
  where
    test name start end result = primCase name DefMath.range [int32 start, int32 end] (list $ int32 <$> result)

mathRound :: TypedTerm TestGroup
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
    test name x result = primCase name DefMath.round [float64 x] (float64 result)

mathRoundFloat32 :: TypedTerm TestGroup
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
    test name n x result = primCase name DefMath.roundFloat32 [int32 n, float32 x] (float32 result)

mathRoundFloat64 :: TypedTerm TestGroup
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
    test name n x result = primCase name DefMath.roundFloat64 [int32 n, float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §6 (numeric class, all 11 types),
-- machine-verified against an independent implementation (round4-317-matrix-attack.md §1).
-- The float rows are the design's §4.3 pin: a naive if x>0/elif x<0/else 0 implementation
-- fails signum(-0.0) and signum(NaN).
mathSignum :: TypedTerm TestGroup
mathSignum = subgroup "signum" [
  test8  "i8 min" (-128) (-1),
  test8  "i8 zero" 0 0,
  test8  "i8 max" 127 1,
  test16 "i16 min" (-32768) (-1),
  test16 "i16 zero" 0 0,
  test16 "i16 max" 32767 1,
  test32 "i32 min" (-2147483648) (-1),
  test32 "i32 zero" 0 0,
  test32 "i32 max" 2147483647 1,
  test64 "i64 min" (-9223372036854775808) (-1),
  test64 "i64 zero" 0 0,
  test64 "i64 max" 9223372036854775807 1,
  testU8  "u8 zero" 0 0,
  testU8  "u8 one" 1 1,
  testU8  "u8 max" 255 1,
  testU16 "u16 zero" 0 0,
  testU16 "u16 one" 1 1,
  testU16 "u16 max" 65535 1,
  testU32 "u32 zero" 0 0,
  testU32 "u32 one" 1 1,
  testU32 "u32 max" 4294967295 1,
  testU64 "u64 zero" 0 0,
  testU64 "u64 one" 1 1,
  testU64 "u64 max" 18446744073709551615 1,
  testBig "big neg" (-1267650600228229401496703205376) (-1),
  testBig "big zero" 0 0,
  testBig "big pos" 1267650600228229401496703205376 1,
  testF64 "f64 pos" 3.5 1.0,
  testF64 "f64 neg" (-3.5) (-1.0),
  testF64 "f64 poszero" 0.0 0.0,
  testF64 "f64 negzero" (-0.0) (-0.0),
  testF64 "f64 posinf" posInf64 1.0,
  testF64 "f64 neginf" negInf64 (-1.0),
  testF64 "f64 nan" nan64 nan64,
  testF32 "f32 pos" 3.5 1.0,
  testF32 "f32 neg" (-3.5) (-1.0),
  testF32 "f32 poszero" 0.0 0.0,
  testF32 "f32 negzero" (-0.0) (-0.0),
  testF32 "f32 posinf" posInf32 1.0,
  testF32 "f32 neginf" negInf32 (-1.0),
  testF32 "f32 nan" nan32 nan32]
  where
    test8   name x r = primCase name DefMath.signum [int8 x] (int8 r)
    test16  name x r = primCase name DefMath.signum [int16 x] (int16 r)
    test32  name x r = primCase name DefMath.signum [int32 x] (int32 r)
    test64  name x r = primCase name DefMath.signum [int64 x] (int64 r)
    testU8  name x r = primCase name DefMath.signum [uint8 x] (uint8 r)
    testU16 name x r = primCase name DefMath.signum [uint16 x] (uint16 r)
    testU32 name x r = primCase name DefMath.signum [uint32 x] (uint32 r)
    testU64 name x r = primCase name DefMath.signum [uint64 x] (uint64 r)
    testBig name x r = primCase name DefMath.signum [bigint x] (bigint r)
    testF64 name x r = primCase name DefMath.signum [float64 x] (float64 r)
    testF32 name x r = primCase name DefMath.signum [float32 x] (float32 r)

mathSin :: TypedTerm TestGroup
mathSin = subgroup "sin" [
  test "sin 0" 0.0 0.0,
  roundedPrimCase1 "sin pi/2" DefMath.sin (pi / 2) 1.0,
  roundedPrimCase1 "sin pi" DefMath.sin pi (sin pi),
  roundedPrimCase1 "sin 1" DefMath.sin 1.0 (sin 1.0),
  roundedPrimCase1 "sin 0.5" DefMath.sin 0.5 (sin 0.5),
  -- Special values
  test "sin NaN" nan64 nan64,
  test "sin +Inf" posInf64 nan64,
  test "sin -Inf" negInf64 nan64]
  where
    test name x result = primCase name DefMath.sin [float64 x] (float64 result)

mathSinh :: TypedTerm TestGroup
mathSinh = subgroup "sinh" [
  test "sinh 0" 0.0 0.0,
  roundedPrimCase1 "sinh 1" DefMath.sinh 1.0 (sinh 1.0),
  roundedPrimCase1 "sinh 2" DefMath.sinh 2.0 (sinh 2.0),
  -- Special values
  test "sinh NaN" nan64 nan64,
  test "sinh +Inf" posInf64 posInf64,
  test "sinh -Inf" negInf64 negInf64]
  where
    test name x result = primCase name DefMath.sinh [float64 x] (float64 result)

mathSqrt :: TypedTerm TestGroup
mathSqrt = subgroup "sqrt" [
  test "sqrt 4" 4.0 2.0,
  test "sqrt 9" 9.0 3.0,
  test "sqrt 2" 2.0 (sqrt 2.0),
  test "sqrt 0" 0.0 0.0,
  roundedPrimCase1 "sqrt 3" DefMath.sqrt 3.0 (sqrt 3.0),
  -- Out-of-domain: returns NaN (domain [0, inf))
  test "sqrt negative" (-1.0) nan64,
  -- Special values
  test "sqrt NaN" nan64 nan64,
  test "sqrt +Inf" posInf64 posInf64,
  test "sqrt -Inf" negInf64 nan64]
  where
    test name x result = primCase name DefMath.sqrt [float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §2 (numeric class, all 11 types),
-- machine-verified against an independent implementation (round4-317-matrix-attack.md §1).
mathSub :: TypedTerm TestGroup
mathSub = subgroup "sub" [
  test8  "i8 nominal" 10 3 7,
  test8  "i8 wrap min" (-128) 1 127,
  test8  "i8 neg min" 0 (-128) (-128),
  test8  "i8 wrap max" 127 (-1) (-128),
  test16 "i16 nominal" 10 3 7,
  test16 "i16 wrap min" (-32768) 1 32767,
  test16 "i16 neg min" 0 (-32768) (-32768),
  test16 "i16 wrap max" 32767 (-1) (-32768),
  test32 "i32 nominal" 10 3 7,
  test32 "i32 wrap min" (-2147483648) 1 2147483647,
  test32 "i32 neg min" 0 (-2147483648) (-2147483648),
  test32 "i32 wrap max" 2147483647 (-1) (-2147483648),
  test64 "i64 nominal" 10 3 7,
  test64 "i64 wrap min" (-9223372036854775808) 1 9223372036854775807,
  test64 "i64 neg min" 0 (-9223372036854775808) (-9223372036854775808),
  test64 "i64 wrap max" 9223372036854775807 (-1) (-9223372036854775808),
  testU8  "u8 nominal" 10 3 7,
  testU8  "u8 wrap zero" 0 1 255,
  testU8  "u8 wrap below" 3 5 254,
  testU16 "u16 nominal" 10 3 7,
  testU16 "u16 wrap zero" 0 1 65535,
  testU16 "u16 wrap below" 3 5 65534,
  testU32 "u32 nominal" 10 3 7,
  testU32 "u32 wrap zero" 0 1 4294967295,
  testU32 "u32 wrap below" 3 5 4294967294,
  testU64 "u64 nominal" 10 3 7,
  testU64 "u64 wrap zero" 0 1 18446744073709551615,
  testU64 "u64 wrap below" 3 5 18446744073709551614,
  testBig "big nominal" 10 3 7,
  testBig "big negative" 0 18446744073709551616 (-18446744073709551616),
  testBig "big large" 1267650600228229401496703205376 1 1267650600228229401496703205375,
  testF64 "f64 nominal" 5.5 2.25 3.25,
  testF64 "f64 inf-inf" posInf64 posInf64 nan64,
  testF64 "f64 x minus x" 1.0 1.0 0.0,
  testF64 "f64 negzero" (-0.0) 0.0 (-0.0),
  testF64 "f64 poszero" 0.0 (-0.0) 0.0,
  testF64 "f64 nan" nan64 nan64 nan64,
  testF32 "f32 nominal" 5.5 2.25 3.25,
  testF32 "f32 tie-even" 16777218.0 1.0 16777216.0,
  testF32 "f32 inf-inf" posInf32 posInf32 nan32,
  testF32 "f32 x minus x" 1.0 1.0 0.0]
  where
    test8   name x y r = primCase name DefMath.sub [int8 x, int8 y] (int8 r)
    test16  name x y r = primCase name DefMath.sub [int16 x, int16 y] (int16 r)
    test32  name x y r = primCase name DefMath.sub [int32 x, int32 y] (int32 r)
    test64  name x y r = primCase name DefMath.sub [int64 x, int64 y] (int64 r)
    testU8  name x y r = primCase name DefMath.sub [uint8 x, uint8 y] (uint8 r)
    testU16 name x y r = primCase name DefMath.sub [uint16 x, uint16 y] (uint16 r)
    testU32 name x y r = primCase name DefMath.sub [uint32 x, uint32 y] (uint32 r)
    testU64 name x y r = primCase name DefMath.sub [uint64 x, uint64 y] (uint64 r)
    testBig name x y r = primCase name DefMath.sub [bigint x, bigint y] (bigint r)
    testF64 name x y r = primCase name DefMath.sub [float64 x, float64 y] (float64 r)
    testF32 name x y r = primCase name DefMath.sub [float32 x, float32 y] (float32 r)

mathSubFloat64 :: TypedTerm TestGroup
mathSubFloat64 = subgroup "subFloat64" [
  test "positive numbers" 5.0 3.0 2.0,
  test "negative result" 3.0 5.0 (-2.0),
  test "negative numbers" (-3.0) (-5.0) 2.0,
  test "with zero" 42.0 0.0 42.0,
  test "same value" 42.0 42.0 0.0,
  test "fractional" 2.5 1.5 1.0]
  where
    test name x y result = primCase name DefMath.subFloat64 [float64 x, float64 y] (float64 result)

mathTan :: TypedTerm TestGroup
mathTan = subgroup "tan" [
  test "tan 0" 0.0 0.0,
  roundedPrimCase1 "tan pi/4" DefMath.tan (pi / 4) (tan (pi / 4)),
  roundedPrimCase1 "tan 1" DefMath.tan 1.0 (tan 1.0),
  roundedPrimCase1 "tan 0.5" DefMath.tan 0.5 (tan 0.5),
  -- Special values
  test "tan NaN" nan64 nan64,
  test "tan +Inf" posInf64 nan64,
  test "tan -Inf" negInf64 nan64]
  where
    test name x result = primCase name DefMath.tan [float64 x] (float64 result)

mathTanh :: TypedTerm TestGroup
mathTanh = subgroup "tanh" [
  test "tanh 0" 0.0 0.0,
  roundedPrimCase1 "tanh 1" DefMath.tanh 1.0 (tanh 1.0),
  roundedPrimCase1 "tanh 0.5" DefMath.tanh 0.5 (tanh 0.5),
  -- Special values: tanh's range is (-1, 1); saturates at infinities
  test "tanh NaN" nan64 nan64,
  test "tanh +Inf" posInf64 1.0,
  test "tanh -Inf" negInf64 (-1.0)]
  where
    test name x result = primCase name DefMath.tanh [float64 x] (float64 result)

mathTruncate :: TypedTerm TestGroup
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
    test name x result = primCase name DefMath.truncate [float64 x] (float64 result)

-- | Special float64 values: positive infinity, negative infinity, and NaN.
-- These are used to test that domain-restricted primitives return IEEE 754
-- special values (NaN/Inf) rather than throwing exceptions, and that all
-- float-accepting primitives propagate NaN/Inf correctly.
nan64 :: Double
nan64 = 0/0

negInf64 :: Double
negInf64 = -1/0

posInf64 :: Double
posInf64 = 1/0

-- | Special float32 values: positive infinity, negative infinity, and NaN.
nan32 :: Float
nan32 = 0/0

negInf32 :: Float
negInf32 = -1/0

posInf32 :: Float
posInf32 = 1/0

-- | Number of significant digits to use when rounding transcendental results
-- for platform-independent comparison. 12 digits is well within float64 precision
-- (which has ~15.9 significant digits) while safely absorbing 1-ULP differences.
roundDigits :: Int
roundDigits = 12

-- | Build a test case that rounds both the computed and expected results to
-- a fixed number of significant digits using roundFloat64. This makes
-- transcendental function tests portable across platforms with different libm
-- implementations.
roundedPrimCase1 :: ToPrimName n => String -> n -> Double -> Double -> TypedTerm TestCaseWithMetadata
roundedPrimCase1 cname prim x result = evalCase cname input output
  where
    input = Terms.primitive DefMath.roundFloat64 @@ int32 roundDigits @@ (Terms.primitive prim @@ float64 x)
    output = float64 (Math.roundFloat64 roundDigits result)

roundedPrimCase2 :: ToPrimName n => String -> n -> Double -> Double -> Double -> TypedTerm TestCaseWithMetadata
roundedPrimCase2 cname prim x y result = evalCase cname input output
  where
    input = Terms.primitive DefMath.roundFloat64 @@ int32 roundDigits @@ (Terms.primitive prim @@ float64 x @@ float64 y)
    output = float64 (Math.roundFloat64 roundDigits result)
