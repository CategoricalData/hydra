{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Lib.Math where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import Hydra.Core.Overlay.Haskell.Dsl.Terms (ToPrimName)
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Core.Testing
import qualified Hydra.Core.Overlay.Haskell.Lib.Math as Math
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Core.Lib.Math as DefMath


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.math"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core.reduction", ModuleName "hydra.core.print.model", ModuleName "hydra.core.model", ModuleName "hydra.core.errors", ModuleName "hydra.core.test.testGraph", ModuleName "hydra.core.testing"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.core.lib.math primitives")}
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

-- Test groups for hydra.core.lib.math primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.math primitives" $
    supergroup "hydra.core.lib.math primitives" [
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
mathAbs = supergroup "abs" [
  subgroup "preservesMagnitudeAcrossTypes" [
    test8  "i8 nominal" 5 5,
    test8  "i8 neg" (-1) 1,
    test16 "i16 nominal" 5 5,
    test16 "i16 neg" (-1) 1,
    test32 "i32 nominal" 5 5,
    test32 "i32 neg" (-1) 1,
    test64 "i64 nominal" 5 5,
    test64 "i64 neg" (-1) 1,
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
    testF32 "f32 nominal" (-1.5) 1.5],
  subgroup "boundaryAndSpecialValues" [
    test8  "i8 min" (-128) (-128),
    test8  "i8 min+1" (-127) 127,
    test16 "i16 min" (-32768) (-32768),
    test16 "i16 min+1" (-32767) 32767,
    test32 "i32 min" (-2147483648) (-2147483648),
    test32 "i32 min+1" (-2147483647) 2147483647,
    test64 "i64 min" (-9223372036854775808) (-9223372036854775808),
    test64 "i64 min+1" (-9223372036854775807) 9223372036854775807,
    testF64 "f64 negzero" (-0.0) 0.0,
    testF64 "f64 neginf" negInf64 posInf64,
    testF64 "f64 nan" nan64 nan64,
    testF64 "f64 subnormal" 5e-324 5e-324,
    testF32 "f32 negzero" (-0.0) 0.0,
    testF32 "f32 neginf" negInf32 posInf32,
    testF32 "f32 nan" nan32 nan32]]
  where
    test8   name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [int8 x] (int8 r)
    test16  name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [int16 x] (int16 r)
    test32  name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [int32 x] (int32 r)
    test64  name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [int64 x] (int64 r)
    testU8  name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [uint8 x] (uint8 r)
    testU16 name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [uint16 x] (uint16 r)
    testU32 name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [uint32 x] (uint32 r)
    testU64 name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [uint64 x] (uint64 r)
    testBig name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [bigint x] (bigint r)
    testF64 name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [float64 x] (float64 r)
    testF32 name x r = primCaseWithProvisions name [segmentOf name] DefMath.abs [float32 x] (float32 r)
    segmentOf n = if n `elem` ["i8 min", "i8 min+1", "i16 min", "i16 min+1", "i32 min", "i32 min+1",
                               "i64 min", "i64 min+1", "f64 negzero", "f64 neginf", "f64 nan", "f64 subnormal",
                               "f32 negzero", "f32 neginf", "f32 nan"]
      then "boundaryAndSpecialValues" else "preservesMagnitudeAcrossTypes"

mathAcos :: TypedTerm TestGroup
mathAcos = supergroup "acos" [
  subgroup "inverseCosineInPrincipalRange" [
    test "acos 1" "inverseCosineInPrincipalRange" 1.0 0.0,
    roundedPrimCase1WithProvisions "acos 0" ["inverseCosineInPrincipalRange"] DefMath.acos 0.0 (pi / 2),
    roundedPrimCase1WithProvisions "acos -1" ["inverseCosineInPrincipalRange"] DefMath.acos (-1.0) pi,
    roundedPrimCase1WithProvisions "acos 0.5" ["inverseCosineInPrincipalRange"] DefMath.acos 0.5 (acos 0.5)],
  subgroup "outOfDomainIsNanSpecialValuesPropagate" [
    test "acos below domain" "outOfDomainIsNanSpecialValuesPropagate" (-2.0) nan64,
    test "acos above domain" "outOfDomainIsNanSpecialValuesPropagate" 2.0 nan64,
    test "acos NaN" "outOfDomainIsNanSpecialValuesPropagate" nan64 nan64,
    test "acos +Inf" "outOfDomainIsNanSpecialValuesPropagate" posInf64 nan64,
    test "acos -Inf" "outOfDomainIsNanSpecialValuesPropagate" negInf64 nan64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.acos [float64 x] (float64 result)

mathAcosh :: TypedTerm TestGroup
mathAcosh = supergroup "acosh" [
  subgroup "inverseHyperbolicCosineInNonnegativeRange" [
    test "acosh 1" "inverseHyperbolicCosineInNonnegativeRange" 1.0 0.0,
    roundedPrimCase1WithProvisions "acosh 2" ["inverseHyperbolicCosineInNonnegativeRange"] DefMath.acosh 2.0 (acosh 2.0),
    roundedPrimCase1WithProvisions "acosh 3" ["inverseHyperbolicCosineInNonnegativeRange"] DefMath.acosh 3.0 (acosh 3.0)],
  subgroup "outOfDomainIsNanAsymmetricAtInfinity" [
    test "acosh below domain" "outOfDomainIsNanAsymmetricAtInfinity" 0.5 nan64,
    test "acosh negative" "outOfDomainIsNanAsymmetricAtInfinity" (-1.0) nan64,
    test "acosh NaN" "outOfDomainIsNanAsymmetricAtInfinity" nan64 nan64,
    test "acosh +Inf" "outOfDomainIsNanAsymmetricAtInfinity" posInf64 posInf64,
    test "acosh -Inf" "outOfDomainIsNanAsymmetricAtInfinity" negInf64 nan64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.acosh [float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §1 (numeric class, all 11 instance
-- types), machine-verified against an independent implementation (round4-317-matrix-attack.md
-- §1). Covers wraparound at both bounds of every fixed-width type, exact bigint arithmetic
-- beyond int64/uint64 range, and IEEE 754 special values (ties-to-even in both directions,
-- signed zeros, NaN/Inf propagation) at float64 and float32.
mathAdd :: TypedTerm TestGroup
mathAdd = supergroup "add" [
  subgroup "wrapsOrRoundsPerType" [
    test8  "i8 nominal" 3 4 7,
    test16 "i16 nominal" 3 4 7,
    test32 "i32 nominal" 3 4 7,
    test64 "i64 nominal" 3 4 7,
    testU8  "u8 nominal" 3 4 7,
    testU16 "u16 nominal" 3 4 7,
    testU32 "u32 nominal" 3 4 7,
    testU64 "u64 nominal" 3 4 7,
    testBig "big nominal" 3 4 7,
    testF64 "f64 nominal" 1.5 2.25 3.75,
    testF64 "f64 classic" 0.1 0.2 0.30000000000000004,
    testF32 "f32 nominal" 1.5 2.25 3.75,
    testF32 "f32 classic" 0.1 0.2 0.30000001192092896],
  subgroup "boundaryWraparoundAndIeeeSpecialValues" [
    test8  "i8 wrap max" 127 1 (-128),
    test8  "i8 wrap min" (-128) (-1) 127,
    test8  "i8 max+max" 127 127 (-2),
    test8  "i8 min+min" (-128) (-128) 0,
    test16 "i16 wrap max" 32767 1 (-32768),
    test16 "i16 wrap min" (-32768) (-1) 32767,
    test16 "i16 max+max" 32767 32767 (-2),
    test16 "i16 min+min" (-32768) (-32768) 0,
    test32 "i32 wrap max" 2147483647 1 (-2147483648),
    test32 "i32 wrap min" (-2147483648) (-1) 2147483647,
    test32 "i32 max+max" 2147483647 2147483647 (-2),
    test32 "i32 min+min" (-2147483648) (-2147483648) 0,
    test64 "i64 wrap max" 9223372036854775807 1 (-9223372036854775808),
    test64 "i64 wrap min" (-9223372036854775808) (-1) 9223372036854775807,
    test64 "i64 max+max" 9223372036854775807 9223372036854775807 (-2),
    test64 "i64 min+min" (-9223372036854775808) (-9223372036854775808) 0,
    testU8  "u8 wrap max" 255 1 0,
    testU8  "u8 max+max" 255 255 254,
    testU8  "u8 zero identity" 0 255 255,
    testU16 "u16 wrap max" 65535 1 0,
    testU16 "u16 max+max" 65535 65535 65534,
    testU16 "u16 zero identity" 0 65535 65535,
    testU32 "u32 wrap max" 4294967295 1 0,
    testU32 "u32 max+max" 4294967295 4294967295 4294967294,
    testU32 "u32 zero identity" 0 4294967295 4294967295,
    testU64 "u64 wrap max" 18446744073709551615 1 0,
    testU64 "u64 max+max" 18446744073709551615 18446744073709551615 18446744073709551614,
    testU64 "u64 zero identity" 0 18446744073709551615 18446744073709551615,
    testBig "big past i64" 9223372036854775807 1 9223372036854775808,
    testBig "big past u64" 18446744073709551615 1 18446744073709551616,
    testBig "big cancel" (-1267650600228229401496703205376) 1267650600228229401496703205376 0,
    testF64 "f64 tie-even down" 9007199254740992.0 1.0 9007199254740992.0,
    testF64 "f64 tie-even up" 9007199254740994.0 1.0 9007199254740996.0,
    testF64 "f64 overflow" 1.7976931348623157e308 1.7976931348623157e308 posInf64,
    testF64 "f64 inf+neginf" posInf64 negInf64 nan64,
    testF64 "f64 nan" nan64 1.0 nan64,
    testF64 "f64 zeros mixed" (-0.0) 0.0 0.0,
    testF64 "f64 zeros neg" (-0.0) (-0.0) (-0.0),
    testF32 "f32 tie-even" 16777216.0 1.0 16777216.0,
    testF32 "f32 exact step" 16777216.0 2.0 16777218.0,
    testF32 "f32 overflow" 3.4028235e38 3.4028235e38 posInf32,
    testF32 "f32 inf+neginf" posInf32 negInf32 nan32,
    testF32 "f32 zeros mixed" (-0.0) 0.0 0.0]]
  where
    test8   name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [int8 x, int8 y] (int8 r)
    test16  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [int16 x, int16 y] (int16 r)
    test32  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [int32 x, int32 y] (int32 r)
    test64  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [int64 x, int64 y] (int64 r)
    testU8  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [uint8 x, uint8 y] (uint8 r)
    testU16 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [uint16 x, uint16 y] (uint16 r)
    testU32 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [uint32 x, uint32 y] (uint32 r)
    testU64 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [uint64 x, uint64 y] (uint64 r)
    testBig name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [bigint x, bigint y] (bigint r)
    testF64 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [float64 x, float64 y] (float64 r)
    testF32 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.add [float32 x, float32 y] (float32 r)
    segmentOf n = if "nominal" `L.isSuffixOf` n || "classic" `L.isSuffixOf` n
      then "wrapsOrRoundsPerType" else "boundaryWraparoundAndIeeeSpecialValues"

mathAddFloat64 :: TypedTerm TestGroup
mathAddFloat64 = subgroup "addsFloat64Values" [
  test "positive numbers" "addsFloat64Values" 3.0 5.0 8.0,
  test "negative numbers" "addsFloat64Values" (-3.0) (-5.0) (-8.0),
  test "mixed sign" "addsFloat64Values" 10.0 (-3.0) 7.0,
  test "with zero" "addsFloat64Values" 42.0 0.0 42.0,
  test "fractional" "addsFloat64Values" 1.5 2.5 4.0]
  where
    test name segment x y result = primCaseWithProvisions name [segment] DefMath.addFloat64 [float64 x, float64 y] (float64 result)

mathAsin :: TypedTerm TestGroup
mathAsin = supergroup "asin" [
  subgroup "inverseSineInPrincipalRange" [
    test "asin 0" "inverseSineInPrincipalRange" 0.0 0.0,
    roundedPrimCase1WithProvisions "asin 1" ["inverseSineInPrincipalRange"] DefMath.asin 1.0 (pi / 2),
    roundedPrimCase1WithProvisions "asin -1" ["inverseSineInPrincipalRange"] DefMath.asin (-1.0) (-(pi / 2)),
    roundedPrimCase1WithProvisions "asin 0.5" ["inverseSineInPrincipalRange"] DefMath.asin 0.5 (asin 0.5)],
  subgroup "outOfDomainIsNanSpecialValuesPropagate" [
    test "asin below domain" "outOfDomainIsNanSpecialValuesPropagate" (-2.0) nan64,
    test "asin above domain" "outOfDomainIsNanSpecialValuesPropagate" 2.0 nan64,
    test "asin NaN" "outOfDomainIsNanSpecialValuesPropagate" nan64 nan64,
    test "asin +Inf" "outOfDomainIsNanSpecialValuesPropagate" posInf64 nan64,
    test "asin -Inf" "outOfDomainIsNanSpecialValuesPropagate" negInf64 nan64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.asin [float64 x] (float64 result)

mathAsinh :: TypedTerm TestGroup
mathAsinh = supergroup "asinh" [
  subgroup "inverseHyperbolicSineDefinedForAllFiniteReals" [
    test "asinh 0" "inverseHyperbolicSineDefinedForAllFiniteReals" 0.0 0.0,
    roundedPrimCase1WithProvisions "asinh 1" ["inverseHyperbolicSineDefinedForAllFiniteReals"] DefMath.asinh 1.0 (asinh 1.0),
    roundedPrimCase1WithProvisions "asinh 0.5" ["inverseHyperbolicSineDefinedForAllFiniteReals"] DefMath.asinh 0.5 (asinh 0.5)],
  subgroup "oddSymmetryAtInfinityNanPropagates" [
    test "asinh NaN" "oddSymmetryAtInfinityNanPropagates" nan64 nan64,
    test "asinh +Inf" "oddSymmetryAtInfinityNanPropagates" posInf64 posInf64,
    test "asinh -Inf" "oddSymmetryAtInfinityNanPropagates" negInf64 negInf64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.asinh [float64 x] (float64 result)

mathAtan :: TypedTerm TestGroup
mathAtan = supergroup "atan" [
  subgroup "inverseTangentInPrincipalRange" [
    test "atan 0" "inverseTangentInPrincipalRange" 0.0 0.0,
    roundedPrimCase1WithProvisions "atan 1" ["inverseTangentInPrincipalRange"] DefMath.atan 1.0 (pi / 4),
    roundedPrimCase1WithProvisions "atan 0.5" ["inverseTangentInPrincipalRange"] DefMath.atan 0.5 (atan 0.5)],
  subgroup "saturatesAtInfinityNanPropagates" [
    test "atan NaN" "saturatesAtInfinityNanPropagates" nan64 nan64,
    roundedPrimCase1WithProvisions "atan +Inf" ["saturatesAtInfinityNanPropagates"] DefMath.atan posInf64 (pi / 2),
    roundedPrimCase1WithProvisions "atan -Inf" ["saturatesAtInfinityNanPropagates"] DefMath.atan negInf64 (-(pi / 2))]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.atan [float64 x] (float64 result)

mathAtan2 :: TypedTerm TestGroup
mathAtan2 = supergroup "atan2" [
  subgroup "quadrantAwareArcTangentOfTwoArguments" [
    roundedPrimCase2WithProvisions "atan2 1 1" ["quadrantAwareArcTangentOfTwoArguments"] DefMath.atan2 1.0 1.0 (pi / 4),
    roundedPrimCase2WithProvisions "atan2 1 0" ["quadrantAwareArcTangentOfTwoArguments"] DefMath.atan2 1.0 0.0 (pi / 2),
    test "atan2 0 1" "quadrantAwareArcTangentOfTwoArguments" 0.0 1.0 0.0,
    roundedPrimCase2WithProvisions "atan2 3 4" ["quadrantAwareArcTangentOfTwoArguments"] DefMath.atan2 3.0 4.0 (atan2 3.0 4.0)],
  subgroup "ieeeSpecialValueMatrix" [
    test "atan2 NaN 1" "ieeeSpecialValueMatrix" nan64 1.0 nan64,
    roundedPrimCase2WithProvisions "atan2 +Inf 1" ["ieeeSpecialValueMatrix"] DefMath.atan2 posInf64 1.0 (pi / 2),
    roundedPrimCase2WithProvisions "atan2 -Inf 1" ["ieeeSpecialValueMatrix"] DefMath.atan2 negInf64 1.0 (-(pi / 2)),
    test "atan2 1 NaN" "ieeeSpecialValueMatrix" 1.0 nan64 nan64,
    test "atan2 1 +Inf" "ieeeSpecialValueMatrix" 1.0 posInf64 0.0,
    roundedPrimCase2WithProvisions "atan2 1 -Inf" ["ieeeSpecialValueMatrix"] DefMath.atan2 1.0 negInf64 pi,
    -- Cross-infinity combinations: Haskell returns NaN; other languages' native atan2
    -- returns ±pi/4 or ±3pi/4, so each implementation must special-case these.
    test "atan2 +Inf +Inf" "ieeeSpecialValueMatrix" posInf64 posInf64 nan64,
    test "atan2 +Inf -Inf" "ieeeSpecialValueMatrix" posInf64 negInf64 nan64,
    test "atan2 -Inf +Inf" "ieeeSpecialValueMatrix" negInf64 posInf64 nan64,
    test "atan2 -Inf -Inf" "ieeeSpecialValueMatrix" negInf64 negInf64 nan64]]
  where
    test name segment y x result = primCaseWithProvisions name [segment] DefMath.atan2 [float64 y, float64 x] (float64 result)

mathAtanh :: TypedTerm TestGroup
mathAtanh = supergroup "atanh" [
  subgroup "inverseHyperbolicTangentOnOpenInterval" [
    test "atanh 0" "inverseHyperbolicTangentOnOpenInterval" 0.0 0.0,
    roundedPrimCase1WithProvisions "atanh 0.5" ["inverseHyperbolicTangentOnOpenInterval"] DefMath.atanh 0.5 (atanh 0.5),
    roundedPrimCase1WithProvisions "atanh 0.1" ["inverseHyperbolicTangentOnOpenInterval"] DefMath.atanh 0.1 (atanh 0.1)],
  subgroup "boundaryIsInfiniteOutOfDomainIsNan" [
    test "atanh upper boundary" "boundaryIsInfiniteOutOfDomainIsNan" 1.0 posInf64,
    test "atanh lower boundary" "boundaryIsInfiniteOutOfDomainIsNan" (-1.0) negInf64,
    test "atanh above domain" "boundaryIsInfiniteOutOfDomainIsNan" 2.0 nan64,
    test "atanh below domain" "boundaryIsInfiniteOutOfDomainIsNan" (-2.0) nan64,
    test "atanh NaN" "boundaryIsInfiniteOutOfDomainIsNan" nan64 nan64,
    test "atanh +Inf" "boundaryIsInfiniteOutOfDomainIsNan" posInf64 nan64,
    test "atanh -Inf" "boundaryIsInfiniteOutOfDomainIsNan" negInf64 nan64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.atanh [float64 x] (float64 result)

mathCeiling :: TypedTerm TestGroup
mathCeiling = supergroup "ceiling" [
  subgroup "roundsTowardPositiveInfinityAsFloat" [
    test "ceiling 3.2" "roundsTowardPositiveInfinityAsFloat" 3.2 4.0,
    test "ceiling 3.0" "roundsTowardPositiveInfinityAsFloat" 3.0 3.0,
    test "ceiling -3.2" "roundsTowardPositiveInfinityAsFloat" (-3.2) (-3.0),
    test "ceiling -3.0" "roundsTowardPositiveInfinityAsFloat" (-3.0) (-3.0)],
  -- Special values propagate per IEEE 754 (see divergence comment on Math.ceiling)
  subgroup "ieeeSpecialValuesPassThrough" [
    test "ceiling NaN" "ieeeSpecialValuesPassThrough" nan64 nan64,
    test "ceiling +Inf" "ieeeSpecialValuesPassThrough" posInf64 posInf64,
    test "ceiling -Inf" "ieeeSpecialValuesPassThrough" negInf64 negInf64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.ceiling [float64 x] (float64 result)

mathCos :: TypedTerm TestGroup
mathCos = supergroup "cos" [
  subgroup "cosineOfAngleInRadians" [
    test "cos 0" "cosineOfAngleInRadians" 0.0 1.0,
    roundedPrimCase1WithProvisions "cos pi/2" ["cosineOfAngleInRadians"] DefMath.cos (pi / 2) (cos (pi / 2)),
    test "cos pi" "cosineOfAngleInRadians" pi (-1.0),
    roundedPrimCase1WithProvisions "cos 1" ["cosineOfAngleInRadians"] DefMath.cos 1.0 (cos 1.0),
    roundedPrimCase1WithProvisions "cos 0.5" ["cosineOfAngleInRadians"] DefMath.cos 0.5 (cos 0.5)],
  subgroup "undefinedAtInfinityNanPropagates" [
    test "cos NaN" "undefinedAtInfinityNanPropagates" nan64 nan64,
    test "cos +Inf" "undefinedAtInfinityNanPropagates" posInf64 nan64,
    test "cos -Inf" "undefinedAtInfinityNanPropagates" negInf64 nan64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.cos [float64 x] (float64 result)

mathCosh :: TypedTerm TestGroup
mathCosh = supergroup "cosh" [
  subgroup "hyperbolicCosineInRangeAtLeastOne" [
    test "cosh 0" "hyperbolicCosineInRangeAtLeastOne" 0.0 1.0,
    roundedPrimCase1WithProvisions "cosh 1" ["hyperbolicCosineInRangeAtLeastOne"] DefMath.cosh 1.0 (cosh 1.0),
    roundedPrimCase1WithProvisions "cosh 2" ["hyperbolicCosineInRangeAtLeastOne"] DefMath.cosh 2.0 (cosh 2.0)],
  subgroup "evenSymmetryAtInfinityNanPropagates" [
    test "cosh NaN" "evenSymmetryAtInfinityNanPropagates" nan64 nan64,
    test "cosh +Inf" "evenSymmetryAtInfinityNanPropagates" posInf64 posInf64,
    test "cosh -Inf" "evenSymmetryAtInfinityNanPropagates" negInf64 posInf64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.cosh [float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §12, machine-verified against an
-- independent IEEE-754 implementation (round4-317-matrix-attack.md §1). Includes the seven
-- -0.0-bearing rows originally withheld pending confirmation that the -0.0 literal round-trips
-- through code generation — CONFIRMED (see the plan doc's "-0.0 blocker resolved" note): the
-- JSON string sentinel survives the full DSL->JSON->Haskell pipeline intact.
mathDivide :: TypedTerm TestGroup
mathDivide = supergroup "divide" [
  subgroup "roundsToNearestPerIeee754" [
    test64 "f64 nominal" 7.0 2.0 3.5,
    test64 "f64 round" 1.0 3.0 (1.0 / 3.0),
    test64 "f64 overflow" 1e308 1e-10 posInf64,
    test64 "f64 subnormal" 2.2250738585072014e-308 2.0 1.1125369292536007e-308,
    test64 "f64 underflow" 5e-324 2.0 0.0,
    test32 "f32 nominal" 7.0 2.0 3.5,
    test32 "f32 round" 1.0 3.0 (1.0 / 3.0),
    test32 "f32 overflow" 3.4028235e38 1e-10 posInf32,
    test32 "f32 subnormal" 1.1754943508222875e-38 2.0 5.877471754111438e-39,
    test32 "f32 underflow" 1.401298464324817e-45 2.0 0.0],
  subgroup "ieeeSpecialValuesAndSignedZeroDivision" [
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
    test32 "f32 pos over pos zero" 1.0 0.0 posInf32,
    test32 "f32 neg over pos zero" (-1.0) 0.0 negInf32,
    test32 "f32 pos over neg zero" 1.0 (-0.0) negInf32,
    test32 "f32 zero over zero" 0.0 0.0 nan32,
    test32 "f32 inf over inf" posInf32 posInf32 nan32,
    test32 "f32 finite over inf" 1.0 posInf32 0.0]]
  where
    test64 name x y result = primCaseWithProvisions name [segmentOf name] DefMath.divide [float64 x, float64 y] (float64 result)
    test32 name x y result = primCaseWithProvisions name [segmentOf name] DefMath.divide [float32 x, float32 y] (float32 result)
    segmentOf n = if n `elem` ["f64 nominal", "f64 round", "f64 overflow", "f64 subnormal", "f64 underflow",
                               "f32 nominal", "f32 round", "f32 overflow", "f32 subnormal", "f32 underflow"]
      then "roundsToNearestPerIeee754" else "ieeeSpecialValuesAndSignedZeroDivision"

mathE :: TypedTerm TestGroup
mathE = subgroup "isEulersConstant" [
  evalCaseWithProvisions "Euler's number" ["isEulersConstant"] DefMath.e
    (Terms.primitive DefMath.roundFloat64 @@ int32 roundDigits @@ Terms.primitive DefMath.e)
    (float64 (Math.roundFloat64 roundDigits (exp 1)))]

-- | Cases sourced from round3-317-conformance-matrix.md §10 (integral class, all 9 int types).
mathEven :: TypedTerm TestGroup
mathEven = subgroup "testsDivisibilityByTwo" [
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
    test8   name x r = primCaseWithProvisions name ["testsDivisibilityByTwo"] DefMath.even [int8 x] r
    test16  name x r = primCaseWithProvisions name ["testsDivisibilityByTwo"] DefMath.even [int16 x] r
    test32  name x r = primCaseWithProvisions name ["testsDivisibilityByTwo"] DefMath.even [int32 x] r
    test64  name x r = primCaseWithProvisions name ["testsDivisibilityByTwo"] DefMath.even [int64 x] r
    testU8  name x r = primCaseWithProvisions name ["testsDivisibilityByTwo"] DefMath.even [uint8 x] r
    testU16 name x r = primCaseWithProvisions name ["testsDivisibilityByTwo"] DefMath.even [uint16 x] r
    testU32 name x r = primCaseWithProvisions name ["testsDivisibilityByTwo"] DefMath.even [uint32 x] r
    testU64 name x r = primCaseWithProvisions name ["testsDivisibilityByTwo"] DefMath.even [uint64 x] r
    testBig name x r = primCaseWithProvisions name ["testsDivisibilityByTwo"] DefMath.even [bigint x] r

mathExp :: TypedTerm TestGroup
mathExp = supergroup "exp" [
  subgroup "exponentialFunctionBaseE" [
    test "exp 0" "exponentialFunctionBaseE" 0.0 1.0,
    roundedPrimCase1WithProvisions "exp 1" ["exponentialFunctionBaseE"] DefMath.exp 1.0 (exp 1.0),
    roundedPrimCase1WithProvisions "exp -1" ["exponentialFunctionBaseE"] DefMath.exp (-1.0) (exp (-1.0)),
    roundedPrimCase1WithProvisions "exp 2" ["exponentialFunctionBaseE"] DefMath.exp 2.0 (exp 2.0),
    roundedPrimCase1WithProvisions "exp 0.5" ["exponentialFunctionBaseE"] DefMath.exp 0.5 (exp 0.5)],
  subgroup "saturatesAtInfinityNanPropagates" [
    test "exp NaN" "saturatesAtInfinityNanPropagates" nan64 nan64,
    test "exp +Inf" "saturatesAtInfinityNanPropagates" posInf64 posInf64,
    test "exp -Inf" "saturatesAtInfinityNanPropagates" negInf64 0.0]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.exp [float64 x] (float64 result)

mathFloor :: TypedTerm TestGroup
mathFloor = supergroup "floor" [
  subgroup "roundsTowardNegativeInfinityAsFloat" [
    test "floor 3.8" "roundsTowardNegativeInfinityAsFloat" 3.8 3.0,
    test "floor 3.0" "roundsTowardNegativeInfinityAsFloat" 3.0 3.0,
    test "floor -3.2" "roundsTowardNegativeInfinityAsFloat" (-3.2) (-4.0),
    test "floor -3.0" "roundsTowardNegativeInfinityAsFloat" (-3.0) (-3.0)],
  subgroup "ieeeSpecialValuesPassThrough" [
    test "floor NaN" "ieeeSpecialValuesPassThrough" nan64 nan64,
    test "floor +Inf" "ieeeSpecialValuesPassThrough" posInf64 posInf64,
    test "floor -Inf" "ieeeSpecialValuesPassThrough" negInf64 negInf64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.floor [float64 x] (float64 result)

mathLog :: TypedTerm TestGroup
mathLog = supergroup "log" [
  subgroup "naturalLogarithmOnPositiveDomain" [
    test "log 1" "naturalLogarithmOnPositiveDomain" 1.0 0.0,
    roundedPrimCase1WithProvisions "log e" ["naturalLogarithmOnPositiveDomain"] DefMath.log (exp 1.0) 1.0,
    roundedPrimCase1WithProvisions "log 2" ["naturalLogarithmOnPositiveDomain"] DefMath.log 2.0 (log 2.0),
    roundedPrimCase1WithProvisions "log 10" ["naturalLogarithmOnPositiveDomain"] DefMath.log 10.0 (log 10.0)],
  subgroup "boundaryIsNegativeInfinityNegativeIsNan" [
    test "log 0" "boundaryIsNegativeInfinityNegativeIsNan" 0.0 negInf64,
    test "log negative" "boundaryIsNegativeInfinityNegativeIsNan" (-1.0) nan64,
    test "log NaN" "boundaryIsNegativeInfinityNegativeIsNan" nan64 nan64,
    test "log +Inf" "boundaryIsNegativeInfinityNegativeIsNan" posInf64 posInf64,
    test "log -Inf" "boundaryIsNegativeInfinityNegativeIsNan" negInf64 nan64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.log [float64 x] (float64 result)

mathLogBase :: TypedTerm TestGroup
mathLogBase = supergroup "logBase" [
  subgroup "logarithmInArbitraryBase" [
    test "log10 1" "logarithmInArbitraryBase" 10.0 1.0 0.0,
    test "log10 10" "logarithmInArbitraryBase" 10.0 10.0 1.0,
    test "log10 100" "logarithmInArbitraryBase" 10.0 100.0 2.0,
    test "log2 8" "logarithmInArbitraryBase" 2.0 8.0 3.0,
    roundedPrimCase2WithProvisions "log2 10" ["logarithmInArbitraryBase"] DefMath.logBase 2.0 10.0 (logBase 2.0 10.0)],
  subgroup "inheritsLogsSpecialCaseBehaviorInEachArgument" [
    test "logBase 10 0" "inheritsLogsSpecialCaseBehaviorInEachArgument" 10.0 0.0 negInf64,
    test "logBase 10 negative" "inheritsLogsSpecialCaseBehaviorInEachArgument" 10.0 (-1.0) nan64,
    test "logBase negative 10" "inheritsLogsSpecialCaseBehaviorInEachArgument" (-1.0) 10.0 nan64,
    test "logBase 10 NaN" "inheritsLogsSpecialCaseBehaviorInEachArgument" 10.0 nan64 nan64,
    test "logBase 10 +Inf" "inheritsLogsSpecialCaseBehaviorInEachArgument" 10.0 posInf64 posInf64,
    test "logBase 10 -Inf" "inheritsLogsSpecialCaseBehaviorInEachArgument" 10.0 negInf64 nan64,
    test "logBase NaN 10" "inheritsLogsSpecialCaseBehaviorInEachArgument" nan64 10.0 nan64,
    test "logBase +Inf 10" "inheritsLogsSpecialCaseBehaviorInEachArgument" posInf64 10.0 0.0,
    test "logBase -Inf 10" "inheritsLogsSpecialCaseBehaviorInEachArgument" negInf64 10.0 nan64]]
  where
    test name segment base x result = primCaseWithProvisions name [segment] DefMath.logBase [float64 base, float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §7 (integral class, all 9 int types)
-- plus round4-317-matrix-attack.md §3.1's exact-negative-division addition (div(-6,2)=just -3,
-- the floor-adjust bug detector — no quadrant row above has a zero remainder to catch an
-- unconditional truncate-then-adjust bug). Machine-verified against an independent
-- implementation (round4 §1).
mathDiv :: TypedTerm TestGroup
mathDiv = supergroup "div" [
  subgroup "flooredQuotientWhenDivisorNonzero" [
    test8  "i8 pp" 7 2 (Just 3),
    test8  "i8 np" (-7) 2 (Just (-4)),
    test8  "i8 pn" 7 (-2) (Just (-4)),
    test8  "i8 nn" (-7) (-2) (Just 3),
    test8  "i8 np-exact" (-6) 2 (Just (-3)),
    test16 "i16 pp" 7 2 (Just 3),
    test16 "i16 np" (-7) 2 (Just (-4)),
    test16 "i16 pn" 7 (-2) (Just (-4)),
    test16 "i16 nn" (-7) (-2) (Just 3),
    test16 "i16 np-exact" (-6) 2 (Just (-3)),
    test32 "i32 pp" 7 2 (Just 3),
    test32 "i32 np" (-7) 2 (Just (-4)),
    test32 "i32 pn" 7 (-2) (Just (-4)),
    test32 "i32 nn" (-7) (-2) (Just 3),
    test32 "i32 np-exact" (-6) 2 (Just (-3)),
    test32 "i32 pn-exact" 6 (-2) (Just (-3)),
    test64 "i64 pp" 7 2 (Just 3),
    test64 "i64 np" (-7) 2 (Just (-4)),
    test64 "i64 pn" 7 (-2) (Just (-4)),
    test64 "i64 nn" (-7) (-2) (Just 3),
    test64 "i64 np-exact" (-6) 2 (Just (-3)),
    testU8  "u8 nominal" 7 2 (Just 3),
    testU8  "u8 max" 255 1 (Just 255),
    testU16 "u16 nominal" 7 2 (Just 3),
    testU16 "u16 max" 65535 1 (Just 65535),
    testU32 "u32 nominal" 7 2 (Just 3),
    testU32 "u32 max" 4294967295 1 (Just 4294967295),
    testU64 "u64 nominal" 7 2 (Just 3),
    testU64 "u64 max" 18446744073709551615 1 (Just 18446744073709551615),
    testBig "big pp" 7 2 (Just 3),
    testBig "big np" (-7) 2 (Just (-4)),
    testBig "big nn" (-7) (-2) (Just 3),
    testBig "big large" 1267650600228229401496703205376 1125899906842624 (Just 1125899906842624)],
  subgroup "noneOnZeroDivisorWraparoundAtMinBound" [
    test8  "i8 zero" 7 0 Nothing,
    test8  "i8 min-neg1" (-128) (-1) (Just (-128)),
    test16 "i16 zero" 7 0 Nothing,
    test16 "i16 min-neg1" (-32768) (-1) (Just (-32768)),
    test32 "i32 zero" 7 0 Nothing,
    test32 "i32 min-neg1" (-2147483648) (-1) (Just (-2147483648)),
    test64 "i64 zero" 7 0 Nothing,
    test64 "i64 min-neg1" (-9223372036854775808) (-1) (Just (-9223372036854775808)),
    testU8  "u8 zero" 7 0 Nothing,
    testU16 "u16 zero" 7 0 Nothing,
    testU32 "u32 zero" 7 0 Nothing,
    testU64 "u64 zero" 7 0 Nothing,
    testBig "big zero" 7 0 Nothing]]
  where
    test8   name x y r = primCaseWithProvisions name [segmentOf name] DefMath.div [int8 x, int8 y] (optionalOf int8 r)
    test16  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.div [int16 x, int16 y] (optionalOf int16 r)
    test32  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.div [int32 x, int32 y] (optionalOf int32 r)
    test64  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.div [int64 x, int64 y] (optionalOf int64 r)
    testU8  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.div [uint8 x, uint8 y] (optionalOf uint8 r)
    testU16 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.div [uint16 x, uint16 y] (optionalOf uint16 r)
    testU32 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.div [uint32 x, uint32 y] (optionalOf uint32 r)
    testU64 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.div [uint64 x, uint64 y] (optionalOf uint64 r)
    testBig name x y r = primCaseWithProvisions name [segmentOf name] DefMath.div [bigint x, bigint y] (optionalOf bigint r)
    segmentOf n = if "zero" `L.isSuffixOf` n || "min-neg1" `L.isSuffixOf` n
      then "noneOnZeroDivisorWraparoundAtMinBound" else "flooredQuotientWhenDivisorNonzero"


-- | Cases sourced from round3-317-conformance-matrix.md §8 (integral class, all 9 int types;
-- sign of result follows the divisor) plus round4-317-matrix-attack.md §3.1's exact-negative
-- addition (mod(-6,2)=just 0, the floor-adjust bug detector for mod).
mathMod :: TypedTerm TestGroup
mathMod = supergroup "mod" [
  subgroup "flooredRemainderSignFollowsDivisor" [
    test8  "i8 pp" 7 2 (Just 1),
    test8  "i8 np" (-7) 2 (Just 1),
    test8  "i8 pn" 7 (-2) (Just (-1)),
    test8  "i8 nn" (-7) (-2) (Just (-1)),
    test8  "i8 np-exact" (-6) 2 (Just 0),
    test16 "i16 pp" 7 2 (Just 1),
    test16 "i16 np" (-7) 2 (Just 1),
    test16 "i16 pn" 7 (-2) (Just (-1)),
    test16 "i16 nn" (-7) (-2) (Just (-1)),
    test16 "i16 np-exact" (-6) 2 (Just 0),
    test32 "i32 pp" 7 2 (Just 1),
    test32 "i32 np" (-7) 2 (Just 1),
    test32 "i32 pn" 7 (-2) (Just (-1)),
    test32 "i32 nn" (-7) (-2) (Just (-1)),
    test32 "i32 np-exact" (-6) 2 (Just 0),
    test64 "i64 pp" 7 2 (Just 1),
    test64 "i64 np" (-7) 2 (Just 1),
    test64 "i64 pn" 7 (-2) (Just (-1)),
    test64 "i64 nn" (-7) (-2) (Just (-1)),
    test64 "i64 np-exact" (-6) 2 (Just 0),
    testU8  "u8 nominal" 7 2 (Just 1),
    testU8  "u8 max" 255 2 (Just 1),
    testU16 "u16 nominal" 7 2 (Just 1),
    testU16 "u16 max" 65535 2 (Just 1),
    testU32 "u32 nominal" 7 2 (Just 1),
    testU32 "u32 max" 4294967295 2 (Just 1),
    testU64 "u64 nominal" 7 2 (Just 1),
    testU64 "u64 max" 18446744073709551615 2 (Just 1),
    testBig "big pp" 7 2 (Just 1),
    testBig "big np" (-7) 2 (Just 1),
    testBig "big pn" 7 (-2) (Just (-1)),
    testBig "big large" 1267650600228229401496703205381 1125899906842624 (Just 5)],
  subgroup "noneOnZeroDivisor" [
    test8  "i8 zero" 7 0 Nothing,
    test8  "i8 min-neg1" (-128) (-1) (Just 0),
    test16 "i16 zero" 7 0 Nothing,
    test16 "i16 min-neg1" (-32768) (-1) (Just 0),
    test32 "i32 zero" 7 0 Nothing,
    test32 "i32 min-neg1" (-2147483648) (-1) (Just 0),
    test64 "i64 zero" 7 0 Nothing,
    test64 "i64 min-neg1" (-9223372036854775808) (-1) (Just 0),
    testU8  "u8 zero" 7 0 Nothing,
    testU16 "u16 zero" 7 0 Nothing,
    testU32 "u32 zero" 7 0 Nothing,
    testU64 "u64 zero" 7 0 Nothing,
    testBig "big zero" 7 0 Nothing]]
  where
    test8   name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mod [int8 x, int8 y] (optionalOf int8 r)
    test16  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mod [int16 x, int16 y] (optionalOf int16 r)
    test32  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mod [int32 x, int32 y] (optionalOf int32 r)
    test64  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mod [int64 x, int64 y] (optionalOf int64 r)
    testU8  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mod [uint8 x, uint8 y] (optionalOf uint8 r)
    testU16 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mod [uint16 x, uint16 y] (optionalOf uint16 r)
    testU32 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mod [uint32 x, uint32 y] (optionalOf uint32 r)
    testU64 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mod [uint64 x, uint64 y] (optionalOf uint64 r)
    testBig name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mod [bigint x, bigint y] (optionalOf bigint r)
    segmentOf n = if "zero" `L.isSuffixOf` n || "min-neg1" `L.isSuffixOf` n
      then "noneOnZeroDivisor" else "flooredRemainderSignFollowsDivisor"


-- | Cases sourced from round3-317-conformance-matrix.md §9 (integral class; sign of result
-- follows the dividend — the §8/§9 pairs share inputs but differ in expected sign, catching
-- any host that wires both names to the same native operator) plus round4-317-matrix-attack.md
-- §3.1's exact-negative addition (rem(-6,2)=just 0).
mathRem :: TypedTerm TestGroup
mathRem = supergroup "rem" [
  subgroup "truncatedRemainderSignFollowsDividend" [
    test8  "i8 pp" 7 2 (Just 1),
    test8  "i8 np" (-7) 2 (Just (-1)),
    test8  "i8 pn" 7 (-2) (Just 1),
    test8  "i8 nn" (-7) (-2) (Just (-1)),
    test16 "i16 pp" 7 2 (Just 1),
    test16 "i16 np" (-7) 2 (Just (-1)),
    test16 "i16 pn" 7 (-2) (Just 1),
    test16 "i16 nn" (-7) (-2) (Just (-1)),
    test32 "i32 pp" 7 2 (Just 1),
    test32 "i32 np" (-7) 2 (Just (-1)),
    test32 "i32 pn" 7 (-2) (Just 1),
    test32 "i32 nn" (-7) (-2) (Just (-1)),
    test32 "i32 np-exact" (-6) 2 (Just 0),
    test64 "i64 pp" 7 2 (Just 1),
    test64 "i64 np" (-7) 2 (Just (-1)),
    test64 "i64 pn" 7 (-2) (Just 1),
    test64 "i64 nn" (-7) (-2) (Just (-1)),
    testU8  "u8 nominal" 7 2 (Just 1),
    testU16 "u16 nominal" 7 2 (Just 1),
    testU32 "u32 nominal" 7 2 (Just 1),
    testU64 "u64 nominal" 7 2 (Just 1),
    testBig "big pp" 7 2 (Just 1),
    testBig "big np" (-7) 2 (Just (-1)),
    testBig "big pn" 7 (-2) (Just 1),
    testBig "big nn" (-7) (-2) (Just (-1))],
  subgroup "noneOnZeroDivisorNoOverflowAtMinBound" [
    test8  "i8 zero" 7 0 Nothing,
    test8  "i8 min-neg1" (-128) (-1) (Just 0),
    test16 "i16 zero" 7 0 Nothing,
    test16 "i16 min-neg1" (-32768) (-1) (Just 0),
    test32 "i32 zero" 7 0 Nothing,
    test32 "i32 min-neg1" (-2147483648) (-1) (Just 0),
    test64 "i64 zero" 7 0 Nothing,
    test64 "i64 min-neg1" (-9223372036854775808) (-1) (Just 0),
    testU8  "u8 zero" 7 0 Nothing,
    testU16 "u16 zero" 7 0 Nothing,
    testU32 "u32 zero" 7 0 Nothing,
    testU64 "u64 zero" 7 0 Nothing,
    testBig "big zero" 7 0 Nothing]]
  where
    test8   name x y r = primCaseWithProvisions name [segmentOf name] DefMath.rem [int8 x, int8 y] (optionalOf int8 r)
    test16  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.rem [int16 x, int16 y] (optionalOf int16 r)
    test32  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.rem [int32 x, int32 y] (optionalOf int32 r)
    test64  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.rem [int64 x, int64 y] (optionalOf int64 r)
    testU8  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.rem [uint8 x, uint8 y] (optionalOf uint8 r)
    testU16 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.rem [uint16 x, uint16 y] (optionalOf uint16 r)
    testU32 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.rem [uint32 x, uint32 y] (optionalOf uint32 r)
    testU64 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.rem [uint64 x, uint64 y] (optionalOf uint64 r)
    testBig name x y r = primCaseWithProvisions name [segmentOf name] DefMath.rem [bigint x, bigint y] (optionalOf bigint r)
    segmentOf n = if "zero" `L.isSuffixOf` n || "min-neg1" `L.isSuffixOf` n
      then "noneOnZeroDivisorNoOverflowAtMinBound" else "truncatedRemainderSignFollowsDividend"


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
mathMul = supergroup "mul" [
  subgroup "wrapsOrRoundsPerType" [
    test8  "i8 nominal" 6 7 42,
    test16 "i16 nominal" 6 7 42,
    test32 "i32 nominal" 6 7 42,
    test64 "i64 nominal" 6 7 42,
    testU8  "u8 nominal" 6 7 42,
    testU16 "u16 nominal" 6 7 42,
    testU32 "u32 nominal" 6 7 42,
    testU64 "u64 nominal" 6 7 42,
    testBig "big nominal" 6 7 42,
    testF64 "f64 nominal" 1.5 2.0 3.0,
    testF64 "f64 round" 0.1 0.1 0.010000000000000002,
    testF32 "f32 nominal" 1.5 2.0 3.0],
  subgroup "boundaryWraparoundAndIeeeSpecialValues" [
    test8  "i8 min*neg1" (-128) (-1) (-128),
    test8  "i8 max*2" 127 2 (-2),
    test8  "i8 min*2" (-128) 2 0,
    test8  "i8 max*max" 127 127 1,
    test16 "i16 min*neg1" (-32768) (-1) (-32768),
    test16 "i16 max*2" 32767 2 (-2),
    test16 "i16 min*2" (-32768) 2 0,
    test16 "i16 max*max" 32767 32767 1,
    test32 "i32 min*neg1" (-2147483648) (-1) (-2147483648),
    test32 "i32 max*2" 2147483647 2 (-2),
    test32 "i32 min*2" (-2147483648) 2 0,
    test32 "i32 max*max" 2147483647 2147483647 1,
    test64 "i64 min*neg1" (-9223372036854775808) (-1) (-9223372036854775808),
    test64 "i64 max*2" 9223372036854775807 2 (-2),
    test64 "i64 min*2" (-9223372036854775808) 2 0,
    test64 "i64 max*max" 9223372036854775807 9223372036854775807 1,
    testU8  "u8 max*2" 255 2 254,
    testU8  "u8 max*max" 255 255 1,
    testU8  "u8 half*half" 16 16 0,
    testU16 "u16 max*2" 65535 2 65534,
    testU16 "u16 max*max" 65535 65535 1,
    testU16 "u16 half*half" 256 256 0,
    testU32 "u32 max*2" 4294967295 2 4294967294,
    testU32 "u32 max*max" 4294967295 4294967295 1,
    testU32 "u32 half*half" 65536 65536 0,
    testU64 "u64 max*2" 18446744073709551615 2 18446744073709551614,
    testU64 "u64 max*max" 18446744073709551615 18446744073709551615 1,
    testU64 "u64 half*half" 4294967296 4294967296 0,
    testBig "big exact 2^64" 4294967296 4294967296 18446744073709551616,
    testBig "big neg large" (-1125899906842624) 1125899906842624 (-1267650600228229401496703205376),
    testF64 "f64 overflow" 1e200 1e200 posInf64,
    testF64 "f64 underflow" 1e-200 1e-200 0.0,
    testF64 "f64 underflow neg" (-1e-200) 1e-200 (-0.0),
    testF64 "f64 inf*zero" posInf64 0.0 nan64,
    testF64 "f64 negzero" (-1.0) 0.0 (-0.0),
    testF64 "f64 nan" nan64 0.0 nan64,
    testF32 "f32 round down" 16777215.0 16777215.0 281474943156224.0,
    testF32 "f32 overflow" 1e20 1e20 posInf32,
    testF32 "f32 underflow" 1e-30 1e-30 0.0,
    testF32 "f32 negzero" (-1.0) 0.0 (-0.0)]]
  where
    test8   name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [int8 x, int8 y] (int8 r)
    test16  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [int16 x, int16 y] (int16 r)
    test32  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [int32 x, int32 y] (int32 r)
    test64  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [int64 x, int64 y] (int64 r)
    testU8  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [uint8 x, uint8 y] (uint8 r)
    testU16 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [uint16 x, uint16 y] (uint16 r)
    testU32 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [uint32 x, uint32 y] (uint32 r)
    testU64 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [uint64 x, uint64 y] (uint64 r)
    testBig name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [bigint x, bigint y] (bigint r)
    testF64 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [float64 x, float64 y] (float64 r)
    testF32 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.mul [float32 x, float32 y] (float32 r)
    segmentOf n = if "nominal" `L.isSuffixOf` n || "round" `L.isSuffixOf` n
      then "wrapsOrRoundsPerType" else "boundaryWraparoundAndIeeeSpecialValues"

mathMulFloat64 :: TypedTerm TestGroup
mathMulFloat64 = subgroup "multipliesFloat64Values" [
  test "positive numbers" "multipliesFloat64Values" 3.0 5.0 15.0,
  test "negative numbers" "multipliesFloat64Values" (-3.0) (-5.0) 15.0,
  test "mixed sign" "multipliesFloat64Values" 10.0 (-3.0) (-30.0),
  test "with zero" "multipliesFloat64Values" 42.0 0.0 0.0,
  test "with one" "multipliesFloat64Values" 42.0 1.0 42.0,
  test "fractional" "multipliesFloat64Values" 1.5 2.0 3.0]
  where
    test name segment x y result = primCaseWithProvisions name [segment] DefMath.mulFloat64 [float64 x, float64 y] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §4 (numeric class, all 11 types),
-- machine-verified against an independent implementation (round4-317-matrix-attack.md §1).
mathNegate :: TypedTerm TestGroup
mathNegate = supergroup "negate" [
  subgroup "arithmeticNegationPerType" [
    test8  "i8 nominal" 5 (-5),
    test16 "i16 nominal" 5 (-5),
    test32 "i32 nominal" 5 (-5),
    test64 "i64 nominal" 5 (-5),
    testU8  "u8 zero" 0 0,
    testU8  "u8 one" 1 255,
    testU8  "u8 nominal" 100 156,
    testU16 "u16 zero" 0 0,
    testU16 "u16 one" 1 65535,
    testU16 "u16 nominal" 100 65436,
    testU32 "u32 zero" 0 0,
    testU32 "u32 one" 1 4294967295,
    testU32 "u32 nominal" 100 4294967196,
    testU64 "u64 zero" 0 0,
    testU64 "u64 one" 1 18446744073709551615,
    testU64 "u64 nominal" 100 18446744073709551516,
    testBig "big nominal" (-5) 5,
    testF64 "f64 nominal" 1.5 (-1.5),
    testF32 "f32 nominal" 1.5 (-1.5)],
  subgroup "boundaryAndIeeeSpecialValues" [
    test8  "i8 min" (-128) (-128),
    test8  "i8 max" 127 (-127),
    test16 "i16 min" (-32768) (-32768),
    test16 "i16 max" 32767 (-32767),
    test32 "i32 min" (-2147483648) (-2147483648),
    test32 "i32 max" 2147483647 (-2147483647),
    test64 "i64 min" (-9223372036854775808) (-9223372036854775808),
    test64 "i64 max" 9223372036854775807 (-9223372036854775807),
    testU8  "u8 max" 255 1,
    testU16 "u16 max" 65535 1,
    testU32 "u32 max" 4294967295 1,
    testU64 "u64 max" 18446744073709551615 1,
    testBig "big large" 1267650600228229401496703205376 (-1267650600228229401496703205376),
    testF64 "f64 poszero" 0.0 (-0.0),
    testF64 "f64 negzero" (-0.0) 0.0,
    testF64 "f64 inf" posInf64 negInf64,
    testF64 "f64 nan" nan64 nan64,
    testF32 "f32 poszero" 0.0 (-0.0),
    testF32 "f32 negzero" (-0.0) 0.0,
    testF32 "f32 neginf" negInf32 posInf32]]
  where
    test8   name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [int8 x] (int8 r)
    test16  name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [int16 x] (int16 r)
    test32  name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [int32 x] (int32 r)
    test64  name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [int64 x] (int64 r)
    testU8  name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [uint8 x] (uint8 r)
    testU16 name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [uint16 x] (uint16 r)
    testU32 name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [uint32 x] (uint32 r)
    testU64 name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [uint64 x] (uint64 r)
    testBig name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [bigint x] (bigint r)
    testF64 name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [float64 x] (float64 r)
    testF32 name x r = primCaseWithProvisions name [segmentOf name] DefMath.negate [float32 x] (float32 r)
    segmentOf n = if n `elem` ["i8 min", "i8 max", "i16 min", "i16 max", "i32 min", "i32 max", "i64 min", "i64 max",
                               "u8 max", "u16 max", "u32 max", "u64 max", "big large",
                               "f64 poszero", "f64 negzero", "f64 inf", "f64 nan",
                               "f32 poszero", "f32 negzero", "f32 neginf"]
      then "boundaryAndIeeeSpecialValues" else "arithmeticNegationPerType"

mathNegateFloat64 :: TypedTerm TestGroup
mathNegateFloat64 = subgroup "negatesFloat64Values" [
  test "positive" "negatesFloat64Values" 5.0 (-5.0),
  test "negative" "negatesFloat64Values" (-5.0) 5.0,
  test "zero" "negatesFloat64Values" 0.0 (-0.0),
  test "fractional" "negatesFloat64Values" 1.5 (-1.5)]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.negateFloat64 [float64 x] (float64 result)

-- | Value-level polymorphic dispatch of the 'numeric'-constrained primitives add/sub/mul/negate
--   on numeric types other than int32. The int32 cases are already covered by mathAdd/mathSub/
--   mathMul/mathNegate above; these exercise the runtime dispatch on float64, int64, and bigint
--   literals, confirming that the single polymorphic primitive computes with the correct per-type
--   semantics (#566).
mathNumericDispatch :: TypedTerm TestGroup
mathNumericDispatch = supergroup "polymorphic numeric dispatch" [
  subgroup "add" [
    primCaseWithProvisions "float64" ["wrapsOrRoundsPerType"] DefMath.add [float64 1.5, float64 2.5] (float64 4.0),
    primCaseWithProvisions "int64"   ["wrapsOrRoundsPerType"] DefMath.add [int64 1000000000000, int64 1] (int64 1000000000001),
    primCaseWithProvisions "bigint"  ["wrapsOrRoundsPerType"] DefMath.add [bigint 100000000000000000000, bigint 1] (bigint 100000000000000000001)],
  subgroup "sub" [
    primCaseWithProvisions "float64" ["wrapsOrRoundsPerType"] DefMath.sub [float64 5.0, float64 1.5] (float64 3.5),
    primCaseWithProvisions "int64"   ["wrapsOrRoundsPerType"] DefMath.sub [int64 1000000000001, int64 1] (int64 1000000000000),
    primCaseWithProvisions "bigint"  ["wrapsOrRoundsPerType"] DefMath.sub [bigint 100000000000000000001, bigint 1] (bigint 100000000000000000000)],
  subgroup "mul" [
    primCaseWithProvisions "float64" ["wrapsOrRoundsPerType"] DefMath.mul [float64 2.0, float64 2.5] (float64 5.0),
    primCaseWithProvisions "int64"   ["wrapsOrRoundsPerType"] DefMath.mul [int64 1000000, int64 1000000] (int64 1000000000000),
    primCaseWithProvisions "bigint"  ["wrapsOrRoundsPerType"] DefMath.mul [bigint 10000000000, bigint 10000000000] (bigint 100000000000000000000)],
  subgroup "negate" [
    primCaseWithProvisions "float64" ["arithmeticNegationPerType"] DefMath.negate [float64 2.5] (float64 (-2.5)),
    primCaseWithProvisions "int64"   ["arithmeticNegationPerType"] DefMath.negate [int64 1000000000000] (int64 (-1000000000000)),
    primCaseWithProvisions "bigint"  ["arithmeticNegationPerType"] DefMath.negate [bigint 100000000000000000000] (bigint (-100000000000000000000))]]

-- | Cases sourced from round3-317-conformance-matrix.md §11 (integral class, all 9 int types).
mathOdd :: TypedTerm TestGroup
mathOdd = subgroup "testsNonDivisibilityByTwo" [
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
    test8   name x r = primCaseWithProvisions name ["testsNonDivisibilityByTwo"] DefMath.odd [int8 x] r
    test16  name x r = primCaseWithProvisions name ["testsNonDivisibilityByTwo"] DefMath.odd [int16 x] r
    test32  name x r = primCaseWithProvisions name ["testsNonDivisibilityByTwo"] DefMath.odd [int32 x] r
    test64  name x r = primCaseWithProvisions name ["testsNonDivisibilityByTwo"] DefMath.odd [int64 x] r
    testU8  name x r = primCaseWithProvisions name ["testsNonDivisibilityByTwo"] DefMath.odd [uint8 x] r
    testU16 name x r = primCaseWithProvisions name ["testsNonDivisibilityByTwo"] DefMath.odd [uint16 x] r
    testU32 name x r = primCaseWithProvisions name ["testsNonDivisibilityByTwo"] DefMath.odd [uint32 x] r
    testU64 name x r = primCaseWithProvisions name ["testsNonDivisibilityByTwo"] DefMath.odd [uint64 x] r
    testBig name x r = primCaseWithProvisions name ["testsNonDivisibilityByTwo"] DefMath.odd [bigint x] r

mathPi :: TypedTerm TestGroup
mathPi = subgroup "isPiConstant" [
  evalCaseWithProvisions "pi constant" ["isPiConstant"] DefMath.pi
    (Terms.primitive DefMath.roundFloat64 @@ int32 roundDigits @@ Terms.primitive DefMath.pi)
    (float64 (Math.roundFloat64 roundDigits pi))]

mathPow :: TypedTerm TestGroup
mathPow = supergroup "pow" [
  subgroup "exponentiationForFiniteArguments" [
    test "2^3" "exponentiationForFiniteArguments" 2.0 3.0 8.0,
    test "10^0" "exponentiationForFiniteArguments" 10.0 0.0 1.0,
    test "2^-1" "exponentiationForFiniteArguments" 2.0 (-1.0) 0.5,
    roundedPrimCase2WithProvisions "2^0.5" ["exponentiationForFiniteArguments"] DefMath.pow 2.0 0.5 (2.0 ** 0.5)],
  subgroup "ieeeSpecialValueMatrix" [
    test "0^0" "ieeeSpecialValueMatrix" 0.0 0.0 1.0,
    test "0^-1" "ieeeSpecialValueMatrix" 0.0 (-1.0) posInf64,
    test "(-1)^0.5" "ieeeSpecialValueMatrix" (-1.0) 0.5 nan64,
    test "NaN^2" "ieeeSpecialValueMatrix" nan64 2.0 nan64,
    test "+Inf^2" "ieeeSpecialValueMatrix" posInf64 2.0 posInf64,
    test "-Inf^2" "ieeeSpecialValueMatrix" negInf64 2.0 posInf64,
    test "+Inf^-1" "ieeeSpecialValueMatrix" posInf64 (-1.0) 0.0,
    test "2^NaN" "ieeeSpecialValueMatrix" 2.0 nan64 nan64,
    test "2^+Inf" "ieeeSpecialValueMatrix" 2.0 posInf64 posInf64,
    test "2^-Inf" "ieeeSpecialValueMatrix" 2.0 negInf64 0.0]]
  where
    test name segment base exp result = primCaseWithProvisions name [segment] DefMath.pow [float64 base, float64 exp] (float64 result)

mathRange :: TypedTerm TestGroup
mathRange = supergroup "range" [
  subgroup "generatesHalfOpenAscendingSequence" [
    test "ascending range" "generatesHalfOpenAscendingSequence" 1 6 [1, 2, 3, 4, 5],
    test "single element" "generatesHalfOpenAscendingSequence" 5 6 [5],
    test "two elements" "generatesHalfOpenAscendingSequence" 3 5 [3, 4],
    test "negative start" "generatesHalfOpenAscendingSequence" (-2) 3 [(-2), (-1), 0, 1, 2],
    test "produce n items idiom, 0-indexed" "generatesHalfOpenAscendingSequence" 0 5 [0, 1, 2, 3, 4],
    test "produce n items idiom, 1-indexed values preserved" "generatesHalfOpenAscendingSequence" 1 6 [1, 2, 3, 4, 5]],
  subgroup "emptyWhenBoundsEqualOrDescending" [
    test "empty range, equal bounds" "emptyWhenBoundsEqualOrDescending" 5 5 [],
    test "empty range, descending bounds" "emptyWhenBoundsEqualOrDescending" 5 3 []]]
  where
    test name segment start end result = primCaseWithProvisions name [segment] DefMath.range [int32 start, int32 end] (list $ int32 <$> result)

mathRound :: TypedTerm TestGroup
mathRound = supergroup "round" [
  subgroup "roundsToNearestTiesToEvenAsFloat" [
    test "round 3.4" "roundsToNearestTiesToEvenAsFloat" 3.4 3.0,
    test "round 3.5" "roundsToNearestTiesToEvenAsFloat" 3.5 4.0,
    test "round 3.6" "roundsToNearestTiesToEvenAsFloat" 3.6 4.0,
    test "round -3.4" "roundsToNearestTiesToEvenAsFloat" (-3.4) (-3.0),
    test "round -3.5" "roundsToNearestTiesToEvenAsFloat" (-3.5) (-4.0)],
  subgroup "ieeeSpecialValuesPassThrough" [
    test "round NaN" "ieeeSpecialValuesPassThrough" nan64 nan64,
    test "round +Inf" "ieeeSpecialValuesPassThrough" posInf64 posInf64,
    test "round -Inf" "ieeeSpecialValuesPassThrough" negInf64 negInf64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.round [float64 x] (float64 result)

mathRoundFloat32 :: TypedTerm TestGroup
mathRoundFloat32 = supergroup "roundFloat32" [
  subgroup "roundsToGivenDecimalPlacesHalfToEven" [
    test "zero" "roundsToGivenDecimalPlacesHalfToEven" 5 0.0 0.0,
    test "round pi to 4 digits" "roundsToGivenDecimalPlacesHalfToEven" 4 3.1415927 3.142,
    test "round 1234.5 to 3 digits" "roundsToGivenDecimalPlacesHalfToEven" 3 1234.5 1230.0,
    test "negative" "roundsToGivenDecimalPlacesHalfToEven" 3 (-1234.5) (-1230.0)],
  subgroup "specialValuesPassThroughUnrounded" [
    test "NaN" "specialValuesPassThroughUnrounded" 3 (0/0) (0/0),
    test "+Inf" "specialValuesPassThroughUnrounded" 3 (1/0) (1/0),
    test "-Inf" "specialValuesPassThroughUnrounded" 3 (-1/0) (-1/0)]]
  where
    test name segment n x result = primCaseWithProvisions name [segment] DefMath.roundFloat32 [int32 n, float32 x] (float32 result)

mathRoundFloat64 :: TypedTerm TestGroup
mathRoundFloat64 = supergroup "roundFloat64" [
  subgroup "roundsToGivenDecimalPlacesHalfToEven" [
    test "zero" "roundsToGivenDecimalPlacesHalfToEven" 5 0.0 0.0,
    test "round pi to 4 digits" "roundsToGivenDecimalPlacesHalfToEven" 4 3.141592653589793 3.142,
    test "round pi to 10 digits" "roundsToGivenDecimalPlacesHalfToEven" 10 3.141592653589793 3.141592654,
    test "round 1234.5 to 3 digits" "roundsToGivenDecimalPlacesHalfToEven" 3 1234.5 1230.0,
    test "round 0.001234 to 2 digits" "roundsToGivenDecimalPlacesHalfToEven" 2 0.001234 0.0012,
    test "negative" "roundsToGivenDecimalPlacesHalfToEven" 3 (-1234.5) (-1230.0),
    test "round 1 digit" "roundsToGivenDecimalPlacesHalfToEven" 1 9.876 10.0],
  subgroup "specialValuesPassThroughUnrounded" [
    test "NaN" "specialValuesPassThroughUnrounded" 3 nan64 nan64,
    test "+Inf" "specialValuesPassThroughUnrounded" 3 posInf64 posInf64,
    test "-Inf" "specialValuesPassThroughUnrounded" 3 negInf64 negInf64]]
  where
    test name segment n x result = primCaseWithProvisions name [segment] DefMath.roundFloat64 [int32 n, float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §6 (numeric class, all 11 types),
-- machine-verified against an independent implementation (round4-317-matrix-attack.md §1).
-- The float rows are the design's §4.3 pin: a naive if x>0/elif x<0/else 0 implementation
-- fails signum(-0.0) and signum(NaN).
mathSignum :: TypedTerm TestGroup
mathSignum = supergroup "signum" [
  subgroup "signPerType" [
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
    testF64 "f64 posinf" posInf64 1.0,
    testF64 "f64 neginf" negInf64 (-1.0),
    testF32 "f32 pos" 3.5 1.0,
    testF32 "f32 neg" (-3.5) (-1.0),
    testF32 "f32 posinf" posInf32 1.0,
    testF32 "f32 neginf" negInf32 (-1.0)],
  subgroup "preservesSignedZeroAndNan" [
    testF64 "f64 poszero" 0.0 0.0,
    testF64 "f64 negzero" (-0.0) (-0.0),
    testF64 "f64 nan" nan64 nan64,
    testF32 "f32 poszero" 0.0 0.0,
    testF32 "f32 negzero" (-0.0) (-0.0),
    testF32 "f32 nan" nan32 nan32]]
  where
    test8   name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [int8 x] (int8 r)
    test16  name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [int16 x] (int16 r)
    test32  name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [int32 x] (int32 r)
    test64  name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [int64 x] (int64 r)
    testU8  name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [uint8 x] (uint8 r)
    testU16 name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [uint16 x] (uint16 r)
    testU32 name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [uint32 x] (uint32 r)
    testU64 name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [uint64 x] (uint64 r)
    testBig name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [bigint x] (bigint r)
    testF64 name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [float64 x] (float64 r)
    testF32 name x r = primCaseWithProvisions name [segmentOf name] DefMath.signum [float32 x] (float32 r)
    segmentOf n = if n `elem` ["f64 poszero", "f64 negzero", "f64 nan", "f32 poszero", "f32 negzero", "f32 nan"]
      then "preservesSignedZeroAndNan" else "signPerType"

mathSin :: TypedTerm TestGroup
mathSin = supergroup "sin" [
  subgroup "sineOfAngleInRadians" [
    test "sin 0" "sineOfAngleInRadians" 0.0 0.0,
    roundedPrimCase1WithProvisions "sin pi/2" ["sineOfAngleInRadians"] DefMath.sin (pi / 2) 1.0,
    roundedPrimCase1WithProvisions "sin pi" ["sineOfAngleInRadians"] DefMath.sin pi (sin pi),
    roundedPrimCase1WithProvisions "sin 1" ["sineOfAngleInRadians"] DefMath.sin 1.0 (sin 1.0),
    roundedPrimCase1WithProvisions "sin 0.5" ["sineOfAngleInRadians"] DefMath.sin 0.5 (sin 0.5)],
  subgroup "undefinedAtInfinityNanPropagates" [
    test "sin NaN" "undefinedAtInfinityNanPropagates" nan64 nan64,
    test "sin +Inf" "undefinedAtInfinityNanPropagates" posInf64 nan64,
    test "sin -Inf" "undefinedAtInfinityNanPropagates" negInf64 nan64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.sin [float64 x] (float64 result)

mathSinh :: TypedTerm TestGroup
mathSinh = supergroup "sinh" [
  subgroup "hyperbolicSineDefinedForAllFiniteReals" [
    test "sinh 0" "hyperbolicSineDefinedForAllFiniteReals" 0.0 0.0,
    roundedPrimCase1WithProvisions "sinh 1" ["hyperbolicSineDefinedForAllFiniteReals"] DefMath.sinh 1.0 (sinh 1.0),
    roundedPrimCase1WithProvisions "sinh 2" ["hyperbolicSineDefinedForAllFiniteReals"] DefMath.sinh 2.0 (sinh 2.0)],
  subgroup "oddSymmetryAtInfinityNanPropagates" [
    test "sinh NaN" "oddSymmetryAtInfinityNanPropagates" nan64 nan64,
    test "sinh +Inf" "oddSymmetryAtInfinityNanPropagates" posInf64 posInf64,
    test "sinh -Inf" "oddSymmetryAtInfinityNanPropagates" negInf64 negInf64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.sinh [float64 x] (float64 result)

mathSqrt :: TypedTerm TestGroup
mathSqrt = supergroup "sqrt" [
  subgroup "nonnegativeSquareRootOnNonnegativeDomain" [
    test "sqrt 4" "nonnegativeSquareRootOnNonnegativeDomain" 4.0 2.0,
    test "sqrt 9" "nonnegativeSquareRootOnNonnegativeDomain" 9.0 3.0,
    test "sqrt 2" "nonnegativeSquareRootOnNonnegativeDomain" 2.0 (sqrt 2.0),
    test "sqrt 0" "nonnegativeSquareRootOnNonnegativeDomain" 0.0 0.0,
    roundedPrimCase1WithProvisions "sqrt 3" ["nonnegativeSquareRootOnNonnegativeDomain"] DefMath.sqrt 3.0 (sqrt 3.0)],
  subgroup "preservesSignOfZeroNegativeIsNan" [
    test "sqrt negative" "preservesSignOfZeroNegativeIsNan" (-1.0) nan64,
    test "sqrt NaN" "preservesSignOfZeroNegativeIsNan" nan64 nan64,
    test "sqrt +Inf" "preservesSignOfZeroNegativeIsNan" posInf64 posInf64,
    test "sqrt -Inf" "preservesSignOfZeroNegativeIsNan" negInf64 nan64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.sqrt [float64 x] (float64 result)

-- | Cases sourced from round3-317-conformance-matrix.md §2 (numeric class, all 11 types),
-- machine-verified against an independent implementation (round4-317-matrix-attack.md §1).
mathSub :: TypedTerm TestGroup
mathSub = supergroup "sub" [
  subgroup "wrapsOrRoundsPerType" [
    test8  "i8 nominal" 10 3 7,
    test16 "i16 nominal" 10 3 7,
    test32 "i32 nominal" 10 3 7,
    test64 "i64 nominal" 10 3 7,
    testU8  "u8 nominal" 10 3 7,
    testU16 "u16 nominal" 10 3 7,
    testU32 "u32 nominal" 10 3 7,
    testU64 "u64 nominal" 10 3 7,
    testBig "big nominal" 10 3 7,
    testF64 "f64 nominal" 5.5 2.25 3.25,
    testF32 "f32 nominal" 5.5 2.25 3.25],
  subgroup "boundaryWraparoundAndIeeeSpecialValues" [
    test8  "i8 wrap min" (-128) 1 127,
    test8  "i8 neg min" 0 (-128) (-128),
    test8  "i8 wrap max" 127 (-1) (-128),
    test16 "i16 wrap min" (-32768) 1 32767,
    test16 "i16 neg min" 0 (-32768) (-32768),
    test16 "i16 wrap max" 32767 (-1) (-32768),
    test32 "i32 wrap min" (-2147483648) 1 2147483647,
    test32 "i32 neg min" 0 (-2147483648) (-2147483648),
    test32 "i32 wrap max" 2147483647 (-1) (-2147483648),
    test64 "i64 wrap min" (-9223372036854775808) 1 9223372036854775807,
    test64 "i64 neg min" 0 (-9223372036854775808) (-9223372036854775808),
    test64 "i64 wrap max" 9223372036854775807 (-1) (-9223372036854775808),
    testU8  "u8 wrap zero" 0 1 255,
    testU8  "u8 wrap below" 3 5 254,
    testU16 "u16 wrap zero" 0 1 65535,
    testU16 "u16 wrap below" 3 5 65534,
    testU32 "u32 wrap zero" 0 1 4294967295,
    testU32 "u32 wrap below" 3 5 4294967294,
    testU64 "u64 wrap zero" 0 1 18446744073709551615,
    testU64 "u64 wrap below" 3 5 18446744073709551614,
    testBig "big negative" 0 18446744073709551616 (-18446744073709551616),
    testBig "big large" 1267650600228229401496703205376 1 1267650600228229401496703205375,
    testF64 "f64 inf-inf" posInf64 posInf64 nan64,
    testF64 "f64 x minus x" 1.0 1.0 0.0,
    testF64 "f64 negzero" (-0.0) 0.0 (-0.0),
    testF64 "f64 poszero" 0.0 (-0.0) 0.0,
    testF64 "f64 nan" nan64 nan64 nan64,
    testF32 "f32 tie-even" 16777218.0 1.0 16777216.0,
    testF32 "f32 inf-inf" posInf32 posInf32 nan32,
    testF32 "f32 x minus x" 1.0 1.0 0.0]]
  where
    test8   name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [int8 x, int8 y] (int8 r)
    test16  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [int16 x, int16 y] (int16 r)
    test32  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [int32 x, int32 y] (int32 r)
    test64  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [int64 x, int64 y] (int64 r)
    testU8  name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [uint8 x, uint8 y] (uint8 r)
    testU16 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [uint16 x, uint16 y] (uint16 r)
    testU32 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [uint32 x, uint32 y] (uint32 r)
    testU64 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [uint64 x, uint64 y] (uint64 r)
    testBig name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [bigint x, bigint y] (bigint r)
    testF64 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [float64 x, float64 y] (float64 r)
    testF32 name x y r = primCaseWithProvisions name [segmentOf name] DefMath.sub [float32 x, float32 y] (float32 r)
    segmentOf n = if "nominal" `L.isSuffixOf` n
      then "wrapsOrRoundsPerType" else "boundaryWraparoundAndIeeeSpecialValues"

mathSubFloat64 :: TypedTerm TestGroup
mathSubFloat64 = subgroup "subtractsFloat64Values" [
  test "positive numbers" "subtractsFloat64Values" 5.0 3.0 2.0,
  test "negative result" "subtractsFloat64Values" 3.0 5.0 (-2.0),
  test "negative numbers" "subtractsFloat64Values" (-3.0) (-5.0) 2.0,
  test "with zero" "subtractsFloat64Values" 42.0 0.0 42.0,
  test "same value" "subtractsFloat64Values" 42.0 42.0 0.0,
  test "fractional" "subtractsFloat64Values" 2.5 1.5 1.0]
  where
    test name segment x y result = primCaseWithProvisions name [segment] DefMath.subFloat64 [float64 x, float64 y] (float64 result)

mathTan :: TypedTerm TestGroup
mathTan = supergroup "tan" [
  subgroup "tangentOfAngleInRadians" [
    test "tan 0" "tangentOfAngleInRadians" 0.0 0.0,
    roundedPrimCase1WithProvisions "tan pi/4" ["tangentOfAngleInRadians"] DefMath.tan (pi / 4) (tan (pi / 4)),
    roundedPrimCase1WithProvisions "tan 1" ["tangentOfAngleInRadians"] DefMath.tan 1.0 (tan 1.0),
    roundedPrimCase1WithProvisions "tan 0.5" ["tangentOfAngleInRadians"] DefMath.tan 0.5 (tan 0.5)],
  subgroup "undefinedAtInfinityNanPropagates" [
    test "tan NaN" "undefinedAtInfinityNanPropagates" nan64 nan64,
    test "tan +Inf" "undefinedAtInfinityNanPropagates" posInf64 nan64,
    test "tan -Inf" "undefinedAtInfinityNanPropagates" negInf64 nan64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.tan [float64 x] (float64 result)

mathTanh :: TypedTerm TestGroup
mathTanh = supergroup "tanh" [
  subgroup "hyperbolicTangentInRangeNegOneToOne" [
    test "tanh 0" "hyperbolicTangentInRangeNegOneToOne" 0.0 0.0,
    roundedPrimCase1WithProvisions "tanh 1" ["hyperbolicTangentInRangeNegOneToOne"] DefMath.tanh 1.0 (tanh 1.0),
    roundedPrimCase1WithProvisions "tanh 0.5" ["hyperbolicTangentInRangeNegOneToOne"] DefMath.tanh 0.5 (tanh 0.5)],
  subgroup "saturatesAtInfinityNanPropagates" [
    test "tanh NaN" "saturatesAtInfinityNanPropagates" nan64 nan64,
    test "tanh +Inf" "saturatesAtInfinityNanPropagates" posInf64 1.0,
    test "tanh -Inf" "saturatesAtInfinityNanPropagates" negInf64 (-1.0)]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.tanh [float64 x] (float64 result)

mathTruncate :: TypedTerm TestGroup
mathTruncate = supergroup "truncate" [
  subgroup "roundsTowardZeroAsFloat" [
    test "truncate 3.8" "roundsTowardZeroAsFloat" 3.8 3.0,
    test "truncate 3.2" "roundsTowardZeroAsFloat" 3.2 3.0,
    test "truncate -3.8" "roundsTowardZeroAsFloat" (-3.8) (-3.0),
    test "truncate -3.2" "roundsTowardZeroAsFloat" (-3.2) (-3.0)],
  subgroup "ieeeSpecialValuesPassThrough" [
    test "truncate NaN" "ieeeSpecialValuesPassThrough" nan64 nan64,
    test "truncate +Inf" "ieeeSpecialValuesPassThrough" posInf64 posInf64,
    test "truncate -Inf" "ieeeSpecialValuesPassThrough" negInf64 negInf64]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefMath.truncate [float64 x] (float64 result)

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

roundedPrimCase1WithProvisions :: ToPrimName n => String -> [String] -> n -> Double -> Double -> TypedTerm TestCaseWithMetadata
roundedPrimCase1WithProvisions cname segments prim x result = evalCaseWithProvisions cname segments prim input output
  where
    input = Terms.primitive DefMath.roundFloat64 @@ int32 roundDigits @@ (Terms.primitive prim @@ float64 x)
    output = float64 (Math.roundFloat64 roundDigits result)

roundedPrimCase2WithProvisions :: ToPrimName n => String -> [String] -> n -> Double -> Double -> Double -> TypedTerm TestCaseWithMetadata
roundedPrimCase2WithProvisions cname segments prim x y result = evalCaseWithProvisions cname segments prim input output
  where
    input = Terms.primitive DefMath.roundFloat64 @@ int32 roundDigits @@ (Terms.primitive prim @@ float64 x @@ float64 y)
    output = float64 (Math.roundFloat64 roundDigits result)
