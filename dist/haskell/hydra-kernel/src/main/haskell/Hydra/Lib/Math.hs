-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.math module.

module Hydra.Lib.Math where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
abs :: Packaging.PrimitiveDefinition
abs =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.abs"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionDescription = "The absolute value of an integer.",
      Packaging.primitiveDefinitionComments = (Just "Absolute value of a signed 32-bit two's-complement integer. For non-negative inputs the result equals the input; for negative inputs the result is the arithmetic negation. The function is total but not injective at the boundary: abs(minBound) = minBound (i.e. abs(-2147483648) = -2147483648), because +2147483648 is not representable in int32. Corresponds to Haskell's abs :: Int32 -> Int32."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
acos :: Packaging.PrimitiveDefinition
acos =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.acos"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The arc cosine of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Principal value of the inverse cosine, in radians. The result is in [0, \960]. For arguments outside the domain [-1, +1] (including \177\8734), the result is NaN. acos(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 acos operation and to Haskell's acos :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
acosh :: Packaging.PrimitiveDefinition
acosh =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.acosh"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The hyperbolic arc cosine of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Principal value of the inverse hyperbolic cosine. The result is in [0, +\8734). For arguments less than 1 the result is NaN; acosh(1) = +0; acosh(+\8734) = +\8734; acosh(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 acosh operation and to Haskell's acosh :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
add :: Packaging.PrimitiveDefinition
add =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.add"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionDescription = "Integer addition.",
      Packaging.primitiveDefinitionComments = (Just "Two's-complement 32-bit signed integer addition. The result is x + y reduced modulo 2^32 and reinterpreted as a signed int32 (i.e. arithmetic wraps silently on overflow, with no exception raised). The operation is total. Corresponds to Haskell's (+) :: Int32 -> Int32 -> Int32."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
addFloat64 :: Packaging.PrimitiveDefinition
addFloat64 =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.addFloat64"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Floating-point addition.",
      Packaging.primitiveDefinitionComments = (Just "IEEE 754 binary64 addition. The result is the value of x + y rounded to the nearest representable float64 under the roundTiesToEven rounding-direction attribute. Adding infinities of opposite sign (+\8734 + -\8734 or -\8734 + +\8734) produces a NaN; adding any value to NaN produces a NaN. The sum of two zeros is +0, except (-0) + (-0) = -0. The operation is total: it never raises, but it may produce NaN. Corresponds to the IEEE 754 \2677.4.1 addition operation and to Haskell's (+) :: Double -> Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
asin :: Packaging.PrimitiveDefinition
asin =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.asin"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The arc sine of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Principal value of the inverse sine, in radians. The result is in [-\960/2, +\960/2]. For arguments outside the domain [-1, +1] (including \177\8734), the result is NaN; asin(\2832) = \2832; asin(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 asin operation and to Haskell's asin :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
asinh :: Packaging.PrimitiveDefinition
asinh =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.asinh"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The hyperbolic arc sine of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Principal value of the inverse hyperbolic sine. Defined for all finite reals; asinh(\2832) = \2832; asinh(\177\8734) = \177\8734; asinh(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 asinh operation and to Haskell's asinh :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
atan :: Packaging.PrimitiveDefinition
atan =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.atan"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The arc tangent of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Principal value of the inverse tangent, in radians. The result is in (-\960/2, +\960/2); atan(\2832) = \2832; atan(\177\8734) = \177\960/2; atan(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 atan operation and to Haskell's atan :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
atan2 :: Packaging.PrimitiveDefinition
atan2 =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.atan2"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The two-argument arc tangent (atan2).",
      Packaging.primitiveDefinitionComments = (Just "atan2(y, x) returns the angle in radians, in (-\960, +\960], from the positive x-axis to the point (x, y), using the signs of both arguments to determine the quadrant. Special cases follow IEEE 754 \2681.2: atan2(\2832, +x>0) = \2832; atan2(\2832, -x<0) = \177\960; atan2(\177y>0, 0) = \177\960/2; atan2(\177y, +\8734) = \2832; atan2(\177y, -\8734) = \177\960; atan2(\177\8734, finite) = \177\960/2; atan2(\177\8734, +\8734) = \177\960/4; atan2(\177\8734, -\8734) = \2835\960/4; atan2 of any NaN argument is NaN. Corresponds to Haskell's atan2 :: Double -> Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
atanh :: Packaging.PrimitiveDefinition
atanh =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.atanh"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The hyperbolic arc tangent of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Principal value of the inverse hyperbolic tangent. Defined for arguments in (-1, +1); atanh(\2833) = \177\8734 (with division-by-zero exception in IEEE 754); arguments outside [-1, +1] produce NaN; atanh(\2832) = \2832; atanh(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 atanh operation and to Haskell's atanh :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
ceiling :: Packaging.PrimitiveDefinition
ceiling =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.ceiling"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The smallest integer greater than or equal to the argument, as a float.",
      Packaging.primitiveDefinitionComments = (Just "The smallest integer value not less than the argument, returned as a float64. Equivalent to IEEE 754 \2677.3.1 roundToIntegralTowardPositive: ceiling(\2832) = \2832; ceiling(\177\8734) = \177\8734; ceiling(NaN) is NaN; the sign of a negative result is preserved when rounding to zero (e.g. ceiling(-0.5) = -0). Note that the return type is float64, not an integer type, so the result can exceed the integer range. Corresponds to Haskell's fromIntegral . ceiling :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
cos :: Packaging.PrimitiveDefinition
cos =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.cos"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The cosine of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Cosine of an angle in radians, correctly rounded for finite arguments. The result is in [-1, +1]; cos(\2832) = 1; cos(\177\8734) is NaN (with invalid-operation exception in IEEE 754); cos(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 cos operation and to Haskell's cos :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
cosh :: Packaging.PrimitiveDefinition
cosh =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.cosh"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The hyperbolic cosine of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Hyperbolic cosine. The result is in [1, +\8734]; cosh(\2832) = 1; cosh(\177\8734) = +\8734; cosh(NaN) is NaN. Large-magnitude arguments overflow to +\8734. Corresponds to the IEEE 754 \2681.2 cosh operation and to Haskell's cosh :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
e :: Packaging.PrimitiveDefinition
e =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.e"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Euler's constant (the base of the natural logarithm).",
      Packaging.primitiveDefinitionComments = (Just "The mathematical constant e \8776 2.718281828459045, the base of the natural logarithm, as the nearest representable float64. Equal to exp(1). Corresponds to Haskell's exp 1 :: Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
even :: Packaging.PrimitiveDefinition
even =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.even"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionDescription = "Test whether an integer is even.",
      Packaging.primitiveDefinitionComments = (Just "True if the argument is divisible by 2 (i.e. x mod 2 = 0), false otherwise. Total on all int32 inputs including negative numbers and minBound. Corresponds to Haskell's even :: Int32 -> Bool."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.equal")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.fromMaybe")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.maybeMod")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}))})),
            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Test whether an integer is even, defined via maybeMod and equality.")))])})))}
exp :: Packaging.PrimitiveDefinition
exp =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.exp"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The exponential function.",
      Packaging.primitiveDefinitionComments = (Just "The exponential function: exp(x) = e^x. exp(\2832) = 1; exp(-\8734) = +0; exp(+\8734) = +\8734; exp(NaN) is NaN. Large positive arguments overflow to +\8734; large negative arguments underflow to +0. Corresponds to the IEEE 754 \2681.2 exp operation and to Haskell's exp :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
floor :: Packaging.PrimitiveDefinition
floor =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.floor"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The largest integer less than or equal to the argument, as a float.",
      Packaging.primitiveDefinitionComments = (Just "The largest integer value not greater than the argument, returned as a float64. Equivalent to IEEE 754 \2677.3.1 roundToIntegralTowardNegative: floor(\2832) = \2832; floor(\177\8734) = \177\8734; floor(NaN) is NaN. Note that the return type is float64, not an integer type, so the result can exceed the integer range. Corresponds to Haskell's fromIntegral . floor :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
log :: Packaging.PrimitiveDefinition
log =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.log"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The natural logarithm.",
      Packaging.primitiveDefinitionComments = (Just "The natural (base-e) logarithm. log(1) = +0; log(+0) = log(-0) = -\8734 (with division-by-zero exception in IEEE 754); log(x) for x < 0 is NaN (with invalid-operation exception); log(+\8734) = +\8734; log(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 log operation and to Haskell's log :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
logBase :: Packaging.PrimitiveDefinition
logBase =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.logBase"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Logarithm of the second argument in the base of the first.",
      Packaging.primitiveDefinitionComments = (Just "logBase(b, x) computes the logarithm of x in base b, equivalent to log(x) / log(b). Inherits the special-case behavior of log for each argument (NaN propagation, sign of zeros, division by zero on log of a zero, NaN on negative arguments). Corresponds to Haskell's logBase :: Double -> Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
max :: Packaging.PrimitiveDefinition
max =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.max"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionDescription = "The maximum of two integers.",
      Packaging.primitiveDefinitionComments = (Just "Return the larger of two int32 values under the usual signed-integer total order. Total on all inputs. Corresponds to Haskell's max :: Int32 -> Int32 -> Int32."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
maybeDiv :: Packaging.PrimitiveDefinition
maybeDiv =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.maybeDiv"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}},
      Packaging.primitiveDefinitionDescription = "Integer division, or Nothing if dividing by zero.",
      Packaging.primitiveDefinitionComments = (Just "Total integer division: maybeDiv(x, y) returns Just(x divided by y, rounded toward negative infinity) when y is non-zero, and Nothing when y = 0. The division rounds toward negative infinity (floor), so for example maybeDiv(-7, 2) = Just(-4). The boundary case maybeDiv(minBound, -1), whose mathematical result +2147483648 is not representable in int32, wraps to minBound (the two's-complement overflow). Corresponds to Haskell's div :: Int32 -> Int32 -> Int32, wrapped in maybe to make the zero-divisor case total."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
maybeMod :: Packaging.PrimitiveDefinition
maybeMod =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.maybeMod"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}},
      Packaging.primitiveDefinitionDescription = "Integer modulus, or Nothing if dividing by zero.",
      Packaging.primitiveDefinitionComments = (Just "Total integer modulus: maybeMod(x, y) returns Just(x mod y) when y is non-zero, and Nothing when y = 0. The result satisfies the identity x = (maybeDiv(x, y) result) * y + (maybeMod(x, y) result), so the sign of the result matches the sign of y (Knuth-style floor division). For example maybeMod(-7, 2) = Just(1). Corresponds to Haskell's mod :: Int32 -> Int32 -> Int32, wrapped in maybe to make the zero-divisor case total."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
maybePred :: Packaging.PrimitiveDefinition
maybePred =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.maybePred"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}},
      Packaging.primitiveDefinitionDescription = "The predecessor of an integer, or Nothing on underflow.",
      Packaging.primitiveDefinitionComments = (Just "maybePred(x) returns Just(x - 1) when x > minBound, and Nothing when x = minBound (i.e. -2147483648). The function is total and does not wrap. Corresponds to Haskell's pred :: Int32 -> Int32, wrapped in maybe to make the boundary case total (Haskell's pred is a partial function that raises an error on minBound)."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
maybeRem :: Packaging.PrimitiveDefinition
maybeRem =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.maybeRem"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}},
      Packaging.primitiveDefinitionDescription = "Integer remainder, or Nothing if dividing by zero.",
      Packaging.primitiveDefinitionComments = (Just "Total integer remainder: maybeRem(x, y) returns Just(x rem y) when y is non-zero, and Nothing when y = 0. The result satisfies x = (truncate(x / y)) * y + (maybeRem(x, y) result), so the sign of the result matches the sign of x (truncated division, C-style remainder). For example maybeRem(-7, 2) = Just(-1). The boundary case maybeRem(minBound, -1) is 0 (no overflow, since the quotient overflow is absorbed). Corresponds to Haskell's rem :: Int32 -> Int32 -> Int32, wrapped in maybe to make the zero-divisor case total."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
maybeSucc :: Packaging.PrimitiveDefinition
maybeSucc =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.maybeSucc"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}},
      Packaging.primitiveDefinitionDescription = "The successor of an integer, or Nothing on overflow.",
      Packaging.primitiveDefinitionComments = (Just "maybeSucc(x) returns Just(x + 1) when x < maxBound, and Nothing when x = maxBound (i.e. 2147483647). The function is total and does not wrap. Corresponds to Haskell's succ :: Int32 -> Int32, wrapped in maybe to make the boundary case total (Haskell's succ is a partial function that raises an error on maxBound)."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
min :: Packaging.PrimitiveDefinition
min =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.min"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionDescription = "The minimum of two integers.",
      Packaging.primitiveDefinitionComments = (Just "Return the smaller of two int32 values under the usual signed-integer total order. Total on all inputs. Corresponds to Haskell's min :: Int32 -> Int32 -> Int32."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
mul :: Packaging.PrimitiveDefinition
mul =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.mul"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionDescription = "Integer multiplication.",
      Packaging.primitiveDefinitionComments = (Just "Two's-complement 32-bit signed integer multiplication. The result is x * y reduced modulo 2^32 and reinterpreted as a signed int32 (i.e. arithmetic wraps silently on overflow, with no exception raised). The operation is total. Corresponds to Haskell's (*) :: Int32 -> Int32 -> Int32."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
mulFloat64 :: Packaging.PrimitiveDefinition
mulFloat64 =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.mulFloat64"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Floating-point multiplication.",
      Packaging.primitiveDefinitionComments = (Just "IEEE 754 binary64 multiplication. The result is the value of x * y rounded to the nearest representable float64 under the roundTiesToEven rounding-direction attribute. Multiplying 0 by \177\8734 (in either order) produces a NaN (with invalid-operation exception in IEEE 754); multiplying any value by NaN produces a NaN. The result's sign is the XOR of the operand signs (\2832 * \2832 = \2832 accordingly). The operation is total: it never raises, but it may produce NaN or \177\8734. Corresponds to the IEEE 754 \2677.4.1 multiplication operation and to Haskell's (*) :: Double -> Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
negate :: Packaging.PrimitiveDefinition
negate =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.negate"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionDescription = "Negate an integer.",
      Packaging.primitiveDefinitionComments = (Just "Arithmetic negation of a 32-bit signed integer. The result is 0 - x reduced modulo 2^32 and reinterpreted as a signed int32. The function is total but not injective at the boundary: negate(minBound) = minBound (i.e. negate(-2147483648) = -2147483648), because +2147483648 is not representable in int32. Corresponds to Haskell's negate :: Int32 -> Int32."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
negateFloat64 :: Packaging.PrimitiveDefinition
negateFloat64 =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.negateFloat64"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Negate a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Sign reversal of a float64. Equivalent to IEEE 754 \2677.5.1 negate: flips the sign bit, so negate(\2832) = \2832 (sign flips), negate(\177\8734) = \177\8734 (sign flips), and negate(NaN) is a NaN (sign may flip; payload preserved). This is a bit-level operation that does not raise any floating-point exception. Corresponds to Haskell's negate :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
odd :: Packaging.PrimitiveDefinition
odd =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.odd"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionDescription = "Test whether an integer is odd.",
      Packaging.primitiveDefinitionComments = (Just "True if the argument is not divisible by 2 (i.e. x mod 2 \8800 0), false otherwise. Total on all int32 inputs including negative numbers and minBound. Corresponds to Haskell's odd :: Int32 -> Bool."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.not")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.even")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Test whether an integer is odd, defined as the negation of even.")))])})))}
pi :: Packaging.PrimitiveDefinition
pi =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.pi"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The mathematical constant pi.",
      Packaging.primitiveDefinitionComments = (Just "The mathematical constant \960 \8776 3.141592653589793, the ratio of a circle's circumference to its diameter, as the nearest representable float64. Corresponds to Haskell's pi :: Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
pow :: Packaging.PrimitiveDefinition
pow =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.pow"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Raise the first argument to the power of the second.",
      Packaging.primitiveDefinitionComments = (Just "pow(x, y) = x^y. Follows the IEEE 754 \2681.2 pow operation: pow(\2832, y) for y < 0 is \177\8734 (with division-by-zero exception); pow(\2832, y) for y > 0 is \2832 if y is an odd integer, else +0; pow(1, y) = 1 for any y including NaN; pow(x, \2832) = 1 for any x including NaN; pow(x, y) for negative x and non-integer y is NaN (with invalid-operation exception); pow(\177\8734, y) follows the limits in the usual way. Otherwise the result is x^y rounded to the nearest representable float64. Corresponds to Haskell's (**) :: Double -> Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
range :: Packaging.PrimitiveDefinition
range =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.range"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}},
      Packaging.primitiveDefinitionDescription = "Construct the inclusive integer range from the first to the second argument.",
      Packaging.primitiveDefinitionComments = (Just "range(a, b) returns the list [a, a+1, ..., b]. The range is inclusive at both ends; if a > b the result is the empty list (i.e. the range does not count downward). For a = b the result is the singleton [a]. The length of the result is max(0, b - a + 1). Corresponds to Haskell's enumFromTo :: Int32 -> Int32 -> [Int32], equivalent to the list-comprehension form [a..b]."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
round :: Packaging.PrimitiveDefinition
round =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.round"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Round a floating-point number to the nearest integer-valued float.",
      Packaging.primitiveDefinitionComments = (Just "Round to the nearest integer value, returned as a float64, with ties rounded to the nearest even integer (banker's rounding). Equivalent to IEEE 754 \2677.3.1 roundToIntegralTiesToEven: round(\2832) = \2832; round(\177\8734) = \177\8734; round(NaN) is NaN. Note that the return type is float64, not an integer type, so the result can exceed the integer range. Corresponds to Haskell's fromIntegral . round :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
roundFloat32 :: Packaging.PrimitiveDefinition
roundFloat32 =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.roundFloat32"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}},
      Packaging.primitiveDefinitionDescription = "Round a float32 to the given number of decimal places.",
      Packaging.primitiveDefinitionComments = (Just "roundFloat32(n, x) rounds the float32 value x to n decimal places using round-half-to-even. The result is the nearest float32 representation of x rounded to that decimal precision; if the exact decimal-rounded value is not representable in float32 (the usual case), the closest float32 is returned. Special values pass through: roundFloat32(n, \2832) = \2832; roundFloat32(n, \177\8734) = \177\8734; roundFloat32(n, NaN) is NaN. Negative n is supported in principle (rounding to powers of ten above 1); host implementations may differ on out-of-range n."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
roundFloat64 :: Packaging.PrimitiveDefinition
roundFloat64 =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.roundFloat64"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Round a float64 to the given number of decimal places.",
      Packaging.primitiveDefinitionComments = (Just "roundFloat64(n, x) rounds the float64 value x to n decimal places using round-half-to-even. The result is the nearest float64 representation of x rounded to that decimal precision; if the exact decimal-rounded value is not representable in float64 (the usual case), the closest float64 is returned. Special values pass through: roundFloat64(n, \2832) = \2832; roundFloat64(n, \177\8734) = \177\8734; roundFloat64(n, NaN) is NaN. Negative n is supported in principle (rounding to powers of ten above 1); host implementations may differ on out-of-range n."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
signum :: Packaging.PrimitiveDefinition
signum =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.signum"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionDescription = "Return the sign of an integer as -1, 0, or 1.",
      Packaging.primitiveDefinitionComments = (Just "signum(x) returns -1 if x < 0, 0 if x = 0, and 1 if x > 0. The function is total and satisfies the identity abs(x) * signum(x) = x for all int32 except minBound (where abs(minBound) * (-1) wraps to minBound rather than equalling -minBound, since +2147483648 is not representable). Corresponds to Haskell's signum :: Int32 -> Int32."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
sin :: Packaging.PrimitiveDefinition
sin =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.sin"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The sine of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Sine of an angle in radians, correctly rounded for finite arguments. The result is in [-1, +1]; sin(\2832) = \2832; sin(\177\8734) is NaN (with invalid-operation exception in IEEE 754); sin(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 sin operation and to Haskell's sin :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
sinh :: Packaging.PrimitiveDefinition
sinh =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.sinh"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The hyperbolic sine of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Hyperbolic sine. sinh(\2832) = \2832; sinh(\177\8734) = \177\8734; sinh(NaN) is NaN. Large-magnitude arguments overflow to \177\8734. Corresponds to the IEEE 754 \2681.2 sinh operation and to Haskell's sinh :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
sqrt :: Packaging.PrimitiveDefinition
sqrt =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.sqrt"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The non-negative square root of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "IEEE 754 binary64 square root. The result is the value of \8730x correctly rounded to the nearest representable float64 under roundTiesToEven. sqrt(+0) = +0; sqrt(-0) = -0 (sign preserved); sqrt(x) for x < 0 (including -\8734) is NaN (with invalid-operation exception); sqrt(+\8734) = +\8734; sqrt(NaN) is NaN. Corresponds to the IEEE 754 \2677.4.1 squareRoot operation and to Haskell's sqrt :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
sub :: Packaging.PrimitiveDefinition
sub =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.sub"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}},
      Packaging.primitiveDefinitionDescription = "Integer subtraction.",
      Packaging.primitiveDefinitionComments = (Just "Two's-complement 32-bit signed integer subtraction. The result is x - y reduced modulo 2^32 and reinterpreted as a signed int32 (i.e. arithmetic wraps silently on overflow, with no exception raised). The operation is total. Corresponds to Haskell's (-) :: Int32 -> Int32 -> Int32."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
subFloat64 :: Packaging.PrimitiveDefinition
subFloat64 =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.subFloat64"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Floating-point subtraction.",
      Packaging.primitiveDefinitionComments = (Just "IEEE 754 binary64 subtraction, defined as x + (-y). The result is correctly rounded to the nearest representable float64 under roundTiesToEven. Subtracting infinities of the same sign (+\8734 - +\8734 or -\8734 - -\8734) produces a NaN; subtracting any value involving NaN produces a NaN. The difference of two equal finite values is +0 (or -0 under round-toward-negative, which is not the default). The operation is total: it never raises, but it may produce NaN. Corresponds to the IEEE 754 \2677.4.1 subtraction operation and to Haskell's (-) :: Double -> Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
tan :: Packaging.PrimitiveDefinition
tan =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.tan"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The tangent of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Tangent of an angle in radians, correctly rounded for finite arguments. tan(\2832) = \2832; near odd multiples of \960/2 the result has large magnitude but remains finite (no exception is raised); tan(\177\8734) is NaN (with invalid-operation exception in IEEE 754); tan(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 tan operation and to Haskell's tan :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
tanh :: Packaging.PrimitiveDefinition
tanh =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.tanh"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "The hyperbolic tangent of a floating-point number.",
      Packaging.primitiveDefinitionComments = (Just "Hyperbolic tangent. The result is in [-1, +1]; tanh(\2832) = \2832; tanh(\177\8734) = \2833; tanh(NaN) is NaN. Corresponds to the IEEE 754 \2681.2 tanh operation and to Haskell's tanh :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
truncate :: Packaging.PrimitiveDefinition
truncate =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.math.truncate"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}},
      Packaging.primitiveDefinitionDescription = "Truncate a floating-point number toward zero, as a float.",
      Packaging.primitiveDefinitionComments = (Just "Round toward zero (truncate the fractional part), returned as a float64. Equivalent to IEEE 754 \2677.3.1 roundToIntegralTowardZero: truncate(\2832) = \2832; truncate(\177\8734) = \177\8734; truncate(NaN) is NaN; sign of the result matches the sign of the argument (so truncate(-0.7) = -0, truncate(+0.7) = +0). Note that the return type is float64, not an integer type, so the result can exceed the integer range. Corresponds to Haskell's fromIntegral . truncate :: Double -> Double."),
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
