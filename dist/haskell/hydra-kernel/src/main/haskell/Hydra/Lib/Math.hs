-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.math namespace.

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
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionDescription = "Round a Float32 to the given number of decimal places.",
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionDescription = "Round a Float64 to the given number of decimal places.",
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
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
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
