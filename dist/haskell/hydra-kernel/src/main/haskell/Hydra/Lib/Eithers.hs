-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.eithers namespace.

module Hydra.Lib.Eithers where
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
bimap :: Packaging.PrimitiveDefinition
bimap =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.bimap"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "z"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "w"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "z"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "y")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "w"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "w"))}))}},
      Packaging.primitiveDefinitionDescription = "Map over both sides of an Either value.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "f"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "g"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "e"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermEither (Left (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))))}))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Map over both sides of an Either, defined in terms of either.")))])})))}
bind :: Packaging.PrimitiveDefinition
bind =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.bind"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "z"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "y")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "z"))}))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "z"))}))}},
      Packaging.primitiveDefinitionDescription = "Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "e"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "f"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "x"))))}))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Monadic bind for Either, defined in terms of either.")))])})))}
either :: Packaging.PrimitiveDefinition
either =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.either"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "z"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "z"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "y")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "z"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "z"))}},
      Packaging.primitiveDefinitionDescription = "Eliminate an Either value by applying one of two functions.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
foldl :: Packaging.PrimitiveDefinition
foldl =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.foldl"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "z"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "y")),
                Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "x"))}))}))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeVariable (Core.Name "y"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "x"))}))}},
      Packaging.primitiveDefinitionDescription = "Left-fold over a list with an Either-returning function, short-circuiting on Left.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
fromLeft :: Packaging.PrimitiveDefinition
fromLeft =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.fromLeft"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "x"))}},
      Packaging.primitiveDefinitionDescription = "Extract the Left value, or return a default.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "def"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "e"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "_"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "def"))}))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Extract the Left value or return a default, defined in terms of either.")))])})))}
fromRight :: Packaging.PrimitiveDefinition
fromRight =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.fromRight"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "y")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "y"))}},
      Packaging.primitiveDefinitionDescription = "Extract the Right value, or return a default.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "def"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "e"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "_"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "def"))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Extract the Right value or return a default, defined in terms of either.")))])})))}
isLeft :: Packaging.PrimitiveDefinition
isLeft =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.isLeft"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionDescription = "Check whether an Either is a Left value.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "e"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "_"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))}))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Check whether an Either is a Left value, defined in terms of either.")))])})))}
isRight :: Packaging.PrimitiveDefinition
isRight =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.isRight"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionDescription = "Check whether an Either is a Right value.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "e"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "_"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Check whether an Either is a Right value, defined in terms of either.")))])})))}
lefts :: Packaging.PrimitiveDefinition
lefts =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.lefts"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))}))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionDescription = "Extract all Left values from a list of Eithers.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
map :: Packaging.PrimitiveDefinition
map =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.map"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "z"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "x"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))}))}},
      Packaging.primitiveDefinitionDescription = "Map a function over the Right side of an Either (standard functor map).",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "f"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "e"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "x"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))))}))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Map a function over the Right side, defined in terms of either.")))])})))}
mapList :: Packaging.PrimitiveDefinition
mapList =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.mapList"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "z"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))}))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
            Core.eitherTypeRight = (Core.TypeList (Core.TypeVariable (Core.Name "y")))}))}},
      Packaging.primitiveDefinitionDescription = "Map a function returning Either over a list, collecting results or short-circuiting on Left.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
mapMaybe :: Packaging.PrimitiveDefinition
mapMaybe =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.mapMaybe"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "z"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))}))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
            Core.eitherTypeRight = (Core.TypeMaybe (Core.TypeVariable (Core.Name "y")))}))}},
      Packaging.primitiveDefinitionDescription = "Map a function returning Either over a Maybe, or return Right Nothing if Nothing.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
mapSet :: Packaging.PrimitiveDefinition
mapSet =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.mapSet"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "z"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))}))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeSet (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "z")),
            Core.eitherTypeRight = (Core.TypeSet (Core.TypeVariable (Core.Name "y")))}))}},
      Packaging.primitiveDefinitionDescription = "Map a function returning Either over a Set, collecting results or short-circuiting on Left.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
partitionEithers :: Packaging.PrimitiveDefinition
partitionEithers =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.partitionEithers"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))}))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "x"))),
            Core.pairTypeSecond = (Core.TypeList (Core.TypeVariable (Core.Name "y")))}))}},
      Packaging.primitiveDefinitionDescription = "Partition a list of Eithers into lefts and rights.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
rights :: Packaging.PrimitiveDefinition
rights =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.eithers.rights"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "x")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "y"))}))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeVariable (Core.Name "y")))}},
      Packaging.primitiveDefinitionDescription = "Extract all Right values from a list of Eithers.",
      Packaging.primitiveDefinitionComments = Nothing,
      Packaging.primitiveDefinitionSeeAlso = [],
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionAvailableSince = Nothing,
      Packaging.primitiveDefinitionDeprecatedSince = Nothing,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
