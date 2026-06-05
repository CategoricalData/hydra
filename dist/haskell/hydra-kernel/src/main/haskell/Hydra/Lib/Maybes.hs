-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.maybes module.

module Hydra.Lib.Maybes where
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
apply :: Packaging.PrimitiveDefinition
apply =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.apply"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Applicative apply for maybes: combine a maybe function and a maybe argument."),
        Packaging.entityMetadataComments = [
          "apply(mf, mx) returns Just(f x) when mf is Just(f) and mx is Just(x), and Nothing if either is Nothing.",
          "The applicative apply for maybe; threads a function-in-context with a value-in-context.",
          "Total. Corresponds to Haskell's (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
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
            Typing.parameterType = (Core.TypeMaybe (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))}))),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "y")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "mf"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "mx"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.bind")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "mf"))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "f"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "mx"))}))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Applicative apply for optionals, defined in terms of bind and map.")))]))})))}
bind :: Packaging.PrimitiveDefinition
bind =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.bind"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Monadic bind for maybes."),
        Packaging.entityMetadataComments = [
          "bind(m, f) returns f(x) when m is Just(x), and Nothing when m is Nothing.",
          "The monadic bind for maybe; used to chain computations that may be absent.",
          "Total. Corresponds to Haskell's (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
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
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "y")))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "y")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "m"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "f"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                  Core.applicationArgument = (Core.TermMaybe Nothing)})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Monadic bind for optionals, defined in terms of maybe.")))]))})))}
cases :: Packaging.PrimitiveDefinition
cases =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.cases"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Case analysis on a maybe, with cases-style argument order."),
        Packaging.entityMetadataComments = [
          "cases(m, def, f) returns f(x) when m is Just(x), and def when m is Nothing.",
          "Identical in behavior to the maybe primitive but with the maybe value as the first argument (matching the convention for case-statement-like elimination).",
          "Total. Argument order is (m, def, f) rather than Haskell's (def, f, m)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
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
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "y")),
            Typing.parameterIsLazy = True},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "y"))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
cat :: Packaging.PrimitiveDefinition
cat =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.cat"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Concatenate maybes, keeping only the present values."),
        Packaging.entityMetadataComments = [
          "cat(xs) returns the list of contained values from Just elements of xs, in original order; Nothing elements are discarded.",
          "Total. Corresponds to Haskell's Data.Maybe.catMaybes :: [Maybe a] -> [a]."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeMaybe (Core.TypeVariable (Core.Name "x")))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "xs"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldr")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "m"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "acc"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))})),
                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.cons")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))}))}))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))}))})),
              Core.applicationArgument = (Core.TermList [])})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Catenate a list of optionals, keeping only the present values.")))]))})))}
compose :: Packaging.PrimitiveDefinition
compose =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.compose"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Kleisli composition for maybes."),
        Packaging.entityMetadataComments = [
          "compose(f, g, x) returns the Kleisli composition of f and g applied to x: bind(f(x), g).",
          "If either f or the second stage produces Nothing, the result is Nothing.",
          "Total. Corresponds to Haskell's Kleisli composition for Maybe, (>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
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
              Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "y")))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "y")),
              Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "z")))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "z")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "f"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "g"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.bind")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "g"))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Kleisli composition for optionals, defined in terms of bind.")))]))})))}
fromMaybe :: Packaging.PrimitiveDefinition
fromMaybe =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.fromMaybe"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Return the value contained in a maybe, falling back to a default if absent."),
        Packaging.entityMetadataComments = [
          "fromMaybe(def, m) returns x when m is Just(x), and def when m is Nothing.",
          "Total. Corresponds to Haskell's Data.Maybe.fromMaybe :: a -> Maybe a -> a."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = True},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "x"))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "def"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "m"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "def"))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Return the contained value or a default, defined in terms of maybe.")))]))})))}
isJust :: Packaging.PrimitiveDefinition
isJust =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.isJust"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Test whether a maybe is present (Just)."),
        Packaging.entityMetadataComments = [
          "isJust(m) returns true iff m is a Just variant.",
          "Total. Corresponds to Haskell's Data.Maybe.isJust :: Maybe a -> Bool."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "m"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Test for presence, defined in terms of maybe.")))]))})))}
isNothing :: Packaging.PrimitiveDefinition
isNothing =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.isNothing"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Test whether a maybe is absent (Nothing)."),
        Packaging.entityMetadataComments = [
          "isNothing(m) returns true iff m is the Nothing variant.",
          "Total. Corresponds to Haskell's Data.Maybe.isNothing :: Maybe a -> Bool."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "m"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))}))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Test for absence, defined in terms of maybe.")))]))})))}
map :: Packaging.PrimitiveDefinition
map =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.map"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Map a function over a maybe."),
        Packaging.entityMetadataComments = [
          "map(f, m) returns Just(f x) when m is Just(x), and Nothing when m is Nothing.",
          "The functor instance for maybe.",
          "Total. Corresponds to Haskell's fmap :: (a -> b) -> Maybe a -> Maybe b."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
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
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "y")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "f"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "m"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                  Core.applicationArgument = (Core.TermMaybe Nothing)})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermMaybe (Just (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))}))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Map a function over an optional, defined in terms of maybe.")))]))})))}
mapMaybe :: Packaging.PrimitiveDefinition
mapMaybe =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.mapMaybe"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Map a partial function over a list, keeping only the present results."),
        Packaging.entityMetadataComments = [
          "mapMaybe(f, xs) applies f to each element of xs and returns the list of contained values from Just results in original order; Nothing results are discarded.",
          "Total. Corresponds to Haskell's Data.Maybe.mapMaybe :: (a -> Maybe b) -> [a] -> [b]."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
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
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "y")))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeList (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeVariable (Core.Name "y")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "f"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "xs"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.cat")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Map a partial function and keep only the present results, defined in terms of lists.map and cat.")))]))})))}
maybe :: Packaging.PrimitiveDefinition
maybe =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.maybe"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Case analysis on a maybe, applying a function if present or returning a default if absent."),
        Packaging.entityMetadataComments = [
          "maybe(def, f, m) returns f(x) when m is Just(x), and def when m is Nothing.",
          "The fundamental eliminator for the maybe type; every other primitive in this namespace can be derived from it.",
          "Total. Corresponds to Haskell's maybe :: b -> (a -> b) -> Maybe a -> b."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "y"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "y")),
            Typing.parameterIsLazy = True},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "y"))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
pure :: Packaging.PrimitiveDefinition
pure =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.pure"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Wrap a value in Just."),
        Packaging.entityMetadataComments = [
          "pure(x) = Just(x). The applicative pure for maybe.",
          "Total. Corresponds to Haskell's pure :: a -> Maybe a / Just."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermMaybe (Just (Core.TermVariable (Core.Name "x"))))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Wrap a value in Just.")))]))})))}
toList :: Packaging.PrimitiveDefinition
toList =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.maybes.toList"),
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Convert a maybe to a list: Just x maps to [x], Nothing to []."),
        Packaging.entityMetadataComments = [
          "toList(m) returns [x] when m is Just(x), and the empty list when m is Nothing.",
          "Total. Corresponds to Haskell's Data.Maybe.maybeToList :: Maybe a -> [a]."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "x"))),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeList (Core.TypeVariable (Core.Name "x")))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "m"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                Core.applicationArgument = (Core.TermList [])})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermList [
                  Core.TermVariable (Core.Name "x")])}))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))})),
        Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
          (
            Core.TermVariable (Core.Name "description"),
            (Core.TermLiteral (Core.LiteralString "Convert an optional to a list, defined in terms of maybe.")))]))})))}
