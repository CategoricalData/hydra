-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.logic namespace.

module Hydra.Lib.Logic where
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
and :: Packaging.PrimitiveDefinition
and =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.logic.and"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Compute the logical AND of two boolean values."),
        Packaging.entityMetadataComments = [
          "and(p, q) returns true iff both p and q are true. Evaluation is strict in both arguments at the primitive level; for short-circuiting behavior, use ifElse. Total. Corresponds to Haskell's (&&) :: Bool -> Bool -> Bool (but without short-circuit evaluation)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "a"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "b"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Logical AND, defined in terms of ifElse.")))])})))}
ifElse :: Packaging.PrimitiveDefinition
ifElse =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.logic.ifElse"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "x"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeVariable (Core.Name "x")),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "x"))}},
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Compute a conditional expression."),
        Packaging.entityMetadataComments = [
          "ifElse(p, t, f) returns t if p is true, or f if p is false. The unselected branch is not necessarily evaluated; ifElse is the standard way to express short-circuiting boolean logic and branching in Hydra. Total. Corresponds to Haskell's if/then/else."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
not :: Packaging.PrimitiveDefinition
not =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.logic.not"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Compute the logical NOT of a boolean value."),
        Packaging.entityMetadataComments = [
          "not(p) returns false if p is true, or true if p is false. Total. Corresponds to Haskell's not :: Bool -> Bool."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "a"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
            Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Logical NOT, defined in terms of ifElse.")))])})))}
or :: Packaging.PrimitiveDefinition
or =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.logic.or"),
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeLiteral Core.LiteralTypeBoolean)}},
      Packaging.primitiveDefinitionMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Compute the logical OR of two boolean values."),
        Packaging.entityMetadataComments = [
          "or(p, q) returns true iff at least one of p and q is true. Evaluation is strict in both arguments at the primitive level; for short-circuiting behavior, use ifElse. Total. Corresponds to Haskell's (||) :: Bool -> Bool -> Bool (but without short-circuit evaluation)."],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = (Just (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "a"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "b"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Logical OR, defined in terms of ifElse.")))])})))}
