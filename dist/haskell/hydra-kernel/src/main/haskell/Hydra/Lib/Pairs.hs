-- Note: this is an automatically generated file. Do not edit.
-- | Primitives in the hydra.lib.pairs namespace.

module Hydra.Lib.Pairs where
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
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.pairs.bimap"),
      Packaging.primitiveDefinitionDescription = "Map over both elements of a pair.",
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "a"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "b"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "c"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "d"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "c"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg1"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "d"))})),
            Typing.parameterIsLazy = False},
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg2"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeVariable (Core.Name "c")),
            Core.pairTypeSecond = (Core.TypeVariable (Core.Name "d"))}))}},
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
              Core.lambdaParameter = (Core.Name "p"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermPair (
                Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}),
                (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}))))}))}))})),
        Core.annotatedTermAnnotation = (M.fromList [
          (
            Core.Name "description",
            (Core.TermLiteral (Core.LiteralString "Map over both elements of a pair, defined in terms of first and second.")))])})))}
first :: Packaging.PrimitiveDefinition
first =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.pairs.first"),
      Packaging.primitiveDefinitionDescription = "Get the first element of a pair.",
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "a"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "b"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "a"))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
second :: Packaging.PrimitiveDefinition
second =
    Packaging.PrimitiveDefinition {
      Packaging.primitiveDefinitionName = (Core.Name "hydra.lib.pairs.second"),
      Packaging.primitiveDefinitionDescription = "Get the second element of a pair.",
      Packaging.primitiveDefinitionSignature = Typing.TermSignature {
        Typing.termSignatureTypeParameters = [
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "a"),
            Typing.typeParameterConstraints = []},
          Typing.TypeParameter {
            Typing.typeParameterName = (Core.Name "b"),
            Typing.typeParameterConstraints = []}],
        Typing.termSignatureParameters = [
          Typing.Parameter {
            Typing.parameterName = (Core.Name "arg0"),
            Typing.parameterDescription = Nothing,
            Typing.parameterType = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})),
            Typing.parameterIsLazy = False}],
        Typing.termSignatureResult = Typing.Result {
          Typing.resultDescription = Nothing,
          Typing.resultType = (Core.TypeVariable (Core.Name "b"))}},
      Packaging.primitiveDefinitionIsPure = True,
      Packaging.primitiveDefinitionIsTotal = True,
      Packaging.primitiveDefinitionDefaultImplementation = Nothing}
