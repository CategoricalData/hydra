-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.encode.testing

module Hydra.Sources.Encode.Testing where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleDescription = (Just "Term encoders for hydra.testing"),
      Packaging.moduleNamespace = (Packaging.Namespace "hydra.encode.testing"),
      Packaging.moduleDependencies = [
        Packaging.Namespace "hydra.encode.core",
        (Packaging.Namespace "hydra.testing")],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.testing.tag"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermInject (Core.Injection {
                Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "wrap"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "typeName"),
                        Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.testing.Tag"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "body"),
                        Core.fieldTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermInject (Core.Injection {
                              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                              Core.injectionField = Core.Field {
                                Core.fieldName = (Core.Name "literal"),
                                Core.fieldTerm = (Core.TermInject (Core.Injection {
                                  Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                  Core.injectionField = Core.Field {
                                    Core.fieldName = (Core.Name "string"),
                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.testing.Tag")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.testing.Tag")))])})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.testing.Tag")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})]}
