-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.decode.testing

module Hydra.Sources.Decode.Testing where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleDescription = (Just "Term decoders for hydra.testing"),
      Packaging.moduleName = (Packaging.ModuleName "hydra.decode.testing"),
      Packaging.moduleDependencies = [
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.extract.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.lexical"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.rewriting"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testing"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.util"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.core"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.testing.tag"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "cx"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "raw"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                      Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "err"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "stripped"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                          Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected wrapped type"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "wrap"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "wrappedTerm"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "b"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.testing.Tag"),
                                        Core.wrappedTermBody = (Core.TermVariable (Core.Name "b"))}))}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "cx"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "raw"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                                                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                  Core.lambdaParameter = (Core.Name "err"),
                                                  Core.lambdaDomain = Nothing,
                                                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                                              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                Core.lambdaParameter = (Core.Name "stripped"),
                                                Core.lambdaDomain = Nothing,
                                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                                                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                                                    Core.caseStatementCases = [
                                                      Core.Field {
                                                        Core.fieldName = (Core.Name "literal"),
                                                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                                          Core.lambdaParameter = (Core.Name "v"),
                                                          Core.lambdaDomain = Nothing,
                                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                                              Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                                Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                                                              Core.caseStatementCases = [
                                                                Core.Field {
                                                                  Core.fieldName = (Core.Name "string"),
                                                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                                                    Core.lambdaParameter = (Core.Name "s"),
                                                                    Core.lambdaDomain = Nothing,
                                                                    Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s"))))}))}]})),
                                                            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermProject (Core.Projection {
                                        Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
                                        Core.projectionFieldName = (Core.Name "body")})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "wrappedTerm"))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.testing.Tag")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg1"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.testing.Tag"))}))}}))})]}
