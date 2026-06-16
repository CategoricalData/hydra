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
      Packaging.moduleName = (Packaging.ModuleName "hydra.decode.testing"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term decoders for hydra.testing"),
        Packaging.entityMetadataComments = [],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
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
          Packaging.termDefinitionMetadata = Nothing,
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.testing.Tag"))}))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "cx"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "raw"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                        Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                          Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                          Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.testing.Tag"))}))})),
                      Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "err"),
                        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                        Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.testing.Tag"))}))}))})),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "stripped"),
                      Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                          Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected wrapped type"))})))),
                              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.testing.Tag"))}))),
                          Core.caseStatementCases = [
                            Core.CaseAlternative {
                              Core.caseAlternativeName = (Core.Name "wrap"),
                              Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "wrappedTerm"),
                                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.WrappedTerm"))),
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.testing.Tag"))})),
                                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "b"),
                                      Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                                      Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.testing.Tag"),
                                        Core.wrappedTermBody = (Core.TermVariable (Core.Name "b"))}))}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "cx"),
                                        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
                                        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "raw"),
                                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                                                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                                                  Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                                                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                                                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                                                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                  Core.lambdaParameter = (Core.Name "err"),
                                                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                                                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                      Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                                                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                                    Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))}))})),
                                              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                Core.lambdaParameter = (Core.Name "stripped"),
                                                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                                                    Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))),
                                                    Core.caseStatementCases = [
                                                      Core.CaseAlternative {
                                                        Core.caseAlternativeName = (Core.Name "literal"),
                                                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                          Core.lambdaParameter = (Core.Name "v"),
                                                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                                              Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                                  Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                                    Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))})))),
                                                                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                                                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))),
                                                              Core.caseStatementCases = [
                                                                Core.CaseAlternative {
                                                                  Core.caseAlternativeName = (Core.Name "string"),
                                                                  Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                    Core.lambdaParameter = (Core.Name "s"),
                                                                    Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                                                                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                                        Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s")))),
                                                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                                                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))}))}]})),
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Decoder for hydra.testing.Tag")))]))}))})]}
