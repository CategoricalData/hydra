-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.decode.haskell.environment

module Hydra.Sources.Decode.Haskell.Environment where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.decode.haskell.environment"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term decoders for hydra.haskell.environment"),
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
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.haskell.environment"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.util"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.haskell.environment.haskellModuleMetadata"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"))}))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.haskell.environment.HaskellModuleMetadata"))}))))),
                          Core.caseStatementCases = [
                            Core.CaseAlternative {
                              Core.caseAlternativeName = (Core.Name "record"),
                              Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "record"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermLet (Core.Let {
                                  Core.letBindings = [
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "fieldMap"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.toFieldMap")),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "record"))})),
                                      Core.bindingTypeScheme = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "usesByteString"))})),
                                            Core.applicationArgument = (Core.TermLambda (Core.Lambda {
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
                                                            Core.CaseAlternative {
                                                              Core.caseAlternativeName = (Core.Name "literal"),
                                                              Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                Core.lambdaParameter = (Core.Name "v"),
                                                                Core.lambdaDomain = Nothing,
                                                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                                                    Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                                                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected boolean literal"))}))))),
                                                                    Core.caseStatementCases = [
                                                                      Core.CaseAlternative {
                                                                        Core.caseAlternativeName = (Core.Name "boolean"),
                                                                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                          Core.lambdaParameter = (Core.Name "b"),
                                                                          Core.lambdaDomain = Nothing,
                                                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b"))))}))}]})),
                                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                                                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_usesByteString"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "usesInt"))})),
                                                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
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
                                                                Core.CaseAlternative {
                                                                  Core.caseAlternativeName = (Core.Name "literal"),
                                                                  Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                    Core.lambdaParameter = (Core.Name "v"),
                                                                    Core.lambdaDomain = Nothing,
                                                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                                      Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                                                        Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                                                        Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected boolean literal"))}))))),
                                                                        Core.caseStatementCases = [
                                                                          Core.CaseAlternative {
                                                                            Core.caseAlternativeName = (Core.Name "boolean"),
                                                                            Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                              Core.lambdaParameter = (Core.Name "b"),
                                                                              Core.lambdaDomain = Nothing,
                                                                              Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b"))))}))}]})),
                                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                                                            Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                                                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                                                          Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                        Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_usesInt"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "usesMap"))})),
                                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
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
                                                                    Core.CaseAlternative {
                                                                      Core.caseAlternativeName = (Core.Name "literal"),
                                                                      Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                        Core.lambdaParameter = (Core.Name "v"),
                                                                        Core.lambdaDomain = Nothing,
                                                                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                                          Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                                                            Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                                                            Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                                              Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected boolean literal"))}))))),
                                                                            Core.caseStatementCases = [
                                                                              Core.CaseAlternative {
                                                                                Core.caseAlternativeName = (Core.Name "boolean"),
                                                                                Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                                  Core.lambdaParameter = (Core.Name "b"),
                                                                                  Core.lambdaDomain = Nothing,
                                                                                  Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b"))))}))}]})),
                                                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                                                                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                                                              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                            Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "field_usesMap"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "usesSet"))})),
                                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
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
                                                                        Core.CaseAlternative {
                                                                          Core.caseAlternativeName = (Core.Name "literal"),
                                                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                            Core.lambdaParameter = (Core.Name "v"),
                                                                            Core.lambdaDomain = Nothing,
                                                                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                                                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                                                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected boolean literal"))}))))),
                                                                                Core.caseStatementCases = [
                                                                                  Core.CaseAlternative {
                                                                                    Core.caseAlternativeName = (Core.Name "boolean"),
                                                                                    Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                                      Core.lambdaParameter = (Core.Name "b"),
                                                                                      Core.lambdaDomain = Nothing,
                                                                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b"))))}))}]})),
                                                                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                                                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))})),
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                  Core.lambdaParameter = (Core.Name "field_usesSet"),
                                                  Core.lambdaDomain = Nothing,
                                                  Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                                    Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
                                                    Core.recordFields = [
                                                      Core.Field {
                                                        Core.fieldName = (Core.Name "usesByteString"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "field_usesByteString"))},
                                                      Core.Field {
                                                        Core.fieldName = (Core.Name "usesInt"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "field_usesInt"))},
                                                      Core.Field {
                                                        Core.fieldName = (Core.Name "usesMap"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "field_usesMap"))},
                                                      Core.Field {
                                                        Core.fieldName = (Core.Name "usesSet"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "field_usesSet"))}]}))))}))}))}))}))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.haskell.environment.HaskellModuleMetadata")))]))}))})]}
