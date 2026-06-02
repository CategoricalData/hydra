-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.decode.error.packaging

module Hydra.Sources.Decode.Error.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.decode.error.packaging"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term decoders for hydra.error.packaging"),
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
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.error.packaging"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.util"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.packaging"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.util"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.conflictingModuleNameError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "first"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_first"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "second"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_second"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingModuleNameError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "first"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_first"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "second"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_second"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.ConflictingModuleNameError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.ConflictingModuleNameError"))}))}}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.conflictingVariantNameError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "moduleName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_moduleName"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "typeName"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_typeName"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "variantName"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                            Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "field_variantName"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "conflictingName"))})),
                                                        Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                  Core.lambdaParameter = (Core.Name "field_conflictingName"),
                                                  Core.lambdaDomain = Nothing,
                                                  Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                                    Core.recordTypeName = (Core.Name "hydra.error.packaging.ConflictingVariantNameError"),
                                                    Core.recordFields = [
                                                      Core.Field {
                                                        Core.fieldName = (Core.Name "moduleName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "field_moduleName"))},
                                                      Core.Field {
                                                        Core.fieldName = (Core.Name "typeName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "field_typeName"))},
                                                      Core.Field {
                                                        Core.fieldName = (Core.Name "variantName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "field_variantName"))},
                                                      Core.Field {
                                                        Core.fieldName = (Core.Name "conflictingName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "field_conflictingName"))}]}))))}))}))}))}))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.ConflictingVariantNameError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.ConflictingVariantNameError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.definitionNotInModuleNameError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "moduleName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_moduleName"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "name"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_name"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "moduleName"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_moduleName"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.DefinitionNotInModuleNameError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.definitionsOutOfOrderError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "moduleName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_moduleName"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "precedingName"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_precedingName"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "followingName"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                            Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "field_followingName"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                                Core.recordTypeName = (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"),
                                                Core.recordFields = [
                                                  Core.Field {
                                                    Core.fieldName = (Core.Name "moduleName"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "field_moduleName"))},
                                                  Core.Field {
                                                    Core.fieldName = (Core.Name "precedingName"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "field_precedingName"))},
                                                  Core.Field {
                                                    Core.fieldName = (Core.Name "followingName"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "field_followingName"))}]}))))}))}))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.DefinitionsOutOfOrderError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.duplicateDefinitionNameError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "moduleName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_moduleName"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "name"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_name"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "moduleName"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_moduleName"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.DuplicateDefinitionNameError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.duplicateModuleNameError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "moduleName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_moduleName"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.packaging.DuplicateModuleNameError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "moduleName"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_moduleName"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.DuplicateModuleNameError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.DuplicateModuleNameError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.invalidDefinitionNameError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "moduleName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_moduleName"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "name"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_name"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "expectedConvention"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.util.caseConvention"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                            Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "field_expectedConvention"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                                Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"),
                                                Core.recordFields = [
                                                  Core.Field {
                                                    Core.fieldName = (Core.Name "moduleName"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "field_moduleName"))},
                                                  Core.Field {
                                                    Core.fieldName = (Core.Name "name"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))},
                                                  Core.Field {
                                                    Core.fieldName = (Core.Name "expectedConvention"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "field_expectedConvention"))}]}))))}))}))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.InvalidDefinitionNameError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.InvalidDefinitionNameError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.invalidModuleError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "inject"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "inj"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermLet (Core.Let {
                                  Core.letBindings = [
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "field"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermProject (Core.Projection {
                                          Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                          Core.projectionFieldName = (Core.Name "field")})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                                      Core.bindingTypeScheme = Nothing},
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "fname"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermProject (Core.Projection {
                                          Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                          Core.projectionFieldName = (Core.Name "name")})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                      Core.bindingTypeScheme = Nothing},
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "fterm"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermProject (Core.Projection {
                                          Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                          Core.projectionFieldName = (Core.Name "term")})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                      Core.bindingTypeScheme = Nothing},
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "variantMap"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
                                        Core.applicationArgument = (Core.TermList [
                                          Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "conflictingVariantName"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "conflictingVariantName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.conflictingVariantNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "definitionNotInModuleName"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "definitionNotInModuleName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.definitionNotInModuleNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "definitionsOutOfOrder"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "definitionsOutOfOrder"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.definitionsOutOfOrderError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateDefinitionName"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "duplicateDefinitionName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.duplicateDefinitionNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidDefinitionName"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidDefinitionName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.invalidDefinitionNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidModuleNameConvention"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidModuleNameConvention"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.invalidModuleNameConventionError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "missingDocumentation"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidModuleError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "missingDocumentation"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.missingDocumentationError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))))])})),
                                      Core.bindingTypeScheme = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                                        Core.applicationArgument = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                          Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat")),
                                            Core.applicationArgument = (Core.TermList [
                                              Core.TermLiteral (Core.LiteralString "no such field "),
                                              (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.core.Name")),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                              (Core.TermLiteral (Core.LiteralString " in union"))])}))}))))})),
                                      Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "f"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))}))})),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.InvalidModuleError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.InvalidModuleError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.invalidModuleNameConventionError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "moduleName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_moduleName"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "moduleName"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_moduleName"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.InvalidModuleNameConventionError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.invalidPackageError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "inject"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "inj"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermLet (Core.Let {
                                  Core.letBindings = [
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "field"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermProject (Core.Projection {
                                          Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                          Core.projectionFieldName = (Core.Name "field")})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                                      Core.bindingTypeScheme = Nothing},
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "fname"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermProject (Core.Projection {
                                          Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                          Core.projectionFieldName = (Core.Name "name")})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                      Core.bindingTypeScheme = Nothing},
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "fterm"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermProject (Core.Projection {
                                          Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                          Core.projectionFieldName = (Core.Name "term")})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                      Core.bindingTypeScheme = Nothing},
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "variantMap"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
                                        Core.applicationArgument = (Core.TermList [
                                          Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "conflictingModuleName"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "conflictingModuleName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.conflictingModuleNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateModuleName"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "duplicateModuleName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.duplicateModuleNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidModule"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidModule"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.invalidModuleError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidPackageName"))}),
                                            (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "input"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "t"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.error.packaging.InvalidPackageError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidPackageName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.packaging.invalidPackageNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))))])})),
                                      Core.bindingTypeScheme = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                                        Core.applicationArgument = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                          Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat")),
                                            Core.applicationArgument = (Core.TermList [
                                              Core.TermLiteral (Core.LiteralString "no such field "),
                                              (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.core.Name")),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                              (Core.TermLiteral (Core.LiteralString " in union"))])}))}))))})),
                                      Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "f"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))}))})),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.InvalidPackageError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.InvalidPackageError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.invalidPackageNameError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "packageName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.packageName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_packageName"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.packaging.InvalidPackageNameError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "packageName"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_packageName"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.InvalidPackageNameError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.InvalidPackageNameError"))}))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.packaging.missingDocumentationError"),
          Packaging.termDefinitionMetadata = Nothing,
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "moduleName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_moduleName"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "name"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_name"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.packaging.MissingDocumentationError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "moduleName"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_moduleName"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (
                Core.Name "description",
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.packaging.MissingDocumentationError")))])})),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.packaging.MissingDocumentationError"))}))}}))}))]}
