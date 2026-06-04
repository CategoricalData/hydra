-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.decode.error.core

module Hydra.Sources.Decode.Error.Core where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.decode.error.core"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term decoders for hydra.error.core"),
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
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.error.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.util"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.paths"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.variants"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.constantConditionError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.ConstantConditionError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.ConstantConditionError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "value"))})),
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
                                          Core.lambdaParameter = (Core.Name "field_value"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "value"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_value"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.ConstantConditionError")))]))}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.duplicateBindingError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateBindingError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.DuplicateBindingError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.DuplicateBindingError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.duplicateFieldError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateFieldError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.DuplicateFieldError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.DuplicateFieldError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.duplicateRecordTypeFieldNamesError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.DuplicateRecordTypeFieldNamesError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.DuplicateRecordTypeFieldNamesError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.duplicateUnionTypeFieldNamesError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.DuplicateUnionTypeFieldNamesError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.DuplicateUnionTypeFieldNamesError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.emptyCaseStatementError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyCaseStatementError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.EmptyCaseStatementError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "typeName"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_typeName"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.EmptyCaseStatementError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.emptyLetBindingsError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyLetBindingsError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.EmptyLetBindingsError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.EmptyLetBindingsError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.emptyRecordTypeError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyRecordTypeError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.EmptyRecordTypeError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.EmptyRecordTypeError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.emptyTermAnnotationError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyTermAnnotationError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.EmptyTermAnnotationError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.EmptyTermAnnotationError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.emptyTypeAnnotationError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyTypeAnnotationError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.EmptyTypeAnnotationError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.EmptyTypeAnnotationError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.emptyTypeNameInTermError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyTypeNameInTermError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.EmptyTypeNameInTermError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.EmptyTypeNameInTermError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.emptyUnionTypeError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyUnionTypeError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.EmptyUnionTypeError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.EmptyUnionTypeError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.invalidForallParameterNameError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidForallParameterNameError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.InvalidForallParameterNameError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.InvalidForallParameterNameError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.invalidLambdaParameterNameError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.InvalidLambdaParameterNameError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.InvalidLambdaParameterNameError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.invalidLetBindingNameError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidLetBindingNameError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.InvalidLetBindingNameError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.InvalidLetBindingNameError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.invalidLiteralError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidLiteralError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union"))}))))),
                          Core.caseStatementCases = [
                            Core.CaseAlternative {
                              Core.caseAlternativeName = (Core.Name "inject"),
                              Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeMismatch"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidLiteralError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "typeMismatch"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.literalTypeMismatchError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))])})),
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.InvalidLiteralError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.invalidTermError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidTermError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union"))}))))),
                          Core.caseStatementCases = [
                            Core.CaseAlternative {
                              Core.caseAlternativeName = (Core.Name "inject"),
                              Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "constantCondition"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "constantCondition"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.constantConditionError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateBinding"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "duplicateBinding"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.duplicateBindingError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateField"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "duplicateField"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.duplicateFieldError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyCaseStatement"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "emptyCaseStatement"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.emptyCaseStatementError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyLetBindings"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "emptyLetBindings"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.emptyLetBindingsError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyTermAnnotation"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "emptyTermAnnotation"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.emptyTermAnnotationError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyTypeNameInTerm"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "emptyTypeNameInTerm"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.emptyTypeNameInTermError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidLambdaParameterName"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidLambdaParameterName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.invalidLambdaParameterNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidLetBindingName"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidLetBindingName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.invalidLetBindingNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidTypeLambdaParameterName"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidTypeLambdaParameterName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.invalidTypeLambdaParameterNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nestedTermAnnotation"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "nestedTermAnnotation"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.nestedTermAnnotationError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "redundantWrapUnwrap"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "redundantWrapUnwrap"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.redundantWrapUnwrapError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "selfApplication"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "selfApplication"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.selfApplicationError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "termVariableShadowing"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "termVariableShadowing"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.termVariableShadowingError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeVariableShadowingInTypeLambda"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "typeVariableShadowingInTypeLambda"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.typeVariableShadowingInTypeLambdaError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTermVariable"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "undefinedTermVariable"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.undefinedTermVariableError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTypeVariableInBindingType"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "undefinedTypeVariableInBindingType"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.undefinedTypeVariableInBindingTypeError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTypeVariableInLambdaDomain"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "undefinedTypeVariableInLambdaDomain"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.undefinedTypeVariableInLambdaDomainError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTypeVariableInTypeApplication"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "undefinedTypeVariableInTypeApplication"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.undefinedTypeVariableInTypeApplicationError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unknownPrimitiveName"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "unknownPrimitiveName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.unknownPrimitiveNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unnecessaryIdentityApplication"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "unnecessaryIdentityApplication"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.unnecessaryIdentityApplicationError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "untypedTermVariable"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "untypedTermVariable"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.untypedTermVariableError")),
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.InvalidTermError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.invalidTypeError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidTypeError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union"))}))))),
                          Core.caseStatementCases = [
                            Core.CaseAlternative {
                              Core.caseAlternativeName = (Core.Name "inject"),
                              Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateRecordTypeFieldNames"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "duplicateRecordTypeFieldNames"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.duplicateRecordTypeFieldNamesError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateUnionTypeFieldNames"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "duplicateUnionTypeFieldNames"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.duplicateUnionTypeFieldNamesError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyRecordType"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "emptyRecordType"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.emptyRecordTypeError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyTypeAnnotation"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "emptyTypeAnnotation"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.emptyTypeAnnotationError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyUnionType"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "emptyUnionType"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.emptyUnionTypeError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidForallParameterName"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidForallParameterName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.invalidForallParameterNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidTypeSchemeVariableName"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidTypeSchemeVariableName"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.invalidTypeSchemeVariableNameError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nestedTypeAnnotation"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "nestedTypeAnnotation"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.nestedTypeAnnotationError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nonComparableMapKeyType"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "nonComparableMapKeyType"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.nonComparableMapKeyTypeError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nonComparableSetElementType"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "nonComparableSetElementType"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.nonComparableSetElementTypeError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "singleVariantUnion"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "singleVariantUnion"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.singleVariantUnionError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeVariableShadowingInForall"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "typeVariableShadowingInForall"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.typeVariableShadowingInForallError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTypeVariable"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "undefinedTypeVariable"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.undefinedTypeVariableError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "voidInNonBottomPosition"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "voidInNonBottomPosition"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.voidInNonBottomPositionError")),
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.InvalidTypeError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.invalidTypeLambdaParameterNameError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.InvalidTypeLambdaParameterNameError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.InvalidTypeLambdaParameterNameError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.invalidTypeSchemeVariableNameError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.InvalidTypeSchemeVariableNameError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.InvalidTypeSchemeVariableNameError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.literalTypeMismatchError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.LiteralTypeMismatchError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.LiteralTypeMismatchError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "expectedType"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.literalType"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_expectedType"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "actualType"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.literalType"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_actualType"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.LiteralTypeMismatchError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "expectedType"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_expectedType"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "actualType"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_actualType"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.LiteralTypeMismatchError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.nestedTermAnnotationError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.NestedTermAnnotationError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.NestedTermAnnotationError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.NestedTermAnnotationError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.nestedTypeAnnotationError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.NestedTypeAnnotationError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.NestedTypeAnnotationError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.NestedTypeAnnotationError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.nonComparableMapKeyTypeError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.NonComparableMapKeyTypeError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "keyType"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.type"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_keyType"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "keyType"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_keyType"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.NonComparableMapKeyTypeError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.nonComparableSetElementTypeError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.NonComparableSetElementTypeError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.NonComparableSetElementTypeError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "elementType"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.type"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_elementType"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "elementType"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_elementType"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.NonComparableSetElementTypeError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.redundantWrapUnwrapError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.RedundantWrapUnwrapError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.RedundantWrapUnwrapError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "typeName"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_typeName"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.RedundantWrapUnwrapError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.selfApplicationError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.SelfApplicationError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.SelfApplicationError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.SelfApplicationError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.singleVariantUnionError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.SingleVariantUnionError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.SingleVariantUnionError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "fieldName"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_fieldName"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "fieldName"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_fieldName"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.SingleVariantUnionError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.termVariableShadowingError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.TermVariableShadowingError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.TermVariableShadowingError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.TermVariableShadowingError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.typeVariableShadowingInForallError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.TypeVariableShadowingInForallError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.TypeVariableShadowingInForallError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.typeVariableShadowingInTypeLambdaError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.TypeVariableShadowingInTypeLambdaError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.TypeVariableShadowingInTypeLambdaError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.undefinedFieldError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedFieldError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UndefinedFieldError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "fieldName"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_fieldName"),
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
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "fieldName"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_fieldName"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "typeName"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_typeName"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UndefinedFieldError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.undefinedTermVariableError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTermVariableError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UndefinedTermVariableError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UndefinedTermVariableError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.undefinedTypeVariableError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTypeVariableError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UndefinedTypeVariableError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UndefinedTypeVariableError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.undefinedTypeVariableInBindingTypeError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UndefinedTypeVariableInBindingTypeError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UndefinedTypeVariableInBindingTypeError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.undefinedTypeVariableInLambdaDomainError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UndefinedTypeVariableInLambdaDomainError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UndefinedTypeVariableInLambdaDomainError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.undefinedTypeVariableInTypeApplicationError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UndefinedTypeVariableInTypeApplicationError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UndefinedTypeVariableInTypeApplicationError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.unexpectedTermVariantError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UnexpectedTermVariantError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UnexpectedTermVariantError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "expectedVariant"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.variants.termVariant"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_expectedVariant"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "actualTerm"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_actualTerm"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "expectedVariant"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_expectedVariant"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "actualTerm"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_actualTerm"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UnexpectedTermVariantError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.unexpectedTypeVariantError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UnexpectedTypeVariantError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UnexpectedTypeVariantError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "expectedVariant"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.variants.typeVariant"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_expectedVariant"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "actualType"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.type"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_actualType"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "expectedVariant"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_expectedVariant"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "actualType"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_actualType"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UnexpectedTypeVariantError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.unknownPrimitiveNameError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UnknownPrimitiveNameError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UnknownPrimitiveNameError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UnknownPrimitiveNameError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.unnecessaryIdentityApplicationError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UnnecessaryIdentityApplicationError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UnnecessaryIdentityApplicationError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.untypedTermVariableError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.UntypedTermVariableError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.UntypedTermVariableError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
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
                                            Core.recordTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "location"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.UntypedTermVariableError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.error.core.voidInNonBottomPositionError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.core.VoidInNonBottomPositionError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.error.core.VoidInNonBottomPositionError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "location"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_location"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "location"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_location"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.error.core.VoidInNonBottomPositionError")))]))}))}))]}
