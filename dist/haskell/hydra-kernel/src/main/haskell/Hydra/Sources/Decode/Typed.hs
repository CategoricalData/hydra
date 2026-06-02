-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.decode.typed

module Hydra.Sources.Decode.Typed where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.decode.typed"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term decoders for hydra.typed"),
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
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.typed"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.util"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.core"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.typed.typedBinding"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [
              Typing.TypeParameter {
                Typing.typeParameterName = (Core.Name "a"),
                Typing.typeParameterConstraints = []}],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                    Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeVariable (Core.Name "a"))}))}))})),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg1"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg2"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                Core.eitherTypeRight = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedBinding")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))}))}))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "a"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
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
                                                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "term"))})),
                                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.typed.typedTerm")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "field_term"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                              Core.recordTypeName = (Core.Name "hydra.typed.TypedBinding"),
                                              Core.recordFields = [
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "name"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))},
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "term"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "field_term"))}]}))))}))}))}))}))}))}))}]})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.typed.TypedBinding")))])}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.typed.typedTerm"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [
              Typing.TypeParameter {
                Typing.typeParameterName = (Core.Name "a"),
                Typing.typeParameterConstraints = []}],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                    Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeVariable (Core.Name "a"))}))}))})),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg1"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg2"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                Core.eitherTypeRight = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))}))}))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "a"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
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
                              Core.CaseAlternative {
                                Core.caseAlternativeName = (Core.Name "wrap"),
                                Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "wrappedTerm"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                      Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "b"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                                          Core.wrappedTermTypeName = (Core.Name "hydra.typed.TypedTerm"),
                                          Core.wrappedTermBody = (Core.TermVariable (Core.Name "b"))}))}))})),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.core.term")),
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
                      Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.typed.TypedTerm")))])}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.typed.typedTermDefinition"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [
              Typing.TypeParameter {
                Typing.typeParameterName = (Core.Name "a"),
                Typing.typeParameterConstraints = []}],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                    Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeVariable (Core.Name "a"))}))}))})),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg1"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg2"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                Core.eitherTypeRight = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTermDefinition")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))}))}))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "a"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
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
                                                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "term"))})),
                                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.typed.typedTerm")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "field_term"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                              Core.recordTypeName = (Core.Name "hydra.typed.TypedTermDefinition"),
                                              Core.recordFields = [
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "name"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))},
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "term"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "field_term"))}]}))))}))}))}))}))}))}))}]})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.typed.TypedTermDefinition")))])}))}))]}
