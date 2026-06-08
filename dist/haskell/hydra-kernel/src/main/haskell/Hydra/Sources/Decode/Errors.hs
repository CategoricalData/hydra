-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.decode.errors

module Hydra.Sources.Decode.Errors where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.decode.errors"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term decoders for hydra.errors"),
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
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.errors"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.util"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.error.checking"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.error.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.paths"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.typing"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.variants"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.decodingError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))}))}})),
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
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
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
                                                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                                                              Core.caseStatementCases = [
                                                                Core.CaseAlternative {
                                                                  Core.caseAlternativeName = (Core.Name "string"),
                                                                  Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.DecodingError")))]))}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.emptyListError"),
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
                Core.eitherTypeRight = Core.TypeUnit}))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "cx"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "t"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeUnit")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.EmptyListError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.error"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.Error"))}))}})),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "checking"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "checking"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.checking.checkingError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "decoding"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "decoding"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.decodingError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "extraction"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "extraction"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.extractionError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "inference"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "inference"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.inferenceError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidLiteral"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "invalidLiteral"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.invalidLiteralError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "other"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "other"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.otherError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "resolution"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "resolution"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.resolutionError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedField"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "undefinedField"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.undefinedFieldError")),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "untypedTermVariable"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.untypedTermVariableError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unexpectedTermVariant"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "unexpectedTermVariant"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.unexpectedTermVariantError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unexpectedTypeVariant"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "unexpectedTypeVariant"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.core.unexpectedTypeVariantError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unification"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.Error"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "unification"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.unificationError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))))])})),
                                      Core.bindingTypeScheme = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.cases")),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))})),
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
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.Error")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.extractionError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.ExtractionError"))}))}})),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyList"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "emptyList"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.emptyListError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "multipleBindings"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "multipleBindings"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.multipleBindingsError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "multipleFields"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "multipleFields"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.multipleFieldsError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "noMatchingField"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "noMatchingField"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.noMatchingFieldError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "noSuchBinding"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "noSuchBinding"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.noSuchBindingError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "notEnoughCases"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "notEnoughCases"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.notEnoughCasesError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unexpectedShape"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ExtractionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "unexpectedShape"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.unexpectedShapeError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))))])})),
                                      Core.bindingTypeScheme = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.cases")),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))})),
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
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.ExtractionError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.inferenceError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.InferenceError"))}))}})),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "checking"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "checking"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.error.checking.checkingError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "other"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "other"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.otherInferenceError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unification"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.InferenceError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "unification"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.unificationInferenceError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))))])})),
                                      Core.bindingTypeScheme = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.cases")),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))})),
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
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.InferenceError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.multipleBindingsError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.MultipleBindingsError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.errors.MultipleBindingsError"))}))))),
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
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.errors.MultipleBindingsError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "name"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.MultipleBindingsError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.multipleFieldsError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.MultipleFieldsError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.errors.MultipleFieldsError"))}))))),
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
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.errors.MultipleFieldsError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "fieldName"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_fieldName"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.MultipleFieldsError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.noMatchingFieldError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.NoMatchingFieldError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.errors.NoMatchingFieldError"))}))))),
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
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.errors.NoMatchingFieldError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "fieldName"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_fieldName"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.NoMatchingFieldError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.noSuchBindingError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.NoSuchBindingError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.errors.NoSuchBindingError"))}))))),
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
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.errors.NoSuchBindingError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "name"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.NoSuchBindingError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.noSuchPrimitiveError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.NoSuchPrimitiveError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.errors.NoSuchPrimitiveError"))}))))),
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
                                      Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                        Core.recordTypeName = (Core.Name "hydra.errors.NoSuchPrimitiveError"),
                                        Core.recordFields = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "name"),
                                            Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))}]}))))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.NoSuchPrimitiveError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.notEnoughCasesError"),
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
                Core.eitherTypeRight = Core.TypeUnit}))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "cx"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "t"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeUnit")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.NotEnoughCasesError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.otherError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.OtherError"))}))}})),
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
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherError"),
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
                                                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                                                              Core.caseStatementCases = [
                                                                Core.CaseAlternative {
                                                                  Core.caseAlternativeName = (Core.Name "string"),
                                                                  Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.OtherError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.otherInferenceError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.OtherInferenceError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.errors.OtherInferenceError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "path"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_path"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "message"))})),
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
                                                                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                                                                        Core.caseStatementCases = [
                                                                          Core.CaseAlternative {
                                                                            Core.caseAlternativeName = (Core.Name "string"),
                                                                            Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                              Core.lambdaParameter = (Core.Name "s"),
                                                                              Core.lambdaDomain = Nothing,
                                                                              Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s"))))}))}]})),
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
                                          Core.lambdaParameter = (Core.Name "field_message"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.errors.OtherInferenceError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "path"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_path"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "message"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_message"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.OtherInferenceError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.otherResolutionError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.OtherResolutionError"))}))}})),
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
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.OtherResolutionError"),
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
                                                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                                                              Core.caseStatementCases = [
                                                                Core.CaseAlternative {
                                                                  Core.caseAlternativeName = (Core.Name "string"),
                                                                  Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.OtherResolutionError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.resolutionError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.ResolutionError"))}))}})),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "noSuchBinding"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "noSuchBinding"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.noSuchBindingError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "noSuchPrimitive"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "noSuchPrimitive"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.noSuchPrimitiveError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "noMatchingField"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "noMatchingField"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.noMatchingFieldError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "other"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "other"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.otherResolutionError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unexpectedShape"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.errors.ResolutionError"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "unexpectedShape"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.errors.unexpectedShapeError")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))))])})),
                                      Core.bindingTypeScheme = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.cases")),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))})),
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
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.ResolutionError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.unexpectedShapeError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.UnexpectedShapeError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.errors.UnexpectedShapeError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "expected"))})),
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
                                                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                                                                    Core.caseStatementCases = [
                                                                      Core.CaseAlternative {
                                                                        Core.caseAlternativeName = (Core.Name "string"),
                                                                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                          Core.lambdaParameter = (Core.Name "s"),
                                                                          Core.lambdaDomain = Nothing,
                                                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s"))))}))}]})),
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
                                      Core.lambdaParameter = (Core.Name "field_expected"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "actual"))})),
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
                                                                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                                                                        Core.caseStatementCases = [
                                                                          Core.CaseAlternative {
                                                                            Core.caseAlternativeName = (Core.Name "string"),
                                                                            Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                              Core.lambdaParameter = (Core.Name "s"),
                                                                              Core.lambdaDomain = Nothing,
                                                                              Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s"))))}))}]})),
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
                                          Core.lambdaParameter = (Core.Name "field_actual"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.errors.UnexpectedShapeError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "expected"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_expected"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "actual"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_actual"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.UnexpectedShapeError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.unificationError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.UnificationError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.errors.UnificationError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "leftType"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.type"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_leftType"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "rightType"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.type"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_rightType"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "message"))})),
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
                                                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                                                                            Core.caseStatementCases = [
                                                                              Core.CaseAlternative {
                                                                                Core.caseAlternativeName = (Core.Name "string"),
                                                                                Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                                  Core.lambdaParameter = (Core.Name "s"),
                                                                                  Core.lambdaDomain = Nothing,
                                                                                  Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s"))))}))}]})),
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
                                              Core.lambdaParameter = (Core.Name "field_message"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                                Core.recordTypeName = (Core.Name "hydra.errors.UnificationError"),
                                                Core.recordFields = [
                                                  Core.Field {
                                                    Core.fieldName = (Core.Name "leftType"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "field_leftType"))},
                                                  Core.Field {
                                                    Core.fieldName = (Core.Name "rightType"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "field_rightType"))},
                                                  Core.Field {
                                                    Core.fieldName = (Core.Name "message"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "field_message"))}]}))))}))}))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.UnificationError")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.errors.unificationInferenceError"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.errors.UnificationInferenceError"))}))}})),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a record of type hydra.errors.UnificationInferenceError"))}))))),
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
                                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "path"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.paths.subtermPath"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "field_path"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "cause"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.errors.unificationError"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                        Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_cause"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.errors.UnificationInferenceError"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "path"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_path"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "cause"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_cause"))}]}))))}))}))}))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Decoder for hydra.errors.UnificationInferenceError")))]))}))}))]}
