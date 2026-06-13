-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.decode.yaml.model

module Hydra.Sources.Decode.Yaml.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.decode.yaml.model"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term decoders for hydra.yaml.model"),
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
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.yaml.model"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.util"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.core"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.yaml.model.node"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.yaml.model.Node"))}))}})),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "mapping"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "mapping"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeMap")),
                                                        Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.yaml.model.node"))})),
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.yaml.model.node"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "scalar"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "scalar"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.yaml.model.scalar")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "sequence"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Node"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "sequence"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeList")),
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.yaml.model.node"))})),
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
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Decoder for hydra.yaml.model.Node")))]))}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.yaml.model.scalar"),
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
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.yaml.model.Scalar"))}))}})),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bool"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "bool"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
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
                                                            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "decimal"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "decimal"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
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
                                                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected decimal literal"))}))))),
                                                                            Core.caseStatementCases = [
                                                                              Core.CaseAlternative {
                                                                                Core.caseAlternativeName = (Core.Name "decimal"),
                                                                                Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                                  Core.lambdaParameter = (Core.Name "d"),
                                                                                  Core.lambdaDomain = Nothing,
                                                                                  Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "d"))))}))}]})),
                                                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                                                                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                                                              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "float"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
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
                                                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected float64 literal"))}))))),
                                                                            Core.caseStatementCases = [
                                                                              Core.CaseAlternative {
                                                                                Core.caseAlternativeName = (Core.Name "float"),
                                                                                Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                                                                  Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
                                                                                  Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                                                    Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected float64 value"))}))))),
                                                                                  Core.caseStatementCases = [
                                                                                    Core.CaseAlternative {
                                                                                      Core.caseAlternativeName = (Core.Name "float64"),
                                                                                      Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                                        Core.lambdaParameter = (Core.Name "f"),
                                                                                        Core.lambdaDomain = Nothing,
                                                                                        Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "f"))))}))}]}))}]})),
                                                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                                                                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                                                              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "int"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
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
                                                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected bigint literal"))}))))),
                                                                            Core.caseStatementCases = [
                                                                              Core.CaseAlternative {
                                                                                Core.caseAlternativeName = (Core.Name "integer"),
                                                                                Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                                                                  Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                                                  Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                                                    Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected bigint value"))}))))),
                                                                                  Core.caseStatementCases = [
                                                                                    Core.CaseAlternative {
                                                                                      Core.caseAlternativeName = (Core.Name "bigint"),
                                                                                      Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                                                                        Core.lambdaParameter = (Core.Name "i"),
                                                                                        Core.lambdaDomain = Nothing,
                                                                                        Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                                                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                                                                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                                                              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "null"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "null"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
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
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "str"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.yaml.model.Scalar"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "str"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
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
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Decoder for hydra.yaml.model.Scalar")))]))}))}))]}
