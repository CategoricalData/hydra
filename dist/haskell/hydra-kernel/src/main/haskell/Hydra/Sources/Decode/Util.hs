-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.decode.util

module Hydra.Sources.Decode.Util where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Scoping as Scoping
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleDescription = (Just "Term decoders for hydra.util"),
      Packaging.moduleName = (Packaging.ModuleName "hydra.decode.util"),
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
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.util"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.util"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.decode.packaging"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.util.caseConvention"),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "camel"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "camel"),
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
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "pascal"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "pascal"),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "lowerSnake"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "lowerSnake"),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "upperSnake"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "upperSnake"),
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
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.util.CaseConvention")))])})),
          Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.util.CaseConvention"))}))}))})),
            Core.typeSchemeConstraints = Nothing})))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.util.comparison"),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "lessThan"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "lessThan"),
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
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "equalTo"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "equalTo"),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "greaterThan"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "greaterThan"),
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
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.util.Comparison")))])})),
          Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.util.Comparison"))}))}))})),
            Core.typeSchemeConstraints = Nothing})))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.util.namespaces"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "n"),
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
                                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "focus"))})),
                                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodePair")),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "n"))}))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                      Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "field_focus"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "mapping"))})),
                                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeMap")),
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.packaging.moduleName"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "n"))}))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "field_mapping"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                              Core.recordTypeName = (Core.Name "hydra.util.Namespaces"),
                                              Core.recordFields = [
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "focus"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "field_focus"))},
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "mapping"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "field_mapping"))}]}))))}))}))}))}))}))}))}]})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.util.Namespaces")))])})),
          Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
            Core.typeSchemeVariables = [
              Core.Name "n"],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                  Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                    Core.eitherTypeRight = (Core.TypeVariable (Core.Name "n"))}))}))})),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                  Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                    Core.eitherTypeRight = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.util.Namespaces")),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "n"))}))}))}))}))})),
            Core.typeSchemeConstraints = Nothing})))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.util.precision"),
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
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "arbitrary"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "arbitrary"),
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
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))}))),
                                          (Core.TermPair (
                                            Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bits"))}),
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
                                                      Core.injectionTypeName = (Core.Name "hydra.util.Precision"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "bits"),
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
                                                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected int32 literal"))}))))),
                                                                            Core.caseStatementCases = [
                                                                              Core.Field {
                                                                                Core.fieldName = (Core.Name "integer"),
                                                                                Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                                                                  Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                                                  Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                                                    Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                                                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected int32 value"))}))))),
                                                                                  Core.caseStatementCases = [
                                                                                    Core.Field {
                                                                                      Core.fieldName = (Core.Name "int32"),
                                                                                      Core.fieldTerm = (Core.TermLambda (Core.Lambda {
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
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Decoder for hydra.util.Precision")))])})),
          Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.util.Precision"))}))}))})),
            Core.typeSchemeConstraints = Nothing})))}))]}
