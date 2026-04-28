-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.encode.packaging

module Hydra.Sources.Encode.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleDescription = (Just "Term encoders for hydra.packaging"),
      Packaging.moduleNamespace = (Packaging.Namespace "hydra.encode.packaging"),
      Packaging.moduleTermDependencies = [
        Packaging.Namespace "hydra.encode.core",
        (Packaging.Namespace "hydra.encode.graph")],
      Packaging.moduleTypeDependencies = [
        Packaging.Namespace "hydra.packaging"],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.definition"),
          Packaging.termDefinitionTerm = (Core.TermCases (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.packaging.Definition"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "term"),
                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "inject"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.Definition"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "term"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.packaging.termDefinition")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "type"),
                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "inject"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.Definition"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "type"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.packaging.typeDefinition")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.packaging.Definition")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.fileExtension"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.FileExtension"))}))},
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
                          Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.FileExtension")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.packaging.FileExtension")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.module"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "record"),
                Core.fieldTerm = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "hydra.core.Record"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "typeName"),
                      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.Module"))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "fields"),
                      Core.fieldTerm = (Core.TermList [
                        Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "description"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "opt"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "maybe"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
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
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
                                    Core.projectionField = (Core.Name "description")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "namespace"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.packaging.namespace")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
                                    Core.projectionField = (Core.Name "namespace")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "termDependencies"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.packaging.namespace"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
                                    Core.projectionField = (Core.Name "termDependencies")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeDependencies"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.packaging.namespace"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
                                    Core.projectionField = (Core.Name "typeDependencies")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "definitions"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.packaging.definition"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
                                    Core.projectionField = (Core.Name "definitions")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.packaging.Module")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.namespace"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.Namespace"))}))},
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
                          Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.Namespace")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.packaging.Namespace")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.namespaces"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "n"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermInject (Core.Injection {
                Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "record"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.core.Record"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "typeName"),
                        Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.Namespaces"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "fields"),
                        Core.fieldTerm = (Core.TermList [
                          Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "focus"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "p"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                      Core.injectionField = Core.Field {
                                        Core.fieldName = (Core.Name "pair"),
                                        Core.fieldTerm = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.bimap")),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.packaging.namespace"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
                                      Core.projectionField = (Core.Name "focus")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "mapping"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "m"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                      Core.injectionField = Core.Field {
                                        Core.fieldName = (Core.Name "map"),
                                        Core.fieldTerm = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.bimap")),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.packaging.namespace"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
                                      Core.projectionField = (Core.Name "mapping")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [
              Core.Name "n"],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "n")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.packaging.Namespaces")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "n"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.package"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "record"),
                Core.fieldTerm = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "hydra.core.Record"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "typeName"),
                      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.Package"))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "fields"),
                      Core.fieldTerm = (Core.TermList [
                        Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "name"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.packaging.packageName")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
                                    Core.projectionField = (Core.Name "name")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "modules"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.packaging.module"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
                                    Core.projectionField = (Core.Name "modules")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "dependencies"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.packaging.packageName"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
                                    Core.projectionField = (Core.Name "dependencies")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "description"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "opt"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "maybe"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
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
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
                                    Core.projectionField = (Core.Name "description")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.packaging.Package")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.packageName"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.PackageName"))}))},
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
                          Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.PackageName")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.packaging.PackageName")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.qualifiedName"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "record"),
                Core.fieldTerm = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "hydra.core.Record"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "typeName"),
                      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.QualifiedName"))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "fields"),
                      Core.fieldTerm = (Core.TermList [
                        Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "namespace"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "opt"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "maybe"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.packaging.namespace"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
                                    Core.projectionField = (Core.Name "namespace")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "local"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
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
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
                                    Core.projectionField = (Core.Name "local")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.packaging.QualifiedName")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.termDefinition"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "record"),
                Core.fieldTerm = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "hydra.core.Record"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "typeName"),
                      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.TermDefinition"))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "fields"),
                      Core.fieldTerm = (Core.TermList [
                        Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "name"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
                                    Core.projectionField = (Core.Name "name")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
                                    Core.projectionField = (Core.Name "term")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeScheme"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "opt"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "maybe"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.typeScheme"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
                                    Core.projectionField = (Core.Name "typeScheme")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.packaging.TermDefinition")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.packaging.typeDefinition"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "record"),
                Core.fieldTerm = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "hydra.core.Record"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "typeName"),
                      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.packaging.TypeDefinition"))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "fields"),
                      Core.fieldTerm = (Core.TermList [
                        Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "name"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
                                    Core.projectionField = (Core.Name "name")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeScheme"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.typeScheme")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
                                    Core.projectionField = (Core.Name "typeScheme")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.packaging.TypeDefinition")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}))]}
