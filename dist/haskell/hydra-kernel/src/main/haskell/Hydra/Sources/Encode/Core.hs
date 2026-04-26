-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.encode.core

module Hydra.Sources.Encode.Core where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleNamespace = (Packaging.Namespace "hydra.encode.core"),
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.annotatedTerm"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.AnnotatedTerm"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
                                    Core.projectionField = (Core.Name "body")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotation"))}))},
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
                                    Core.projectionField = (Core.Name "annotation")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.AnnotatedTerm")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.annotatedType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.AnnotatedType"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
                                    Core.projectionField = (Core.Name "body")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotation"))}))},
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
                                    Core.projectionField = (Core.Name "annotation")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.AnnotatedType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.application"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Application"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "function"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Application"),
                                    Core.projectionField = (Core.Name "function")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "argument"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Application"),
                                    Core.projectionField = (Core.Name "argument")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Application")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.applicationType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.ApplicationType"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "function"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
                                    Core.projectionField = (Core.Name "function")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "argument"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
                                    Core.projectionField = (Core.Name "argument")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.ApplicationType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.binding"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Binding"))}))},
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
                                    Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
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
                                    Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
                                    Core.projectionField = (Core.Name "term")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
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
                                    Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
                                    Core.projectionField = (Core.Name "type")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Binding")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.caseStatement"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.CaseStatement"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
                                    Core.projectionField = (Core.Name "typeName")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "default"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
                                    Core.projectionField = (Core.Name "default")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "cases"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.field"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
                                    Core.projectionField = (Core.Name "cases")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.CaseStatement")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.eitherType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.EitherType"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "left"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
                                    Core.projectionField = (Core.Name "left")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "right"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
                                    Core.projectionField = (Core.Name "right")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.EitherType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.pairType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.PairType"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "first"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
                                    Core.projectionField = (Core.Name "first")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "second"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
                                    Core.projectionField = (Core.Name "second")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.PairType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.field"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Field"))}))},
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
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
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
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                    Core.projectionField = (Core.Name "term")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Field")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.fieldType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FieldType"))}))},
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
                                    Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
                                    Core.projectionField = (Core.Name "name")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
                                    Core.projectionField = (Core.Name "type")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.FieldType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.floatType"),
          Packaging.termDefinitionTerm = (Core.TermCases (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.core.FloatType"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "bigfloat"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bigfloat"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "float32"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float32"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "float64"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float64"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.FloatType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.floatValue"),
          Packaging.termDefinitionTerm = (Core.TermCases (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "bigfloat"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bigfloat"))}))},
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
                                              Core.fieldName = (Core.Name "float"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "bigfloat"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "float32"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float32"))}))},
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
                                              Core.fieldName = (Core.Name "float"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "float32"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "float64"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float64"))}))},
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
                                              Core.fieldName = (Core.Name "float"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "float64"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.FloatValue")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.forallType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.ForallType"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "parameter"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
                                    Core.projectionField = (Core.Name "parameter")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
                                    Core.projectionField = (Core.Name "body")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.ForallType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.functionType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FunctionType"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "domain"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
                                    Core.projectionField = (Core.Name "domain")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "codomain"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
                                    Core.projectionField = (Core.Name "codomain")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.FunctionType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.injection"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Injection"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                    Core.projectionField = (Core.Name "typeName")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "field"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.field")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                    Core.projectionField = (Core.Name "field")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Injection")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.integerType"),
          Packaging.termDefinitionTerm = (Core.TermCases (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerType"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "bigint"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bigint"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "int8"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int8"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "int16"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int16"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "int32"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int32"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "int64"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int64"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "uint8"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint8"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "uint16"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint16"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "uint32"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint32"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "uint64"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint64"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.IntegerType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.integerValue"),
          Packaging.termDefinitionTerm = (Core.TermCases (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "bigint"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bigint"))}))},
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
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "bigint"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "int8"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int8"))}))},
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
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "int8"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "int16"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int16"))}))},
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
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "int16"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "int32"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int32"))}))},
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
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "int32"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "int64"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int64"))}))},
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
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "int64"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "uint8"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint8"))}))},
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
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "uint8"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "uint16"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint16"))}))},
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
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "uint16"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "uint32"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint32"))}))},
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
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "uint32"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "uint64"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint64"))}))},
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
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "uint64"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.IntegerValue")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.lambda"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Lambda"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "parameter"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
                                    Core.projectionField = (Core.Name "parameter")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "domain"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.type"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
                                    Core.projectionField = (Core.Name "domain")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
                                    Core.projectionField = (Core.Name "body")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Lambda")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.let"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Let"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bindings"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.binding"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Let"),
                                    Core.projectionField = (Core.Name "bindings")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Let"),
                                    Core.projectionField = (Core.Name "body")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Let")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.literal"),
          Packaging.termDefinitionTerm = (Core.TermCases (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "binary"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "binary"))}))},
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
                                              Core.fieldName = (Core.Name "binary"),
                                              Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "boolean"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "boolean"))}))},
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
                                              Core.fieldName = (Core.Name "boolean"),
                                              Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "decimal"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "decimal"))}))},
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
                                              Core.fieldName = (Core.Name "decimal"),
                                              Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "float"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.floatValue")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "integer"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "integer"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.integerValue")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "string"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "string"))}))},
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
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Literal")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.literalType"),
          Packaging.termDefinitionTerm = (Core.TermCases (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.core.LiteralType"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "binary"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "binary"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "boolean"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "boolean"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "decimal"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "decimal"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "float"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.floatType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "integer"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "integer"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.integerType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "string"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "string"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.LiteralType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.mapType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.MapType"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "keys"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
                                    Core.projectionField = (Core.Name "keys")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "values"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
                                    Core.projectionField = (Core.Name "values")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.MapType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.name"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Name"))}))},
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
                          Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.core.Name")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Name")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.projection"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Projection"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
                                    Core.projectionField = (Core.Name "typeName")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "field"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
                                    Core.projectionField = (Core.Name "field")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Projection")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.record"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Record"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Record"),
                                    Core.projectionField = (Core.Name "typeName")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "fields"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.field"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Record"),
                                    Core.projectionField = (Core.Name "fields")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Record")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.term"),
          Packaging.termDefinitionTerm = (Core.TermCases (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "annotated"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotated"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.annotatedTerm")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "application"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "application"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.application")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "cases"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "cases"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.caseStatement")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "either"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "either"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "e"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "either"),
                                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bimap")),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "inject"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "inject"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.injection")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "lambda"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "lambda"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.lambda")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "let"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "let"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.let")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "list"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "list"))}))},
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
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "literal"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "literal"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.literal")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "map"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "map"))}))},
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
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "maybe"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "maybe"))}))},
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
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "pair"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "pair"))}))},
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
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "project"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "project"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.projection")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "record"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "record"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.record")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "set"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "set"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "s"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "set"),
                                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.map")),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "typeApplication"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeApplication"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.typeApplicationTerm")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "typeLambda"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeLambda"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.typeLambda")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "unit"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unit"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "unwrap"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unwrap"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "variable"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "variable"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "wrap"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "wrap"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.wrappedTerm")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.type"),
          Packaging.termDefinitionTerm = (Core.TermCases (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.core.Type"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "annotated"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotated"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.annotatedType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "application"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "application"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.applicationType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "either"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "either"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.eitherType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "forall"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "forall"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.forallType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "function"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "function"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.functionType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "list"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "list"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "literal"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "literal"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.literalType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "map"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "map"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.mapType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "maybe"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "maybe"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "pair"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "pair"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.pairType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "record"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "record"))}))},
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
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.fieldType"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "set"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "set"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "union"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "union"))}))},
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
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.fieldType"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "unit"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unit"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "variable"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "variable"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "void"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "void"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "wrap"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "wrap"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Type")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.typeApplicationTerm"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.TypeApplicationTerm"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
                                    Core.projectionField = (Core.Name "body")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
                                    Core.projectionField = (Core.Name "type")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.TypeApplicationTerm")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.typeLambda"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.TypeLambda"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "parameter"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
                                    Core.projectionField = (Core.Name "parameter")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
                                    Core.projectionField = (Core.Name "body")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.TypeLambda")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.typeScheme"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.TypeScheme"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "variables"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
                                    Core.projectionField = (Core.Name "variables")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
                                    Core.projectionField = (Core.Name "body")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "constraints"))}))},
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
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.typeVariableMetadata"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
                                    Core.projectionField = (Core.Name "constraints")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.typeVariableMetadata"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.TypeVariableMetadata"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "classes"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "s"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "set"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.TypeVariableMetadata"),
                                    Core.projectionField = (Core.Name "classes")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.TypeVariableMetadata")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.wrappedTerm"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.WrappedTerm"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
                                    Core.projectionField = (Core.Name "typeName")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
                                    Core.projectionField = (Core.Name "body")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.WrappedTerm")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}))],
      Packaging.moduleTermDependencies = [
        Packaging.Namespace "hydra.encode.core"],
      Packaging.moduleTypeDependencies = [
        Packaging.Namespace "hydra.core"],
      Packaging.moduleDescription = (Just "Term encoders for hydra.core")}
