-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.tabular

module Hydra.Sources.Encode.Tabular where

import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleNamespace = (Packaging.Namespace "hydra.encode.tabular"),
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.tabular.columnType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.tabular.ColumnType"))}))},
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.relational.columnName")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
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
                                    Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
                                    Core.projectionField = (Core.Name "type")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.tabular.ColumnType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.tabular.dataRow"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.tabular.DataRow"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "body"),
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
                                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "opt"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "maybe"),
                                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))}))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tabular.DataRow")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [
              Core.Name "v"],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.tabular.DataRow")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.tabular.headerRow"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.tabular.HeaderRow"))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "body"),
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tabular.HeaderRow")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.tabular.HeaderRow")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.tabular.table"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.tabular.Table"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "header"))}))},
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.tabular.headerRow"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
                                      Core.projectionField = (Core.Name "header")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "data"))}))},
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
                                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.tabular.dataRow")),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
                                      Core.projectionField = (Core.Name "data")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [
              Core.Name "v"],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.tabular.Table")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.tabular.tableType"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.tabular.TableType"))}))},
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.relational.relationName")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
                                    Core.projectionField = (Core.Name "name")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "columns"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.tabular.columnType"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
                                    Core.projectionField = (Core.Name "columns")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.tabular.TableType")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}))],
      Packaging.moduleTermDependencies = [
        Packaging.Namespace "hydra.encode.core",
        (Packaging.Namespace "hydra.encode.relational")],
      Packaging.moduleTypeDependencies = [
        Packaging.Namespace "hydra.tabular"],
      Packaging.moduleDescription = (Just "Term encoders for hydra.tabular")}
