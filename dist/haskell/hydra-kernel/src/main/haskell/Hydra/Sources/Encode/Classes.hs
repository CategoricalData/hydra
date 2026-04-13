-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.classes

module Hydra.Sources.Encode.Classes where

import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleNamespace = (Packaging.Namespace "hydra.encode.classes"),
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.classes.typeClass"),
          Packaging.termDefinitionTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.classes.TypeClass"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "equality"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.classes.TypeClass"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "equality"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))}))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "ordering"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.classes.TypeClass"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "ordering"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "_"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "unit"),
                                          Core.fieldTerm = Core.TermUnit}}))}))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))}]})))),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.classes.TypeClass")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})],
      Packaging.moduleTermDependencies = [
        Packaging.Namespace "hydra.encode.core"],
      Packaging.moduleTypeDependencies = [
        Packaging.Namespace "hydra.classes"],
      Packaging.moduleDescription = (Just "Term encoders for hydra.classes")}
