-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.paths

module Hydra.Sources.Encode.Paths where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

module_ :: Module.Module
module_ =
    Module.Module {
      Module.moduleNamespace = (Module.Namespace "hydra.encode.paths"),
      Module.moduleDefinitions = [
        Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtermEdge"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermEdge"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "source"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.paths.subtermNode")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
                                    Core.projectionField = (Core.Name "source")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "path"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.paths.subtermPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
                                    Core.projectionField = (Core.Name "path")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "target"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.paths.subtermNode")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtermEdge"),
                                    Core.projectionField = (Core.Name "target")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtermEdge")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}),
        (Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtermGraph"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermGraph"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nodes"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.paths.subtermNode"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
                                    Core.projectionField = (Core.Name "nodes")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "edges"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.paths.subtermEdge"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtermGraph"),
                                    Core.projectionField = (Core.Name "edges")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtermGraph")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtermNode"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermNode"))}))},
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "label"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "x"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "literal"),
                                      Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "string"),
                                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
                                    Core.projectionField = (Core.Name "label")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "id"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "x"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "literal"),
                                      Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "string"),
                                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtermNode"),
                                    Core.projectionField = (Core.Name "id")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtermNode")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtermPath"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermPath"))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "body"),
                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "xs"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                            Core.injectionField = Core.Field {
                              Core.fieldName = (Core.Name "list"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.paths.subtermStep"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.paths.SubtermPath")))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtermPath")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtermStep"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.paths.SubtermStep"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "annotatedBody"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotatedBody"))}))},
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
                Core.fieldName = (Core.Name "applicationFunction"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "applicationFunction"))}))},
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
                Core.fieldName = (Core.Name "applicationArgument"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "applicationArgument"))}))},
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
                Core.fieldName = (Core.Name "lambdaBody"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "lambdaBody"))}))},
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
                Core.fieldName = (Core.Name "unionCasesDefault"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unionCasesDefault"))}))},
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
                Core.fieldName = (Core.Name "unionCasesBranch"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unionCasesBranch"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "letBody"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "letBody"))}))},
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
                Core.fieldName = (Core.Name "letBinding"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "letBinding"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "listElement"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "listElement"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "x"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "literal"),
                                          Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                            Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                            Core.injectionField = Core.Field {
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "int32"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))}))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "mapKey"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "mapKey"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "x"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "literal"),
                                          Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                            Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                            Core.injectionField = Core.Field {
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "int32"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))}))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "mapValue"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "mapValue"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "x"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "literal"),
                                          Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                            Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                            Core.injectionField = Core.Field {
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "int32"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))}))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "maybeTerm"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "maybeTerm"))}))},
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
                Core.fieldName = (Core.Name "productTerm"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "productTerm"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "x"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "literal"),
                                          Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                            Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                            Core.injectionField = Core.Field {
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "int32"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))}))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "recordField"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "recordField"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "setElement"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "setElement"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "x"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "literal"),
                                          Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                            Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                            Core.injectionField = Core.Field {
                                              Core.fieldName = (Core.Name "integer"),
                                              Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                                Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                Core.injectionField = Core.Field {
                                                  Core.fieldName = (Core.Name "int32"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))}))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "sumTerm"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "sumTerm"))}))},
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
                Core.fieldName = (Core.Name "typeLambdaBody"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeLambdaBody"))}))},
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
                Core.fieldName = (Core.Name "typeApplicationTerm"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeApplicationTerm"))}))},
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
                Core.fieldName = (Core.Name "injectionTerm"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "injectionTerm"))}))},
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
                Core.fieldName = (Core.Name "wrappedTerm"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtermStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "wrappedTerm"))}))},
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
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtermStep")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtypeEdge"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeEdge"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "source"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.paths.subtypeNode")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
                                    Core.projectionField = (Core.Name "source")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "path"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.paths.subtypePath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
                                    Core.projectionField = (Core.Name "path")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "target"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.paths.subtypeNode")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
                                    Core.projectionField = (Core.Name "target")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtypeEdge")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtypeGraph"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeGraph"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nodes"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.paths.subtypeNode"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
                                    Core.projectionField = (Core.Name "nodes")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "edges"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.paths.subtypeEdge"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
                                    Core.projectionField = (Core.Name "edges")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtypeGraph")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtypeNode"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeNode"))}))},
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "label"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "x"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "literal"),
                                      Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "string"),
                                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
                                    Core.projectionField = (Core.Name "label")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "id"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "x"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "literal"),
                                      Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "string"),
                                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.paths.SubtypeNode"),
                                    Core.projectionField = (Core.Name "id")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtypeNode")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtypePath"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypePath"))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "body"),
                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "xs"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                            Core.injectionField = Core.Field {
                              Core.fieldName = (Core.Name "list"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.paths.subtypeStep"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.paths.SubtypePath")))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtypePath")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Module.DefinitionTerm (Module.TermDefinition {
          Module.termDefinitionName = (Core.Name "hydra.encode.paths.subtypeStep"),
          Module.termDefinitionTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.paths.SubtypeStep"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "annotatedBody"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotatedBody"))}))},
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
                Core.fieldName = (Core.Name "applicationFunction"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "applicationFunction"))}))},
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
                Core.fieldName = (Core.Name "applicationArgument"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "applicationArgument"))}))},
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
                Core.fieldName = (Core.Name "eitherLeft"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "eitherLeft"))}))},
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
                Core.fieldName = (Core.Name "eitherRight"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "eitherRight"))}))},
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
                Core.fieldName = (Core.Name "forallBody"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "forallBody"))}))},
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
                Core.fieldName = (Core.Name "functionDomain"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "functionDomain"))}))},
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
                Core.fieldName = (Core.Name "functionCodomain"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "functionCodomain"))}))},
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
                Core.fieldName = (Core.Name "listElement"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "listElement"))}))},
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
                Core.fieldName = (Core.Name "mapKeys"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "mapKeys"))}))},
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
                Core.fieldName = (Core.Name "mapValues"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "mapValues"))}))},
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
                Core.fieldName = (Core.Name "maybeElement"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "maybeElement"))}))},
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
                Core.fieldName = (Core.Name "pairFirst"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "pairFirst"))}))},
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
                Core.fieldName = (Core.Name "pairSecond"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "pairSecond"))}))},
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
                Core.fieldName = (Core.Name "recordField"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "recordField"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "setElement"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "setElement"))}))},
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
                Core.fieldName = (Core.Name "unionField"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unionField"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "wrappedType"),
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.paths.SubtypeStep"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "wrappedType"))}))},
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
          Module.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.paths.SubtypeStep")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}))],
      Module.moduleTermDependencies = [
        Module.Namespace "hydra.encode.core"],
      Module.moduleTypeDependencies = [
        Module.Namespace "hydra.paths"],
      Module.moduleDescription = (Just "Term encoders for hydra.paths")}
