-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.query

module Hydra.Sources.Encode.Query where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

module_ :: Module.Module
module_ = Module.Module {
  Module.moduleNamespace = (Module.Namespace "hydra.encode.query"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.comparisonConstraint"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.query.ComparisonConstraint"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "equal"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.ComparisonConstraint"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "equal"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "notEqual"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.ComparisonConstraint"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "notEqual"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "lessThan"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.ComparisonConstraint"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "lessThan"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "greaterThan"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.ComparisonConstraint"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "greaterThan"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "lessThanOrEqual"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.ComparisonConstraint"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "lessThanOrEqual"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "greaterThanOrEqual"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.ComparisonConstraint"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "greaterThanOrEqual"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))}]})))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.edge"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Edge"))}))},
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "type"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
                                Core.projectionField = (Core.Name "type")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "out"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "opt"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                Core.injectionField = Core.Field {
                                  Core.fieldName = (Core.Name "maybe"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.map"))),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
                                Core.projectionField = (Core.Name "out")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "in"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "opt"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                Core.injectionField = Core.Field {
                                  Core.fieldName = (Core.Name "maybe"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.map"))),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.Edge"),
                                Core.projectionField = (Core.Name "in")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.graphPattern"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.GraphPattern"))}))},
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "graph"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
                                Core.projectionField = (Core.Name "graph")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "patterns"))}))},
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.query.pattern"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.GraphPattern"),
                                Core.projectionField = (Core.Name "patterns")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.node"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.query.Node"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "term"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Node"))}))},
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "variable"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Node"))}))},
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.variable")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "wildcard"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Node"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "wildcard"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))}]})))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.path"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.query.Path"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "step"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Path"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "step"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.step")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "regex"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Path"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "regex"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.regexSequence")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "inverse"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Path"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "inverse"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.path")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))}]})))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.pattern"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.query.Pattern"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "triple"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Pattern"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "triple"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.triplePattern")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "negation"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Pattern"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "negation"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.pattern")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "conjunction"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Pattern"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "conjunction"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.query.pattern"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "disjunction"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Pattern"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "disjunction"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.query.pattern"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "graph"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Pattern"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "graph"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.graphPattern")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))}]})))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.query"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Query"))}))},
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.query.variable"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.Query"),
                                Core.projectionField = (Core.Name "variables")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "patterns"))}))},
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.query.pattern"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.Query"),
                                Core.projectionField = (Core.Name "patterns")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.range"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Range"))}))},
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "min"))}))},
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
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.Range"),
                                Core.projectionField = (Core.Name "min")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "max"))}))},
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
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.Range"),
                                Core.projectionField = (Core.Name "max")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.regexQuantifier"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.query.RegexQuantifier"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "one"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.RegexQuantifier"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "one"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "zeroOrOne"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.RegexQuantifier"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "zeroOrOne"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "zeroOrMore"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.RegexQuantifier"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "zeroOrMore"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "oneOrMore"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.RegexQuantifier"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "oneOrMore"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "exactly"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.RegexQuantifier"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "exactly"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "atLeast"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.RegexQuantifier"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "atLeast"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "range"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.RegexQuantifier"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "range"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.range")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))}]})))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.regexSequence"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.RegexSequence"))}))},
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "path"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.path")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
                                Core.projectionField = (Core.Name "path")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "quantifier"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.regexQuantifier")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.RegexSequence"),
                                Core.projectionField = (Core.Name "quantifier")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.step"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.query.Step"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "edge"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Step"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "edge"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.edge")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "project"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Step"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "compare"),
            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Step"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "compare"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.comparisonConstraint")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))}]})))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.triplePattern"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.TriplePattern"))}))},
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "subject"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.node")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
                                Core.projectionField = (Core.Name "subject")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "predicate"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.path")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
                                Core.projectionField = (Core.Name "predicate")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "object"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.query.node")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.query.TriplePattern"),
                                Core.projectionField = (Core.Name "object")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.query.variable"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.query.Variable"))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "body"),
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.query.Variable")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
      Core.bindingType = Nothing}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.encode.core"],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.query"],
  Module.moduleDescription = (Just "Term encoders for hydra.query")}
