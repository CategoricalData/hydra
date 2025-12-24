-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.parsing

module Hydra.Sources.Encode.Parsing where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

module_ :: Module.Module
module_ = Module.Module {
  Module.moduleNamespace = (Module.Namespace "hydra.encode.parsing"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.parsing.parseError"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.parsing.ParseError"))}))},
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "message"))}))},
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
                                Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
                                Core.projectionField = (Core.Name "message")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "remainder"))}))},
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
                                Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
                                Core.projectionField = (Core.Name "remainder")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.parsing.parseResult"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "a"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
          Core.caseStatementTypeName = (Core.Name "hydra.parsing.ParseResult"),
          Core.caseStatementDefault = Nothing,
          Core.caseStatementCases = [
            Core.Field {
              Core.fieldName = (Core.Name "success"),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.parsing.ParseResult"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "field"),
                          Core.fieldTerm = (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "success"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.parsing.parseSuccess")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))},
            Core.Field {
              Core.fieldName = (Core.Name "failure"),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.parsing.ParseResult"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "field"),
                          Core.fieldTerm = (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "failure"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.parsing.parseError")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}]}))}]}))}}))})))}]}))))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.parsing.parseSuccess"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "a"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.parsing.ParseSuccess"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "value"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "a")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
                                  Core.projectionField = (Core.Name "value")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "remainder"))}))},
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
                                  Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
                                  Core.projectionField = (Core.Name "remainder")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.encode.core"],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.parsing"],
  Module.moduleDescription = (Just "Term encoders for hydra.parsing")}
