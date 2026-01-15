-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.module

module Hydra.Sources.Encode.Module where

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
  Module.moduleNamespace = (Module.Namespace "hydra.encode.module"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.module.definition"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.module.Definition"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "term"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.module.Definition"))}))},
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.module.termDefinition")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "type"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.module.Definition"))}))},
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.module.typeDefinition")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))}]})))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.module.fileExtension"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.module.FileExtension"))}))},
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.module.FileExtension")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.module.module"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.module.Module"))}))},
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
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.module.namespace")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.module.Module"),
                                Core.projectionField = (Core.Name "namespace")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "hydra.core.Field"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "elements"))}))},
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.binding"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.module.Module"),
                                Core.projectionField = (Core.Name "elements")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    Core.TermRecord (Core.Record {
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.module.namespace"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.module.Module"),
                                Core.projectionField = (Core.Name "termDependencies")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    Core.TermRecord (Core.Record {
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.module.namespace"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.module.Module"),
                                Core.projectionField = (Core.Name "typeDependencies")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
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
                                      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.module.Module"),
                                Core.projectionField = (Core.Name "description")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.module.namespace"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.module.Namespace"))}))},
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.module.Namespace")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.module.namespaces"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "n"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.module.Namespaces"))}))},
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
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "p"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                  Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                  Core.injectionField = Core.Field {
                                    Core.fieldName = (Core.Name "pair"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.bimap"))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.module.namespace"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.module.Namespaces"),
                                  Core.projectionField = (Core.Name "focus")})))),
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
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "m"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                  Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                  Core.injectionField = Core.Field {
                                    Core.fieldName = (Core.Name "map"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.bimap"))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.module.namespace"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.module.Namespaces"),
                                  Core.projectionField = (Core.Name "mapping")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.module.qualifiedName"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.module.QualifiedName"))}))},
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.module.namespace"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.module.QualifiedName"),
                                Core.projectionField = (Core.Name "namespace")})))),
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
                                Core.projectionTypeName = (Core.Name "hydra.module.QualifiedName"),
                                Core.projectionField = (Core.Name "local")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.module.termDefinition"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.module.TermDefinition"))}))},
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
                                Core.projectionTypeName = (Core.Name "hydra.module.TermDefinition"),
                                Core.projectionField = (Core.Name "name")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    Core.TermRecord (Core.Record {
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
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.module.TermDefinition"),
                                Core.projectionField = (Core.Name "term")})))),
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
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.typeScheme")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.module.TermDefinition"),
                                Core.projectionField = (Core.Name "type")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.module.typeDefinition"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.module.TypeDefinition"))}))},
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
                                Core.projectionTypeName = (Core.Name "hydra.module.TypeDefinition"),
                                Core.projectionField = (Core.Name "name")})))),
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
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.module.TypeDefinition"),
                                Core.projectionField = (Core.Name "type")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.encode.core"],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.module"],
  Module.moduleDescription = (Just "Term encoders for hydra.module")}
