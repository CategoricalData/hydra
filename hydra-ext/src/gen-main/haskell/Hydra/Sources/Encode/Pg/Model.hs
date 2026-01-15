-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.pg.model

module Hydra.Sources.Encode.Pg.Model where

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
  Module.moduleNamespace = (Module.Namespace "hydra.encode.pg.model"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.adjacentEdge"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.AdjacentEdge"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "label"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edgeLabel")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
                                  Core.projectionField = (Core.Name "label")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "v")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
                                  Core.projectionField = (Core.Name "id")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "vertex"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "v")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
                                  Core.projectionField = (Core.Name "vertex")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "properties"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.pg.model.propertyKey"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
                                  Core.projectionField = (Core.Name "properties")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.direction"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.pg.model.Direction"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "out"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Direction"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
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
                                  Core.lambdaParameter = (Core.Name "_"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "unit"),
                                      Core.fieldTerm = Core.TermUnit}}))}))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "in"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Direction"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
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
                                  Core.lambdaParameter = (Core.Name "_"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "unit"),
                                      Core.fieldTerm = Core.TermUnit}}))}))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "both"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Direction"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "both"))}))},
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
            Core.fieldName = (Core.Name "undirected"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Direction"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undirected"))}))},
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
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.edge"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Edge"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "label"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edgeLabel")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
                                  Core.projectionField = (Core.Name "label")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "v")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
                                  Core.projectionField = (Core.Name "id")})))),
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "v")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
                                  Core.projectionField = (Core.Name "out")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "v")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
                                  Core.projectionField = (Core.Name "in")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "properties"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.pg.model.propertyKey"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
                                  Core.projectionField = (Core.Name "properties")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.edgeLabel"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.EdgeLabel"))}))},
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.model.EdgeLabel")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.edgeType"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "t"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.EdgeType"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "label"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edgeLabel")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
                                  Core.projectionField = (Core.Name "label")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "t")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
                                  Core.projectionField = (Core.Name "id")})))),
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertexLabel")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
                                  Core.projectionField = (Core.Name "out")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertexLabel")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
                                  Core.projectionField = (Core.Name "in")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "properties"))}))},
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
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.propertyType")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
                                  Core.projectionField = (Core.Name "properties")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.element"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
          Core.caseStatementTypeName = (Core.Name "hydra.pg.model.Element"),
          Core.caseStatementDefault = Nothing,
          Core.caseStatementCases = [
            Core.Field {
              Core.fieldName = (Core.Name "vertex"),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Element"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "field"),
                          Core.fieldTerm = (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "vertex"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertex")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
            Core.Field {
              Core.fieldName = (Core.Name "edge"),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Element"))}))},
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
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edge")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))}]}))))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.elementKind"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.pg.model.ElementKind"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "vertex"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.ElementKind"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "vertex"))}))},
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
            Core.fieldName = (Core.Name "edge"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.ElementKind"))}))},
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
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "_"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "unit"),
                                      Core.fieldTerm = Core.TermUnit}}))}))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))}]})))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.elementTree"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.ElementTree"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "self"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.element")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
                                  Core.projectionField = (Core.Name "self")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
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
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.elementTree")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
                                  Core.projectionField = (Core.Name "dependencies")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.elementType"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "t"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
          Core.caseStatementTypeName = (Core.Name "hydra.pg.model.ElementType"),
          Core.caseStatementDefault = Nothing,
          Core.caseStatementCases = [
            Core.Field {
              Core.fieldName = (Core.Name "vertex"),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.ElementType"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "field"),
                          Core.fieldTerm = (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "vertex"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertexType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "t"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
            Core.Field {
              Core.fieldName = (Core.Name "edge"),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.ElementType"))}))},
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
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edgeType")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "t"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))}]}))))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.elementTypeTree"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "t"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.ElementTypeTree"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "self"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.elementType")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "t"))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
                                  Core.projectionField = (Core.Name "self")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
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
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.elementTypeTree")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
                                  Core.projectionField = (Core.Name "dependencies")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.graph"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Graph"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "vertices"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertex")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
                                  Core.projectionField = (Core.Name "vertices")})))),
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edge")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
                                  Core.projectionField = (Core.Name "edges")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.graphSchema"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "t"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.GraphSchema"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "vertices"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertexLabel"))})),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertexType")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
                                  Core.projectionField = (Core.Name "vertices")})))),
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edgeLabel"))})),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edgeType")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
                                  Core.projectionField = (Core.Name "edges")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.label"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.Name "hydra.pg.model.Label"),
        Core.caseStatementDefault = Nothing,
        Core.caseStatementCases = [
          Core.Field {
            Core.fieldName = (Core.Name "vertex"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Label"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "field"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "vertex"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertexLabel")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
          Core.Field {
            Core.fieldName = (Core.Name "edge"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Label"))}))},
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edgeLabel")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))}]})))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.lazyGraph"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.LazyGraph"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "vertices"))}))},
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
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertex")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
                                  Core.projectionField = (Core.Name "vertices")})))),
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
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.edge")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
                                  Core.projectionField = (Core.Name "edges")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.property"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Property"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "key"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.propertyKey")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
                                  Core.projectionField = (Core.Name "key")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "v")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
                                  Core.projectionField = (Core.Name "value")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.propertyKey"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.PropertyKey"))}))},
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.model.PropertyKey")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.propertyType"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "t"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.PropertyType"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "key"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.propertyKey")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
                                  Core.projectionField = (Core.Name "key")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "t")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
                                  Core.projectionField = (Core.Name "value")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "required"))}))},
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
                                        Core.fieldName = (Core.Name "boolean"),
                                        Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
                                  Core.projectionField = (Core.Name "required")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.vertex"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.Vertex"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "label"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertexLabel")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
                                  Core.projectionField = (Core.Name "label")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "v")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
                                  Core.projectionField = (Core.Name "id")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "properties"))}))},
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
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.pg.model.propertyKey"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
                                  Core.projectionField = (Core.Name "properties")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.vertexLabel"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.VertexLabel"))}))},
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.model.VertexLabel")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.vertexType"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "t"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.VertexType"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "label"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertexLabel")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
                                  Core.projectionField = (Core.Name "label")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "t")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
                                  Core.projectionField = (Core.Name "id")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "properties"))}))},
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
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.propertyType")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
                                  Core.projectionField = (Core.Name "properties")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.pg.model.vertexWithAdjacentEdges"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.pg.model.VertexWithAdjacentEdges"))}))},
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
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "vertex"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "term"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.vertex")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
                                  Core.projectionField = (Core.Name "vertex")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "ins"))}))},
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
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.adjacentEdge")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
                                  Core.projectionField = (Core.Name "ins")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "outs"))}))},
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
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.pg.model.adjacentEdge")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
                                  Core.projectionField = (Core.Name "outs")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.encode.core"],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.pg.model"],
  Module.moduleDescription = (Just "Term encoders for hydra.pg.model")}
