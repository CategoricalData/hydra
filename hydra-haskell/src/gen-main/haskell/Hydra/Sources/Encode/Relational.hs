-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.relational

module Hydra.Sources.Encode.Relational where

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
  Module.moduleNamespace = (Module.Namespace "hydra.encode.relational"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.relational.columnName"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.relational.ColumnName"))}))},
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.ColumnName")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.relational.columnSchema"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.relational.ColumnSchema"))}))},
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
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
                                  Core.projectionField = (Core.Name "name")})))),
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
                              Core.applicationFunction = (Core.TermVariable (Core.Name "t")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
                                  Core.projectionField = (Core.Name "domain")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.relational.foreignKey"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.relational.ForeignKey"))}))},
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foreignRelation"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "term"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.relational.relationName")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
                                Core.projectionField = (Core.Name "foreignRelation")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                    (Core.TermRecord (Core.Record {
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
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.relational.columnName"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.relational.columnName"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
                                Core.projectionField = (Core.Name "keys")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.relational.primaryKey"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.relational.PrimaryKey"))}))},
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
                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.relational.columnName"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.PrimaryKey")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.relational.relation"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.relational.Relation"))}))},
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
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.relational.row")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.Relation")))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.relational.relationName"),
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
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.relational.RelationName"))}))},
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.RelationName")))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.relational.relationSchema"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.relational.RelationSchema"))}))},
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
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
                                  Core.projectionField = (Core.Name "name")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
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
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.relational.columnSchema")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
                                  Core.projectionField = (Core.Name "columns")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "primaryKeys"))}))},
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
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.relational.primaryKey"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
                                  Core.projectionField = (Core.Name "primaryKeys")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                      (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Field"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "name"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foreignKeys"))}))},
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
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.relational.foreignKey"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
                                  Core.projectionField = (Core.Name "foreignKeys")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.relational.relationship"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.relational.Relationship"))}))},
                  Core.Field {
                    Core.fieldName = (Core.Name "body"),
                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "s"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermUnion (Core.Injection {
                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                          Core.injectionField = Core.Field {
                            Core.fieldName = (Core.Name "set"),
                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.map"))),
                                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.relational.columnName"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}}))}))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.Relationship")))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.relational.row"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "v"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.relational.Row"))}))},
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
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.Row")))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})))}))),
      Core.bindingType = Nothing}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.encode.core"],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.relational"],
  Module.moduleDescription = (Just "Term encoders for hydra.relational")}
