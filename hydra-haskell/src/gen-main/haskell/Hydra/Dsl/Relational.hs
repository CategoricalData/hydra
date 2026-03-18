-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.relational

module Hydra.Dsl.Relational where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Relational as Relational
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

columnName :: Phantoms.TTerm String -> Phantoms.TTerm Relational.ColumnName
columnName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.ColumnName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unColumnName :: Phantoms.TTerm Relational.ColumnName -> Phantoms.TTerm String
unColumnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.ColumnName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnSchema :: Phantoms.TTerm Relational.ColumnName -> Phantoms.TTerm t -> Phantoms.TTerm (Relational.ColumnSchema t)
columnSchema name domain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ColumnSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm domain)}]}))

columnSchemaName :: Phantoms.TTerm (Relational.ColumnSchema t) -> Phantoms.TTerm Relational.ColumnName
columnSchemaName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnSchemaDomain :: Phantoms.TTerm (Relational.ColumnSchema t) -> Phantoms.TTerm t
columnSchemaDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
        Core.projectionField = (Core.Name "domain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnSchemaWithName :: Phantoms.TTerm (Relational.ColumnSchema t) -> Phantoms.TTerm Relational.ColumnName -> Phantoms.TTerm (Relational.ColumnSchema t)
columnSchemaWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ColumnSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnSchemaWithDomain :: Phantoms.TTerm (Relational.ColumnSchema t) -> Phantoms.TTerm t -> Phantoms.TTerm (Relational.ColumnSchema t)
columnSchemaWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ColumnSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

foreignKey :: Phantoms.TTerm Relational.RelationName -> Phantoms.TTerm (M.Map Relational.ColumnName Relational.ColumnName) -> Phantoms.TTerm Relational.ForeignKey
foreignKey foreignRelation keys =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ForeignKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "foreignRelation"),
          Core.fieldTerm = (Phantoms.unTTerm foreignRelation)},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm keys)}]}))

foreignKeyForeignRelation :: Phantoms.TTerm Relational.ForeignKey -> Phantoms.TTerm Relational.RelationName
foreignKeyForeignRelation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
        Core.projectionField = (Core.Name "foreignRelation")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

foreignKeyKeys :: Phantoms.TTerm Relational.ForeignKey -> Phantoms.TTerm (M.Map Relational.ColumnName Relational.ColumnName)
foreignKeyKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
        Core.projectionField = (Core.Name "keys")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

foreignKeyWithForeignRelation :: Phantoms.TTerm Relational.ForeignKey -> Phantoms.TTerm Relational.RelationName -> Phantoms.TTerm Relational.ForeignKey
foreignKeyWithForeignRelation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ForeignKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "foreignRelation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
              Core.projectionField = (Core.Name "keys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

foreignKeyWithKeys :: Phantoms.TTerm Relational.ForeignKey -> Phantoms.TTerm (M.Map Relational.ColumnName Relational.ColumnName) -> Phantoms.TTerm Relational.ForeignKey
foreignKeyWithKeys original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ForeignKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "foreignRelation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
              Core.projectionField = (Core.Name "foreignRelation")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

primaryKey :: Phantoms.TTerm [Relational.ColumnName] -> Phantoms.TTerm Relational.PrimaryKey
primaryKey x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.PrimaryKey"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPrimaryKey :: Phantoms.TTerm Relational.PrimaryKey -> Phantoms.TTerm [Relational.ColumnName]
unPrimaryKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.PrimaryKey")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relation :: Phantoms.TTerm [Relational.Row v] -> Phantoms.TTerm (Relational.Relation v)
relation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.Relation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRelation :: Phantoms.TTerm (Relational.Relation v) -> Phantoms.TTerm [Relational.Row v]
unRelation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.Relation")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationName :: Phantoms.TTerm String -> Phantoms.TTerm Relational.RelationName
relationName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.RelationName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRelationName :: Phantoms.TTerm Relational.RelationName -> Phantoms.TTerm String
unRelationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.RelationName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationSchema :: Phantoms.TTerm Relational.RelationName -> Phantoms.TTerm [Relational.ColumnSchema t] -> Phantoms.TTerm [Relational.PrimaryKey] -> Phantoms.TTerm [Relational.ForeignKey] -> Phantoms.TTerm (Relational.RelationSchema t)
relationSchema name columns primaryKeys foreignKeys =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm columns)},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Phantoms.unTTerm primaryKeys)},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Phantoms.unTTerm foreignKeys)}]}))

relationSchemaName :: Phantoms.TTerm (Relational.RelationSchema t) -> Phantoms.TTerm Relational.RelationName
relationSchemaName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationSchemaColumns :: Phantoms.TTerm (Relational.RelationSchema t) -> Phantoms.TTerm [Relational.ColumnSchema t]
relationSchemaColumns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
        Core.projectionField = (Core.Name "columns")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationSchemaPrimaryKeys :: Phantoms.TTerm (Relational.RelationSchema t) -> Phantoms.TTerm [Relational.PrimaryKey]
relationSchemaPrimaryKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
        Core.projectionField = (Core.Name "primaryKeys")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationSchemaForeignKeys :: Phantoms.TTerm (Relational.RelationSchema t) -> Phantoms.TTerm [Relational.ForeignKey]
relationSchemaForeignKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
        Core.projectionField = (Core.Name "foreignKeys")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationSchemaWithName :: Phantoms.TTerm (Relational.RelationSchema t) -> Phantoms.TTerm Relational.RelationName -> Phantoms.TTerm (Relational.RelationSchema t)
relationSchemaWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "columns")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "primaryKeys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "foreignKeys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationSchemaWithColumns :: Phantoms.TTerm (Relational.RelationSchema t) -> Phantoms.TTerm [Relational.ColumnSchema t] -> Phantoms.TTerm (Relational.RelationSchema t)
relationSchemaWithColumns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "primaryKeys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "foreignKeys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationSchemaWithPrimaryKeys :: Phantoms.TTerm (Relational.RelationSchema t) -> Phantoms.TTerm [Relational.PrimaryKey] -> Phantoms.TTerm (Relational.RelationSchema t)
relationSchemaWithPrimaryKeys original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "columns")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "foreignKeys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationSchemaWithForeignKeys :: Phantoms.TTerm (Relational.RelationSchema t) -> Phantoms.TTerm [Relational.ForeignKey] -> Phantoms.TTerm (Relational.RelationSchema t)
relationSchemaWithForeignKeys original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "columns")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionField = (Core.Name "primaryKeys")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relationship :: Phantoms.TTerm (S.Set (M.Map Relational.ColumnName v)) -> Phantoms.TTerm (Relational.Relationship v)
relationship x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.Relationship"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRelationship :: Phantoms.TTerm (Relational.Relationship v) -> Phantoms.TTerm (S.Set (M.Map Relational.ColumnName v))
unRelationship x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.Relationship")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

row :: Phantoms.TTerm [v] -> Phantoms.TTerm (Relational.Row v)
row x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.Row"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRow :: Phantoms.TTerm (Relational.Row v) -> Phantoms.TTerm [v]
unRow x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.relational.Row")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
