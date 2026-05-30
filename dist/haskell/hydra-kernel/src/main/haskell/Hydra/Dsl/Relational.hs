-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.relational

module Hydra.Dsl.Relational where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Relational as Relational
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for the hydra.relational.ColumnName wrapper
columnName :: Typed.TypedTerm String -> Typed.TypedTerm Relational.ColumnName
columnName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.ColumnName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.relational.ColumnSchema
columnSchema :: Typed.TypedTerm Relational.ColumnName -> Typed.TypedTerm t -> Typed.TypedTerm (Relational.ColumnSchema t)
columnSchema name domain =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ColumnSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm domain)}]}))
-- | DSL accessor for the domain field of hydra.relational.ColumnSchema
columnSchemaDomain :: Typed.TypedTerm (Relational.ColumnSchema t) -> Typed.TypedTerm t
columnSchemaDomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
        Core.projectionFieldName = (Core.Name "domain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.relational.ColumnSchema
columnSchemaName :: Typed.TypedTerm (Relational.ColumnSchema t) -> Typed.TypedTerm Relational.ColumnName
columnSchemaName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the domain field of hydra.relational.ColumnSchema
columnSchemaWithDomain :: Typed.TypedTerm (Relational.ColumnSchema t) -> Typed.TypedTerm t -> Typed.TypedTerm (Relational.ColumnSchema t)
columnSchemaWithDomain original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ColumnSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.relational.ColumnSchema
columnSchemaWithName :: Typed.TypedTerm (Relational.ColumnSchema t) -> Typed.TypedTerm Relational.ColumnName -> Typed.TypedTerm (Relational.ColumnSchema t)
columnSchemaWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ColumnSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.ColumnSchema"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.relational.ForeignKey
foreignKey :: Typed.TypedTerm Relational.RelationName -> Typed.TypedTerm (M.Map Relational.ColumnName Relational.ColumnName) -> Typed.TypedTerm Relational.ForeignKey
foreignKey foreignRelation keys =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ForeignKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "foreignRelation"),
          Core.fieldTerm = (Typed.unTypedTerm foreignRelation)},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Typed.unTypedTerm keys)}]}))
-- | DSL accessor for the foreignRelation field of hydra.relational.ForeignKey
foreignKeyForeignRelation :: Typed.TypedTerm Relational.ForeignKey -> Typed.TypedTerm Relational.RelationName
foreignKeyForeignRelation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
        Core.projectionFieldName = (Core.Name "foreignRelation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the keys field of hydra.relational.ForeignKey
foreignKeyKeys :: Typed.TypedTerm Relational.ForeignKey -> Typed.TypedTerm (M.Map Relational.ColumnName Relational.ColumnName)
foreignKeyKeys x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
        Core.projectionFieldName = (Core.Name "keys")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the foreignRelation field of hydra.relational.ForeignKey
foreignKeyWithForeignRelation :: Typed.TypedTerm Relational.ForeignKey -> Typed.TypedTerm Relational.RelationName -> Typed.TypedTerm Relational.ForeignKey
foreignKeyWithForeignRelation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ForeignKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "foreignRelation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
              Core.projectionFieldName = (Core.Name "keys")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the keys field of hydra.relational.ForeignKey
foreignKeyWithKeys :: Typed.TypedTerm Relational.ForeignKey -> Typed.TypedTerm (M.Map Relational.ColumnName Relational.ColumnName) -> Typed.TypedTerm Relational.ForeignKey
foreignKeyWithKeys original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.ForeignKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "foreignRelation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.ForeignKey"),
              Core.projectionFieldName = (Core.Name "foreignRelation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.relational.PrimaryKey wrapper
primaryKey :: Typed.TypedTerm [Relational.ColumnName] -> Typed.TypedTerm Relational.PrimaryKey
primaryKey x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.PrimaryKey"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.relational.Relation wrapper
relation :: Typed.TypedTerm [Relational.Row v] -> Typed.TypedTerm (Relational.Relation v)
relation x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.Relation"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.relational.RelationName wrapper
relationName :: Typed.TypedTerm String -> Typed.TypedTerm Relational.RelationName
relationName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.RelationName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.relational.RelationSchema
relationSchema :: Typed.TypedTerm Relational.RelationName -> Typed.TypedTerm [Relational.ColumnSchema t] -> Typed.TypedTerm [Relational.PrimaryKey] -> Typed.TypedTerm [Relational.ForeignKey] -> Typed.TypedTerm (Relational.RelationSchema t)
relationSchema name columns primaryKeys foreignKeys =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Typed.unTypedTerm columns)},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Typed.unTypedTerm primaryKeys)},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Typed.unTypedTerm foreignKeys)}]}))
-- | DSL accessor for the columns field of hydra.relational.RelationSchema
relationSchemaColumns :: Typed.TypedTerm (Relational.RelationSchema t) -> Typed.TypedTerm [Relational.ColumnSchema t]
relationSchemaColumns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
        Core.projectionFieldName = (Core.Name "columns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the foreignKeys field of hydra.relational.RelationSchema
relationSchemaForeignKeys :: Typed.TypedTerm (Relational.RelationSchema t) -> Typed.TypedTerm [Relational.ForeignKey]
relationSchemaForeignKeys x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
        Core.projectionFieldName = (Core.Name "foreignKeys")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.relational.RelationSchema
relationSchemaName :: Typed.TypedTerm (Relational.RelationSchema t) -> Typed.TypedTerm Relational.RelationName
relationSchemaName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primaryKeys field of hydra.relational.RelationSchema
relationSchemaPrimaryKeys :: Typed.TypedTerm (Relational.RelationSchema t) -> Typed.TypedTerm [Relational.PrimaryKey]
relationSchemaPrimaryKeys x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
        Core.projectionFieldName = (Core.Name "primaryKeys")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the columns field of hydra.relational.RelationSchema
relationSchemaWithColumns :: Typed.TypedTerm (Relational.RelationSchema t) -> Typed.TypedTerm [Relational.ColumnSchema t] -> Typed.TypedTerm (Relational.RelationSchema t)
relationSchemaWithColumns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "primaryKeys")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "foreignKeys")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the foreignKeys field of hydra.relational.RelationSchema
relationSchemaWithForeignKeys :: Typed.TypedTerm (Relational.RelationSchema t) -> Typed.TypedTerm [Relational.ForeignKey] -> Typed.TypedTerm (Relational.RelationSchema t)
relationSchemaWithForeignKeys original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "columns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "primaryKeys")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.relational.RelationSchema
relationSchemaWithName :: Typed.TypedTerm (Relational.RelationSchema t) -> Typed.TypedTerm Relational.RelationName -> Typed.TypedTerm (Relational.RelationSchema t)
relationSchemaWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "columns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "primaryKeys")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "foreignKeys")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the primaryKeys field of hydra.relational.RelationSchema
relationSchemaWithPrimaryKeys :: Typed.TypedTerm (Relational.RelationSchema t) -> Typed.TypedTerm [Relational.PrimaryKey] -> Typed.TypedTerm (Relational.RelationSchema t)
relationSchemaWithPrimaryKeys original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "columns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primaryKeys"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "foreignKeys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.relational.RelationSchema"),
              Core.projectionFieldName = (Core.Name "foreignKeys")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.relational.Relationship wrapper
relationship :: Typed.TypedTerm (S.Set (M.Map Relational.ColumnName v)) -> Typed.TypedTerm (Relational.Relationship v)
relationship x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.Relationship"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.relational.Row wrapper
row :: Typed.TypedTerm [v] -> Typed.TypedTerm (Relational.Row v)
row x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.relational.Row"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.relational.ColumnName
unColumnName :: Typed.TypedTerm Relational.ColumnName -> Typed.TypedTerm String
unColumnName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.relational.ColumnName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.relational.PrimaryKey
unPrimaryKey :: Typed.TypedTerm Relational.PrimaryKey -> Typed.TypedTerm [Relational.ColumnName]
unPrimaryKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.relational.PrimaryKey")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.relational.Relation
unRelation :: Typed.TypedTerm (Relational.Relation v) -> Typed.TypedTerm [Relational.Row v]
unRelation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.relational.Relation")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.relational.RelationName
unRelationName :: Typed.TypedTerm Relational.RelationName -> Typed.TypedTerm String
unRelationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.relational.RelationName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.relational.Relationship
unRelationship :: Typed.TypedTerm (Relational.Relationship v) -> Typed.TypedTerm (S.Set (M.Map Relational.ColumnName v))
unRelationship x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.relational.Relationship")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.relational.Row
unRow :: Typed.TypedTerm (Relational.Row v) -> Typed.TypedTerm [v]
unRow x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.relational.Row")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
