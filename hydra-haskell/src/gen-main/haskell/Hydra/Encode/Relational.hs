-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.relational

module Hydra.Encode.Relational where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Relational as Relational
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

columnName :: (Relational.ColumnName -> Core.Term)
columnName x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.relational.ColumnName"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Relational.unColumnName x))}))

columnSchema :: ((t0 -> Core.Term) -> Relational.ColumnSchema t0 -> Core.Term)
columnSchema t x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.relational.ColumnSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (columnName (Relational.columnSchemaName x))},
    Core.Field {
      Core.fieldName = (Core.Name "domain"),
      Core.fieldTerm = (t (Relational.columnSchemaDomain x))}]}))

foreignKey :: (Relational.ForeignKey -> Core.Term)
foreignKey x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.relational.ForeignKey"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "foreignRelation"),
      Core.fieldTerm = (relationName (Relational.foreignKeyForeignRelation x))},
    Core.Field {
      Core.fieldName = (Core.Name "keys"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap columnName columnName m)) (Relational.foreignKeyKeys x))}]}))

primaryKey :: (Relational.PrimaryKey -> Core.Term)
primaryKey x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.relational.PrimaryKey"),
  Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map columnName xs)) (Relational.unPrimaryKey x))}))

relation :: ((t0 -> Core.Term) -> Relational.Relation t0 -> Core.Term)
relation v x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.relational.Relation"),
  Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map (row v) xs)) (Relational.unRelation x))}))

relationName :: (Relational.RelationName -> Core.Term)
relationName x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.relational.RelationName"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Relational.unRelationName x))}))

relationSchema :: ((t0 -> Core.Term) -> Relational.RelationSchema t0 -> Core.Term)
relationSchema t x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.relational.RelationSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (relationName (Relational.relationSchemaName x))},
    Core.Field {
      Core.fieldName = (Core.Name "columns"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (columnSchema t) xs)) (Relational.relationSchemaColumns x))},
    Core.Field {
      Core.fieldName = (Core.Name "primaryKeys"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map primaryKey xs)) (Relational.relationSchemaPrimaryKeys x))},
    Core.Field {
      Core.fieldName = (Core.Name "foreignKeys"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map foreignKey xs)) (Relational.relationSchemaForeignKeys x))}]}))

relationship :: ((t0 -> Core.Term) -> Relational.Relationship t0 -> Core.Term)
relationship v x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.relational.Relationship"),
  Core.wrappedTermBody = ((\s -> Core.TermSet (Sets.map (\m -> Core.TermMap (Maps.bimap columnName v m)) s)) (Relational.unRelationship x))}))

row :: ((t0 -> Core.Term) -> Relational.Row t0 -> Core.Term)
row v x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.relational.Row"),
  Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map v xs)) (Relational.unRow x))}))
