-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.relational

module Hydra.Dsl.Relational where

import qualified Hydra.Relational as Relational
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

columnName :: (String -> Relational.ColumnName)
columnName x = (Relational.ColumnName x)

unColumnName :: (Relational.ColumnName -> String)
unColumnName = Relational.unColumnName

columnSchema :: (Relational.ColumnName -> t0 -> Relational.ColumnSchema t0)
columnSchema name domain = Relational.ColumnSchema {
  Relational.columnSchemaName = name,
  Relational.columnSchemaDomain = domain}

columnSchemaName :: (Relational.ColumnSchema t0 -> Relational.ColumnName)
columnSchemaName = Relational.columnSchemaName

columnSchemaDomain :: (Relational.ColumnSchema t0 -> t0)
columnSchemaDomain = Relational.columnSchemaDomain

columnSchemaWithName :: (Relational.ColumnSchema t0 -> Relational.ColumnName -> Relational.ColumnSchema t0)
columnSchemaWithName original newVal = Relational.ColumnSchema {
  Relational.columnSchemaName = newVal,
  Relational.columnSchemaDomain = (Relational.columnSchemaDomain original)}

columnSchemaWithDomain :: (Relational.ColumnSchema t0 -> t1 -> Relational.ColumnSchema t1)
columnSchemaWithDomain original newVal = Relational.ColumnSchema {
  Relational.columnSchemaName = (Relational.columnSchemaName original),
  Relational.columnSchemaDomain = newVal}

foreignKey :: (Relational.RelationName -> M.Map Relational.ColumnName Relational.ColumnName -> Relational.ForeignKey)
foreignKey foreignRelation keys = Relational.ForeignKey {
  Relational.foreignKeyForeignRelation = foreignRelation,
  Relational.foreignKeyKeys = keys}

foreignKeyForeignRelation :: (Relational.ForeignKey -> Relational.RelationName)
foreignKeyForeignRelation = Relational.foreignKeyForeignRelation

foreignKeyKeys :: (Relational.ForeignKey -> M.Map Relational.ColumnName Relational.ColumnName)
foreignKeyKeys = Relational.foreignKeyKeys

foreignKeyWithForeignRelation :: (Relational.ForeignKey -> Relational.RelationName -> Relational.ForeignKey)
foreignKeyWithForeignRelation original newVal = Relational.ForeignKey {
  Relational.foreignKeyForeignRelation = newVal,
  Relational.foreignKeyKeys = (Relational.foreignKeyKeys original)}

foreignKeyWithKeys :: (Relational.ForeignKey -> M.Map Relational.ColumnName Relational.ColumnName -> Relational.ForeignKey)
foreignKeyWithKeys original newVal = Relational.ForeignKey {
  Relational.foreignKeyForeignRelation = (Relational.foreignKeyForeignRelation original),
  Relational.foreignKeyKeys = newVal}

primaryKey :: ([Relational.ColumnName] -> Relational.PrimaryKey)
primaryKey x = (Relational.PrimaryKey x)

unPrimaryKey :: (Relational.PrimaryKey -> [Relational.ColumnName])
unPrimaryKey = Relational.unPrimaryKey

relation :: ([Relational.Row t0] -> Relational.Relation t0)
relation x = (Relational.Relation x)

unRelation :: (Relational.Relation t0 -> [Relational.Row t0])
unRelation = Relational.unRelation

relationName :: (String -> Relational.RelationName)
relationName x = (Relational.RelationName x)

unRelationName :: (Relational.RelationName -> String)
unRelationName = Relational.unRelationName

relationSchema :: (Relational.RelationName -> [Relational.ColumnSchema t0] -> [Relational.PrimaryKey] -> [Relational.ForeignKey] -> Relational.RelationSchema t0)
relationSchema name columns primaryKeys foreignKeys = Relational.RelationSchema {
  Relational.relationSchemaName = name,
  Relational.relationSchemaColumns = columns,
  Relational.relationSchemaPrimaryKeys = primaryKeys,
  Relational.relationSchemaForeignKeys = foreignKeys}

relationSchemaName :: (Relational.RelationSchema t0 -> Relational.RelationName)
relationSchemaName = Relational.relationSchemaName

relationSchemaColumns :: (Relational.RelationSchema t0 -> [Relational.ColumnSchema t0])
relationSchemaColumns = Relational.relationSchemaColumns

relationSchemaPrimaryKeys :: (Relational.RelationSchema t0 -> [Relational.PrimaryKey])
relationSchemaPrimaryKeys = Relational.relationSchemaPrimaryKeys

relationSchemaForeignKeys :: (Relational.RelationSchema t0 -> [Relational.ForeignKey])
relationSchemaForeignKeys = Relational.relationSchemaForeignKeys

relationSchemaWithName :: (Relational.RelationSchema t0 -> Relational.RelationName -> Relational.RelationSchema t0)
relationSchemaWithName original newVal = Relational.RelationSchema {
  Relational.relationSchemaName = newVal,
  Relational.relationSchemaColumns = (Relational.relationSchemaColumns original),
  Relational.relationSchemaPrimaryKeys = (Relational.relationSchemaPrimaryKeys original),
  Relational.relationSchemaForeignKeys = (Relational.relationSchemaForeignKeys original)}

relationSchemaWithColumns :: (Relational.RelationSchema t0 -> [Relational.ColumnSchema t1] -> Relational.RelationSchema t1)
relationSchemaWithColumns original newVal = Relational.RelationSchema {
  Relational.relationSchemaName = (Relational.relationSchemaName original),
  Relational.relationSchemaColumns = newVal,
  Relational.relationSchemaPrimaryKeys = (Relational.relationSchemaPrimaryKeys original),
  Relational.relationSchemaForeignKeys = (Relational.relationSchemaForeignKeys original)}

relationSchemaWithPrimaryKeys :: (Relational.RelationSchema t0 -> [Relational.PrimaryKey] -> Relational.RelationSchema t0)
relationSchemaWithPrimaryKeys original newVal = Relational.RelationSchema {
  Relational.relationSchemaName = (Relational.relationSchemaName original),
  Relational.relationSchemaColumns = (Relational.relationSchemaColumns original),
  Relational.relationSchemaPrimaryKeys = newVal,
  Relational.relationSchemaForeignKeys = (Relational.relationSchemaForeignKeys original)}

relationSchemaWithForeignKeys :: (Relational.RelationSchema t0 -> [Relational.ForeignKey] -> Relational.RelationSchema t0)
relationSchemaWithForeignKeys original newVal = Relational.RelationSchema {
  Relational.relationSchemaName = (Relational.relationSchemaName original),
  Relational.relationSchemaColumns = (Relational.relationSchemaColumns original),
  Relational.relationSchemaPrimaryKeys = (Relational.relationSchemaPrimaryKeys original),
  Relational.relationSchemaForeignKeys = newVal}

relationship :: (S.Set (M.Map Relational.ColumnName t0) -> Relational.Relationship t0)
relationship x = (Relational.Relationship x)

unRelationship :: (Relational.Relationship t0 -> S.Set (M.Map Relational.ColumnName t0))
unRelationship = Relational.unRelationship

row :: ([t0] -> Relational.Row t0)
row x = (Relational.Row x)

unRow :: (Relational.Row t0 -> [t0])
unRow = Relational.unRow
