-- | An interpretation of Codd's Relational Model, as described in 'A Relational Model of Data for Large Shared Data Banks' (1970). Types ('domains') and values are parameterized so as to allow for application-specific implementations. No special support is provided for 'nonsimple' domains; i.e. relations are assumed to be normalized.

module Hydra.Langs.RelationalModel where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A name for a domain which serves to identify the role played by that domain in the given relation; a 'role name' in Codd
newtype ColumnName = 
  ColumnName {
    unColumnName :: String}
  deriving (Eq, Ord, Read, Show)

_ColumnName = (Core.Name "hydra/langs/relationalModel.ColumnName")

_ColumnName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | An abstract specification of the domain represented by a column in a relation; a role
data ColumnSchema t = 
  ColumnSchema {
    -- | A unique name for the column
    columnSchemaName :: ColumnName,
    -- | The domain (type) of the column
    columnSchemaDomain :: t,
    -- | Whether this column represents the primary key of its relation
    columnSchemaIsPrimaryKey :: Bool}
  deriving (Eq, Ord, Read, Show)

_ColumnSchema = (Core.Name "hydra/langs/relationalModel.ColumnSchema")

_ColumnSchema_name = (Core.Name "name")

_ColumnSchema_domain = (Core.Name "domain")

_ColumnSchema_isPrimaryKey = (Core.Name "isPrimaryKey")

_ColumnSchema_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "t"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/relationalModel.ColumnSchema"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "name"),
        Core.fieldTypeType = _ColumnName_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "domain"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "t"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "isPrimaryKey"),
        Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))}))

-- | A mapping from certain columns of a source relation to primary key columns of a target relation
data ForeignKey = 
  ForeignKey {
    -- | The name of the target relation
    foreignKeyForeignRelation :: RelationName,
    foreignKeyKeys :: (Map ColumnName ColumnName)}
  deriving (Eq, Ord, Read, Show)

_ForeignKey = (Core.Name "hydra/langs/relationalModel.ForeignKey")

_ForeignKey_foreignRelation = (Core.Name "foreignRelation")

_ForeignKey_keys = (Core.Name "keys")

_ForeignKey_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/relationalModel.ForeignKey"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "foreignRelation"),
      Core.fieldTypeType = _RelationName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keys"),
      Core.fieldTypeType = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = _ColumnName_type_,
        Core.mapTypeValues = _ColumnName_type_}))}]}))

-- | A primary key of a relation, specified either as a single column, or as a list of columns
newtype PrimaryKey = 
  PrimaryKey {
    unPrimaryKey :: [ColumnName]}
  deriving (Eq, Ord, Read, Show)

_PrimaryKey = (Core.Name "hydra/langs/relationalModel.PrimaryKey")

_PrimaryKey_type_ = (Core.TypeList _ColumnName_type_)

-- | A set of distinct n-tuples; a table
newtype Relation v = 
  Relation {
    unRelation :: (Set [v])}
  deriving (Eq, Ord, Read, Show)

_Relation = (Core.Name "hydra/langs/relationalModel.Relation")

_Relation_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeSet (Core.TypeList (Core.TypeVariable (Core.Name "v"))))}))

-- | A unique relation (table) name
newtype RelationName = 
  RelationName {
    unRelationName :: String}
  deriving (Eq, Ord, Read, Show)

_RelationName = (Core.Name "hydra/langs/relationalModel.RelationName")

_RelationName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | An abstract relation; the name and columns of a relation without its actual data
data RelationSchema t = 
  RelationSchema {
    -- | A unique name for the relation
    relationSchemaName :: RelationName,
    -- | A list of column specifications
    relationSchemaColumns :: [ColumnSchema t],
    -- | Any number of primary keys for the relation, each of which must be valid for this relation
    relationSchemaPrimaryKeys :: [PrimaryKey],
    -- | Any number of foreign keys, each of which must be valid for both this relation and the target relation
    relationSchemaForeignKeys :: [ForeignKey]}
  deriving (Eq, Ord, Read, Show)

_RelationSchema = (Core.Name "hydra/langs/relationalModel.RelationSchema")

_RelationSchema_name = (Core.Name "name")

_RelationSchema_columns = (Core.Name "columns")

_RelationSchema_primaryKeys = (Core.Name "primaryKeys")

_RelationSchema_foreignKeys = (Core.Name "foreignKeys")

_RelationSchema_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "t"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/relationalModel.RelationSchema"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "name"),
        Core.fieldTypeType = _RelationName_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "columns"),
        Core.fieldTypeType = (Core.TypeList (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _ColumnSchema_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t"))})))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "primaryKeys"),
        Core.fieldTypeType = (Core.TypeList _PrimaryKey_type_)},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "foreignKeys"),
        Core.fieldTypeType = (Core.TypeList _ForeignKey_type_)}]}))}))

-- | A domain-unordered (string-indexed, rather than position-indexed) relation
newtype Relationship v = 
  Relationship {
    unRelationship :: (Set (Map ColumnName v))}
  deriving (Eq, Ord, Read, Show)

_Relationship = (Core.Name "hydra/langs/relationalModel.Relationship")

_Relationship_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeSet (Core.TypeMap (Core.MapType {
    Core.mapTypeKeys = _ColumnName_type_,
    Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})))}))

-- | An n-tuple which is an element of a given relation
newtype Row v = 
  Row {
    unRow :: [v]}
  deriving (Eq, Ord, Read, Show)

_Row = (Core.Name "hydra/langs/relationalModel.Row")

_Row_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "v")))}))