{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Relational where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc, nonemptyList, nonemptyMap)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.relational"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just ("An interpretation of Codd's Relational Model, " ++
      "as described in 'A Relational Model of Data for Large Shared Data Banks' (1970). " ++
      "Types ('domains') and values are parameterized so as to allow for application-specific implementations. " ++
      "No special support is provided for 'nonsimple' domains; i.e. relations are assumed to be normalized.")
  where
    elements = [
      columnName,
      columnSchema,
      foreignKey,
      primaryKey,
      relation,
      relationName,
      relationSchema,
      relationship,
      row]

columnName :: Binding
columnName = define "ColumnName" $
  doc ("A name for a domain which serves to identify the role played by that domain in the given relation;"
    ++ " a 'role name' in Codd") $
  T.wrap T.string

columnSchema :: Binding
columnSchema = define "ColumnSchema" $
  doc "An abstract specification of the domain represented by a column in a relation; a role" $
  T.forAll "t" $ T.record [
    "name">:
      doc "A unique name for the column" $
      use columnName,
    "domain">:
      doc "The domain (type) of the column" $
      T.var "t"]

foreignKey :: Binding
foreignKey = define "ForeignKey" $
  doc "A mapping from certain columns of a source relation to primary key columns of a target relation" $
  T.record [
    "foreignRelation">:
      doc "The name of the target relation" $
      use relationName,
    "keys">:
      doc ("The mapping of source column names to target column names."
         ++ " The target column names must together make up the primary key of the target relation.") $
      nonemptyMap (use columnName) (use columnName)]

primaryKey :: Binding
primaryKey = define "PrimaryKey" $
  doc "A primary key of a relation, specified either as a single column, or as a list of columns" $
  T.wrap $ nonemptyList $ use columnName

relation :: Binding
relation = define "Relation" $
  doc "A set of distinct n-tuples; a table" $
  T.forAll "v" $ T.wrap $ T.list (use row @@ T.var "v")

relationName :: Binding
relationName = define "RelationName" $
  doc "A unique relation (table) name" $
  T.wrap T.string

relationSchema :: Binding
relationSchema = define "RelationSchema" $
  doc "An abstract relation; the name and columns of a relation without its actual data" $
  T.forAll "t" $ T.record [
    "name">:
      doc "A unique name for the relation" $
      use relationName,
    "columns">:
      doc "A list of column specifications" $
      nonemptyList $ use columnSchema @@ T.var "t",
    "primaryKeys">:
      doc "Any number of primary keys for the relation, each of which must be valid for this relation" $
      T.list $ use primaryKey,
    "foreignKeys">:
      doc "Any number of foreign keys, each of which must be valid for both this relation and the target relation" $
      T.list $ use foreignKey]

relationship :: Binding
relationship = define "Relationship" $
  doc "A domain-unordered (string-indexed, rather than position-indexed) relation" $
  T.forAll "v" $ T.wrap $ T.set $ T.map (use columnName) (T.var "v")

row :: Binding
row = define "Row" $
  doc "An n-tuple which is an element of a given relation" $
  T.forAll "v" $ T.wrap $ nonemptyList (T.var "v")
