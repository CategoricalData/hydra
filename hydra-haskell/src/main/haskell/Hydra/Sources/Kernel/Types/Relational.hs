{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Relational where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms                 as Terms
import           Hydra.Dsl.Types                 as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just ("An interpretation of Codd's Relational Model, " ++
      "as described in 'A Relational Model of Data for Large Shared Data Banks' (1970). " ++
      "Types ('domains') and values are parameterized so as to allow for application-specific implementations. " ++
      "No special support is provided for 'nonsimple' domains; i.e. relations are assumed to be normalized.")
  where
    ns = Namespace "hydra.relational"
    def = datatype ns
    rm = typeref ns

    elements = [
      def "ColumnName" $
        doc ("A name for a domain which serves to identify the role played by that domain in the given relation;"
          ++ " a 'role name' in Codd") $
        wrap string,

      def "ColumnSchema" $
        doc "An abstract specification of the domain represented by a column in a relation; a role" $
        forAll "t" $ record [
          "name">:
            doc "A unique name for the column" $
            rm "ColumnName",
          "domain">:
            doc "The domain (type) of the column" $
            "t"],

      def "ForeignKey" $
        doc "A mapping from certain columns of a source relation to primary key columns of a target relation" $
        record [
          "foreignRelation">:
            doc "The name of the target relation" $
            rm "RelationName",
          "keys">:
            doc ("The mapping of source column names to target column names."
               ++ " The target column names must together make up the primary key of the target relation.") $
            nonemptyMap (rm "ColumnName") (rm "ColumnName")],

      def "PrimaryKey" $
        doc "A primary key of a relation, specified either as a single column, or as a list of columns" $
        wrap $ nonemptyList $ rm "ColumnName",

      def "Relation" $
        doc "A set of distinct n-tuples; a table" $
        forAll "v" $ wrap $ list (rm "Row" @@ "v"),

      def "RelationName" $
        doc "A unique relation (table) name" $
        wrap string,

      def "RelationSchema" $ -- Note: this term is not in Codd
        doc "An abstract relation; the name and columns of a relation without its actual data" $
        forAll "t" $ record [
          "name">:
            doc "A unique name for the relation" $
            rm "RelationName",
          "columns">:
            doc "A list of column specifications" $
            nonemptyList $ rm "ColumnSchema" @@ "t",
          "primaryKeys">:
            doc "Any number of primary keys for the relation, each of which must be valid for this relation" $
            list $ rm "PrimaryKey",
          "foreignKeys">:
            doc "Any number of foreign keys, each of which must be valid for both this relation and the target relation" $
            list $ rm "ForeignKey"],

      def "Relationship" $
        doc "A domain-unordered (string-indexed, rather than position-indexed) relation" $
        forAll "v" $ wrap $ set $ Types.map (rm "ColumnName") "v",

      def "Row" $
        doc "An n-tuple which is an element of a given relation" $
        forAll "v" $ wrap $ nonemptyList "v"]
