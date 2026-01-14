-- | DSL helpers for constructing tabular (relational) schemas

module Hydra.Dsl.Tabular where

import Hydra.Core
import Hydra.Relational
import Hydra.Tabular


columnType :: String -> Type -> ColumnType
columnType name typ = ColumnType (ColumnName name) typ

tableType :: String -> [ColumnType] -> TableType
tableType name columns = TableType (RelationName name) columns
