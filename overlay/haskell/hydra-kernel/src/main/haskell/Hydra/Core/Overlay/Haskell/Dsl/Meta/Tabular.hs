-- | DSL helpers for constructing tabular (relational) schemas

module Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular where

import Hydra.Core.Model
import Hydra.Core.Relational
import Hydra.Core.Tabular


columnType :: String -> Type -> ColumnType
columnType name typ = ColumnType (ColumnName name) typ

tableType :: String -> [ColumnType] -> TableType
tableType name columns = TableType (RelationName name) columns
