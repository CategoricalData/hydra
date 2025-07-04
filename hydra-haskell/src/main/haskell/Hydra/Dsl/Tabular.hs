module Hydra.Dsl.Tabular where

import Hydra.Core
import Hydra.Relational


data ColumnType = ColumnType {
  columnTypeName ::  ColumnName,
  columnTypeType :: Type} deriving (Eq, Ord, Show)

data TableType = TableType {
  tableTypeName :: RelationName,
  tableTypeColumns :: [ColumnType]} deriving (Eq, Ord, Show)

columnType :: String -> Type -> ColumnType
columnType name typ = ColumnType (ColumnName name) typ

tableType :: String -> [ColumnType] -> TableType
tableType name columns = TableType (RelationName name) columns
