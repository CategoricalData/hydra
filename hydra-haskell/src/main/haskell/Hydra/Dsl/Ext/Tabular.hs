module Hydra.Dsl.Ext.Tabular where

import Hydra.Core


newtype ColumnName = ColumnName String deriving (Eq, Ord, Show)

data ColumnType = ColumnType {
  columnTypeName ::  ColumnName,
  columnTypeType :: Type} deriving (Eq, Ord, Show)

newtype TableName = TableName String deriving (Eq, Ord, Show)

data TableType = TableType {
  tableTypeName :: TableName,
  tableTypeColumns :: [ColumnType]} deriving (Eq, Ord, Show)

columnType :: String -> Type -> ColumnType
columnType name typ = ColumnType (ColumnName name) typ

tableType :: String -> [ColumnType] -> TableType
tableType name columns = TableType (TableName name) columns
