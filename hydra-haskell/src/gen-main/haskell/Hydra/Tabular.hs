-- Note: this is an automatically generated file. Do not edit.

-- | A simple, untyped tabular data model, suitable for CSVs and TSVs

module Hydra.Tabular where

import qualified Hydra.Core as Core
import qualified Hydra.Relational as Relational
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A column type, consisting of a name and a value type
data ColumnType = 
  ColumnType {
    columnTypeName :: Relational.ColumnName,
    columnTypeType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_ColumnType = (Core.Name "hydra.tabular.ColumnType")

_ColumnType_name = (Core.Name "name")

_ColumnType_type = (Core.Name "type")

-- | A data row, containing optional-valued cells; one per column
newtype DataRow v = 
  DataRow {
    unDataRow :: [Maybe v]}
  deriving (Eq, Ord, Read, Show)

_DataRow = (Core.Name "hydra.tabular.DataRow")

-- | A header row, containing column names (but no types or data)
newtype HeaderRow = 
  HeaderRow {
    unHeaderRow :: [String]}
  deriving (Eq, Ord, Read, Show)

_HeaderRow = (Core.Name "hydra.tabular.HeaderRow")

-- | A simple table as in a CSV file, having an optional header row and any number of data rows
data Table v = 
  Table {
    -- | The optional header row of the table. If present, the header must have the same number of cells as each data row.
    tableHeader :: (Maybe HeaderRow),
    -- | The data rows of the table. Each row must have the same number of cells.
    tableData :: [DataRow v]}
  deriving (Eq, Ord, Read, Show)

_Table = (Core.Name "hydra.tabular.Table")

_Table_header = (Core.Name "header")

_Table_data = (Core.Name "data")

-- | A type definition for a table, including column names and types
data TableType = 
  TableType {
    tableTypeName :: Relational.RelationName,
    tableTypeColumns :: [ColumnType]}
  deriving (Eq, Ord, Read, Show)

_TableType = (Core.Name "hydra.tabular.TableType")

_TableType_name = (Core.Name "name")

_TableType_columns = (Core.Name "columns")
