-- | A simple, untyped tabular data model, suitable for CSVs and TSVs

module Hydra.Ext.Tabular where

import qualified Hydra.Core as Core
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A data row, containing optional-valued cells; one per column
newtype DataRow v = 
  DataRow {
    unDataRow :: [Maybe v]}
  deriving (Eq, Ord, Read, Show)

_DataRow = (Core.Name "hydra.ext.tabular.DataRow")

-- | A header row, containing column names (but no types or data)
newtype HeaderRow = 
  HeaderRow {
    unHeaderRow :: [String]}
  deriving (Eq, Ord, Read, Show)

_HeaderRow = (Core.Name "hydra.ext.tabular.HeaderRow")

-- | A simple table as in a CSV file, having an optional header row and any number of data rows
data Table v = 
  Table {
    -- | The optional header row of the table. If present, the header must have the same number of cells as each data row.
    tableHeader :: (Maybe HeaderRow),
    -- | The data rows of the table. Each row must have the same number of cells.
    tableData :: [DataRow v]}
  deriving (Eq, Ord, Read, Show)

_Table = (Core.Name "hydra.ext.tabular.Table")

_Table_header = (Core.Name "header")

_Table_data = (Core.Name "data")
