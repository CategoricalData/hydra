-- | A simple, untyped tabular data model, suitable for CSVs and TSVs

module Hydra.Langs.Tabular where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A data row, containing optional-valued cells; one per column
newtype DataRow v = 
  DataRow {
    unDataRow :: [Maybe v]}
  deriving (Eq, Ord, Read, Show)

_DataRow = (Core.Name "hydra/langs/tabular.DataRow")

_DataRow_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeList (Core.TypeOptional (Core.TypeVariable (Core.Name "v"))))}))

-- | A header row, containing column names (but no types or data)
newtype HeaderRow = 
  HeaderRow {
    unHeaderRow :: [String]}
  deriving (Eq, Ord, Read, Show)

_HeaderRow = (Core.Name "hydra/langs/tabular.HeaderRow")

_HeaderRow_type_ = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))

-- | A simple table as in a CSV file, having an optional header row and any number of data rows
data Table v = 
  Table {
    -- | The optional header row of the table. If present, the header must have the same number of cells as each data row.
    tableHeader :: (Maybe HeaderRow),
    -- | The data rows of the table. Each row must have the same number of cells.
    tableData :: [DataRow v]}
  deriving (Eq, Ord, Read, Show)

_Table = (Core.Name "hydra/langs/tabular.Table")

_Table_header = (Core.Name "header")

_Table_data = (Core.Name "data")

_Table_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "v"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/tabular.Table"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "header"),
        Core.fieldTypeType = (Core.TypeOptional _HeaderRow_type_)},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "data"),
        Core.fieldTypeType = (Core.TypeList (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _DataRow_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))})))}]}))}))