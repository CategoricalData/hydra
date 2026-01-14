module Hydra.Sources.Kernel.Types.Tabular where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Relational as Relational


ns :: Namespace
ns = Namespace "hydra.tabular"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.ns, Relational.ns] [Core.ns] $
    Just "A simple, untyped tabular data model, suitable for CSVs and TSVs"
  where
    elements = [
      columnType,
      dataRow,
      headerRow,
      table,
      tableType]

columnType :: Binding
columnType = define "ColumnType" $
 doc "A column type, consisting of a name and a value type" $
  T.record [
    "name">: Relational.columnName,
    "type">: Core.type_]

dataRow :: Binding
dataRow = define "DataRow" $
  doc "A data row, containing optional-valued cells; one per column" $
  T.forAll "v" $ T.wrap $ T.list $ T.maybe "v"

headerRow :: Binding
headerRow = define "HeaderRow" $
  doc "A header row, containing column names (but no types or data)" $
  T.wrap $ T.list T.string

table :: Binding
table = define "Table" $
  doc "A simple table as in a CSV file, having an optional header row and any number of data rows" $
  T.forAll "v" $ T.record [
    "header">:
      doc "The optional header row of the table. If present, the header must have the same number of cells as each data row." $
      T.maybe headerRow,
    "data">:
      doc "The data rows of the table. Each row must have the same number of cells." $
      T.list (dataRow @@ "v")]

tableType :: Binding
tableType = define "TableType" $
  doc "A type definition for a table, including column names and types" $
  T.record [
    "name">: Relational.relationName,
    "columns">: T.list columnType]
