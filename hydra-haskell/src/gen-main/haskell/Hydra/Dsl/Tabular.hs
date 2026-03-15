-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.tabular

module Hydra.Dsl.Tabular where

import qualified Hydra.Core as Core
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

columnType :: (Relational.ColumnName -> Core.Type -> Tabular.ColumnType)
columnType name type_ = Tabular.ColumnType {
  Tabular.columnTypeName = name,
  Tabular.columnTypeType = type_}

columnTypeName :: (Tabular.ColumnType -> Relational.ColumnName)
columnTypeName = Tabular.columnTypeName

columnTypeType :: (Tabular.ColumnType -> Core.Type)
columnTypeType = Tabular.columnTypeType

columnTypeWithName :: (Tabular.ColumnType -> Relational.ColumnName -> Tabular.ColumnType)
columnTypeWithName original newVal = Tabular.ColumnType {
  Tabular.columnTypeName = newVal,
  Tabular.columnTypeType = (Tabular.columnTypeType original)}

columnTypeWithType :: (Tabular.ColumnType -> Core.Type -> Tabular.ColumnType)
columnTypeWithType original newVal = Tabular.ColumnType {
  Tabular.columnTypeName = (Tabular.columnTypeName original),
  Tabular.columnTypeType = newVal}

dataRow :: ([Maybe t0] -> Tabular.DataRow t0)
dataRow x = (Tabular.DataRow x)

unDataRow :: (Tabular.DataRow t0 -> [Maybe t0])
unDataRow = Tabular.unDataRow

headerRow :: ([String] -> Tabular.HeaderRow)
headerRow x = (Tabular.HeaderRow x)

unHeaderRow :: (Tabular.HeaderRow -> [String])
unHeaderRow = Tabular.unHeaderRow

table :: (Maybe Tabular.HeaderRow -> [Tabular.DataRow t0] -> Tabular.Table t0)
table header data_ = Tabular.Table {
  Tabular.tableHeader = header,
  Tabular.tableData = data_}

tableHeader :: (Tabular.Table t0 -> Maybe Tabular.HeaderRow)
tableHeader = Tabular.tableHeader

tableData :: (Tabular.Table t0 -> [Tabular.DataRow t0])
tableData = Tabular.tableData

tableWithHeader :: (Tabular.Table t0 -> Maybe Tabular.HeaderRow -> Tabular.Table t0)
tableWithHeader original newVal = Tabular.Table {
  Tabular.tableHeader = newVal,
  Tabular.tableData = (Tabular.tableData original)}

tableWithData :: (Tabular.Table t0 -> [Tabular.DataRow t1] -> Tabular.Table t1)
tableWithData original newVal = Tabular.Table {
  Tabular.tableHeader = (Tabular.tableHeader original),
  Tabular.tableData = newVal}

tableType :: (Relational.RelationName -> [Tabular.ColumnType] -> Tabular.TableType)
tableType name columns = Tabular.TableType {
  Tabular.tableTypeName = name,
  Tabular.tableTypeColumns = columns}

tableTypeName :: (Tabular.TableType -> Relational.RelationName)
tableTypeName = Tabular.tableTypeName

tableTypeColumns :: (Tabular.TableType -> [Tabular.ColumnType])
tableTypeColumns = Tabular.tableTypeColumns

tableTypeWithName :: (Tabular.TableType -> Relational.RelationName -> Tabular.TableType)
tableTypeWithName original newVal = Tabular.TableType {
  Tabular.tableTypeName = newVal,
  Tabular.tableTypeColumns = (Tabular.tableTypeColumns original)}

tableTypeWithColumns :: (Tabular.TableType -> [Tabular.ColumnType] -> Tabular.TableType)
tableTypeWithColumns original newVal = Tabular.TableType {
  Tabular.tableTypeName = (Tabular.tableTypeName original),
  Tabular.tableTypeColumns = newVal}
