-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.tabular

module Hydra.Encode.Tabular where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Encode.Relational as Relational
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Tabular as Tabular
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

columnType :: (Tabular.ColumnType -> Core.Term)
columnType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.tabular.ColumnType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Relational.columnName (Tabular.columnTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core_.type_ (Tabular.columnTypeType x))}]}))

dataRow :: ((t0 -> Core.Term) -> Tabular.DataRow t0 -> Core.Term)
dataRow v x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.tabular.DataRow"),
  Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map (\opt -> Core.TermMaybe (Maybes.map v opt)) xs)) (Tabular.unDataRow x))}))

headerRow :: (Tabular.HeaderRow -> Core.Term)
headerRow x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.tabular.HeaderRow"),
  Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map (\x -> Core.TermLiteral (Core.LiteralString x)) xs)) (Tabular.unHeaderRow x))}))

table :: ((t0 -> Core.Term) -> Tabular.Table t0 -> Core.Term)
table v x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.tabular.Table"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "header"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map headerRow opt)) (Tabular.tableHeader x))},
    Core.Field {
      Core.fieldName = (Core.Name "data"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (dataRow v) xs)) (Tabular.tableData x))}]}))

tableType :: (Tabular.TableType -> Core.Term)
tableType x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.tabular.TableType"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Relational.relationName (Tabular.tableTypeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "columns"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map columnType xs)) (Tabular.tableTypeColumns x))}]}))
