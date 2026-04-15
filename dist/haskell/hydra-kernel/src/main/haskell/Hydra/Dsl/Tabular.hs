-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.tabular

module Hydra.Dsl.Tabular where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

columnType :: Phantoms.TTerm Relational.ColumnName -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Tabular.ColumnType
columnType name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.ColumnType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

columnTypeName :: Phantoms.TTerm Tabular.ColumnType -> Phantoms.TTerm Relational.ColumnName
columnTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnTypeType :: Phantoms.TTerm Tabular.ColumnType -> Phantoms.TTerm Core.Type
columnTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnTypeWithName :: Phantoms.TTerm Tabular.ColumnType -> Phantoms.TTerm Relational.ColumnName -> Phantoms.TTerm Tabular.ColumnType
columnTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.ColumnType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnTypeWithType :: Phantoms.TTerm Tabular.ColumnType -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Tabular.ColumnType
columnTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.ColumnType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataRow :: Phantoms.TTerm [Maybe v] -> Phantoms.TTerm (Tabular.DataRow v)
dataRow x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tabular.DataRow"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

headerRow :: Phantoms.TTerm [String] -> Phantoms.TTerm Tabular.HeaderRow
headerRow x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tabular.HeaderRow"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

table :: Phantoms.TTerm (Maybe Tabular.HeaderRow) -> Phantoms.TTerm [Tabular.DataRow v] -> Phantoms.TTerm (Tabular.Table v)
table header data_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.Table"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "data"),
          Core.fieldTerm = (Phantoms.unTTerm data_)}]}))

tableData :: Phantoms.TTerm (Tabular.Table v) -> Phantoms.TTerm [Tabular.DataRow v]
tableData x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
        Core.projectionField = (Core.Name "data")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableHeader :: Phantoms.TTerm (Tabular.Table v) -> Phantoms.TTerm (Maybe Tabular.HeaderRow)
tableHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
        Core.projectionField = (Core.Name "header")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableType :: Phantoms.TTerm Relational.RelationName -> Phantoms.TTerm [Tabular.ColumnType] -> Phantoms.TTerm Tabular.TableType
tableType name columns =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.TableType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm columns)}]}))

tableTypeColumns :: Phantoms.TTerm Tabular.TableType -> Phantoms.TTerm [Tabular.ColumnType]
tableTypeColumns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
        Core.projectionField = (Core.Name "columns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableTypeName :: Phantoms.TTerm Tabular.TableType -> Phantoms.TTerm Relational.RelationName
tableTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tableTypeWithColumns :: Phantoms.TTerm Tabular.TableType -> Phantoms.TTerm [Tabular.ColumnType] -> Phantoms.TTerm Tabular.TableType
tableTypeWithColumns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.TableType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tableTypeWithName :: Phantoms.TTerm Tabular.TableType -> Phantoms.TTerm Relational.RelationName -> Phantoms.TTerm Tabular.TableType
tableTypeWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.TableType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
              Core.projectionField = (Core.Name "columns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tableWithData :: Phantoms.TTerm (Tabular.Table v) -> Phantoms.TTerm [Tabular.DataRow v] -> Phantoms.TTerm (Tabular.Table v)
tableWithData original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.Table"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
              Core.projectionField = (Core.Name "header")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "data"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tableWithHeader :: Phantoms.TTerm (Tabular.Table v) -> Phantoms.TTerm (Maybe Tabular.HeaderRow) -> Phantoms.TTerm (Tabular.Table v)
tableWithHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.Table"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "data"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
              Core.projectionField = (Core.Name "data")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unDataRow :: Phantoms.TTerm (Tabular.DataRow v) -> Phantoms.TTerm [Maybe v]
unDataRow x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tabular.DataRow")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unHeaderRow :: Phantoms.TTerm Tabular.HeaderRow -> Phantoms.TTerm [String]
unHeaderRow x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tabular.HeaderRow")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
