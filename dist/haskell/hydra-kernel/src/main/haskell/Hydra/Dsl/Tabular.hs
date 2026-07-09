-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.tabular

module Hydra.Dsl.Tabular where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Tabular as DecodeTabular
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Encode.Tabular as EncodeTabular
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | DSL constructor for hydra.tabular.ColumnType
columnType :: Typed.TypedTerm Relational.ColumnName -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Tabular.ColumnType
columnType name type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.ColumnType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))

-- | DSL name token for hydra.tabular.ColumnType
columnTypeColumnType :: Typed.TypedName Tabular.ColumnType
columnTypeColumnType = Typed.TypedName (Core.Name "hydra.tabular.ColumnType")

-- | DSL accessor for the name field of hydra.tabular.ColumnType
columnTypeName :: Typed.TypedTerm Tabular.ColumnType -> Typed.TypedTerm Relational.ColumnName
columnTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the type field of hydra.tabular.ColumnType
columnTypeType :: Typed.TypedTerm Tabular.ColumnType -> Typed.TypedTerm Core.Type
columnTypeType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL updater for the name field of hydra.tabular.ColumnType
columnTypeWithName :: Typed.TypedTerm Tabular.ColumnType -> Typed.TypedTerm Relational.ColumnName -> Typed.TypedTerm Tabular.ColumnType
columnTypeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.ColumnType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL updater for the type field of hydra.tabular.ColumnType
columnTypeWithType :: Typed.TypedTerm Tabular.ColumnType -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Tabular.ColumnType
columnTypeWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.ColumnType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.ColumnType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))

-- | DSL constructor for the hydra.tabular.DataRow wrapper
dataRow :: Typed.TypedTerm [Maybe v] -> Typed.TypedTerm (Tabular.DataRow v)
dataRow x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tabular.DataRow"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))

-- | DSL name token for hydra.tabular.DataRow
dataRowDataRow :: Typed.TypedName (Tabular.DataRow v)
dataRowDataRow = Typed.TypedName (Core.Name "hydra.tabular.DataRow")

-- | DSL composition builder for the decoder of hydra.tabular.DataRow
decodeDataRow :: Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError v) -> Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError (Tabular.DataRow v))
decodeDataRow v =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.tabular.dataRow")),
      Core.applicationArgument = (Typed.unTypedTerm v)}))

-- | DSL composition builder for the decoder of hydra.tabular.Table
decodeTable :: Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError v) -> Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError (Tabular.Table v))
decodeTable v =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.tabular.table")),
      Core.applicationArgument = (Typed.unTypedTerm v)}))

-- | DSL composition builder for the encoder of hydra.tabular.DataRow
encodeDataRow :: Typed.TypedTerm (v -> Core.Term) -> Typed.TypedTerm (Tabular.DataRow v -> Core.Term)
encodeDataRow v =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.tabular.dataRow")),
      Core.applicationArgument = (Typed.unTypedTerm v)}))

-- | DSL composition builder for the encoder of hydra.tabular.Table
encodeTable :: Typed.TypedTerm (v -> Core.Term) -> Typed.TypedTerm (Tabular.Table v -> Core.Term)
encodeTable v =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.tabular.table")),
      Core.applicationArgument = (Typed.unTypedTerm v)}))

-- | DSL constructor for the hydra.tabular.HeaderRow wrapper
headerRow :: Typed.TypedTerm [String] -> Typed.TypedTerm Tabular.HeaderRow
headerRow x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.tabular.HeaderRow"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))

-- | DSL name token for hydra.tabular.HeaderRow
headerRowHeaderRow :: Typed.TypedName Tabular.HeaderRow
headerRowHeaderRow = Typed.TypedName (Core.Name "hydra.tabular.HeaderRow")

-- | DSL constructor for hydra.tabular.Table
table :: Typed.TypedTerm (Maybe Tabular.HeaderRow) -> Typed.TypedTerm [Tabular.DataRow v] -> Typed.TypedTerm (Tabular.Table v)
table header data_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.Table"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm header)},
        Core.Field {
          Core.fieldName = (Core.Name "data"),
          Core.fieldTerm = (Typed.unTypedTerm data_)}]}))

-- | DSL accessor for the data field of hydra.tabular.Table
tableData :: Typed.TypedTerm (Tabular.Table v) -> Typed.TypedTerm [Tabular.DataRow v]
tableData x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
        Core.projectionFieldName = (Core.Name "data")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the header field of hydra.tabular.Table
tableHeader :: Typed.TypedTerm (Tabular.Table v) -> Typed.TypedTerm (Maybe Tabular.HeaderRow)
tableHeader x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
        Core.projectionFieldName = (Core.Name "header")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL name token for hydra.tabular.Table
tableTable :: Typed.TypedName (Tabular.Table v)
tableTable = Typed.TypedName (Core.Name "hydra.tabular.Table")

-- | DSL constructor for hydra.tabular.TableType
tableType :: Typed.TypedTerm Relational.RelationName -> Typed.TypedTerm [Tabular.ColumnType] -> Typed.TypedTerm Tabular.TableType
tableType name columns =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.TableType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Typed.unTypedTerm columns)}]}))

-- | DSL accessor for the columns field of hydra.tabular.TableType
tableTypeColumns :: Typed.TypedTerm Tabular.TableType -> Typed.TypedTerm [Tabular.ColumnType]
tableTypeColumns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
        Core.projectionFieldName = (Core.Name "columns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the name field of hydra.tabular.TableType
tableTypeName :: Typed.TypedTerm Tabular.TableType -> Typed.TypedTerm Relational.RelationName
tableTypeName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL name token for hydra.tabular.TableType
tableTypeTableType :: Typed.TypedName Tabular.TableType
tableTypeTableType = Typed.TypedName (Core.Name "hydra.tabular.TableType")

-- | DSL updater for the columns field of hydra.tabular.TableType
tableTypeWithColumns :: Typed.TypedTerm Tabular.TableType -> Typed.TypedTerm [Tabular.ColumnType] -> Typed.TypedTerm Tabular.TableType
tableTypeWithColumns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.TableType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))

-- | DSL updater for the name field of hydra.tabular.TableType
tableTypeWithName :: Typed.TypedTerm Tabular.TableType -> Typed.TypedTerm Relational.RelationName -> Typed.TypedTerm Tabular.TableType
tableTypeWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.TableType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.TableType"),
              Core.projectionFieldName = (Core.Name "columns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL updater for the data field of hydra.tabular.Table
tableWithData :: Typed.TypedTerm (Tabular.Table v) -> Typed.TypedTerm [Tabular.DataRow v] -> Typed.TypedTerm (Tabular.Table v)
tableWithData original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.Table"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
              Core.projectionFieldName = (Core.Name "header")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "data"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))

-- | DSL updater for the header field of hydra.tabular.Table
tableWithHeader :: Typed.TypedTerm (Tabular.Table v) -> Typed.TypedTerm (Maybe Tabular.HeaderRow) -> Typed.TypedTerm (Tabular.Table v)
tableWithHeader original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.tabular.Table"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "header"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "data"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.tabular.Table"),
              Core.projectionFieldName = (Core.Name "data")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))

-- | DSL accessor for the body of hydra.tabular.DataRow
unDataRow :: Typed.TypedTerm (Tabular.DataRow v) -> Typed.TypedTerm [Maybe v]
unDataRow x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tabular.DataRow")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the body of hydra.tabular.HeaderRow
unHeaderRow :: Typed.TypedTerm Tabular.HeaderRow -> Typed.TypedTerm [String]
unHeaderRow x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.tabular.HeaderRow")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
