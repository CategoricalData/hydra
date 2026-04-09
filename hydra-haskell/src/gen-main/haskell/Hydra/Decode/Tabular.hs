-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.tabular

module Hydra.Decode.Tabular where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Relational as Relational
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Tabular as Tabular
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

columnType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Tabular.ColumnType
columnType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "name" Relational.columnName fieldMap cx) (\field_name -> Eithers.bind (Core__.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Right (Tabular.ColumnType {
          Tabular.columnTypeName = field_name,
          Tabular.columnTypeType = field_type}))))
      _ -> Left (Errors.DecodingError "expected record")) (Core__.stripWithDecodingError cx raw)

dataRow :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Tabular.DataRow t0)
dataRow v cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Tabular.DataRow b) (Core__.decodeList (Core__.decodeMaybe v) cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Core__.stripWithDecodingError cx raw)

headerRow :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Tabular.HeaderRow
headerRow cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Tabular.HeaderRow b) (Core__.decodeList (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Core__.stripWithDecodingError cx2 raw2)) cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Core__.stripWithDecodingError cx raw)

table :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Tabular.Table t0)
table v cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "header" (Core__.decodeMaybe headerRow) fieldMap cx) (\field_header -> Eithers.bind (Core__.requireField "data" (Core__.decodeList (dataRow v)) fieldMap cx) (\field_data -> Right (Tabular.Table {
          Tabular.tableHeader = field_header,
          Tabular.tableData = field_data}))))
      _ -> Left (Errors.DecodingError "expected record")) (Core__.stripWithDecodingError cx raw)

tableType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Tabular.TableType
tableType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "name" Relational.relationName fieldMap cx) (\field_name -> Eithers.bind (Core__.requireField "columns" (Core__.decodeList columnType) fieldMap cx) (\field_columns -> Right (Tabular.TableType {
          Tabular.tableTypeName = field_name,
          Tabular.tableTypeColumns = field_columns}))))
      _ -> Left (Errors.DecodingError "expected record")) (Core__.stripWithDecodingError cx raw)
