-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.tabular

module Hydra.Decode.Tabular where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Relational as Relational
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Tabular as Tabular
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

columnType :: Graph.Graph -> Core.Term -> Either Error.DecodingError Tabular.ColumnType
columnType cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->  
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "name" Relational.columnName fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Right (Tabular.ColumnType {
          Tabular.columnTypeName = field_name,
          Tabular.columnTypeType = field_type}))))
      _ -> Left (Error.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

dataRow :: (Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Error.DecodingError (Tabular.DataRow t0)
dataRow v cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Tabular.DataRow b) (Helpers.decodeList (Helpers.decodeMaybe v) cx (Core.wrappedTermBody v0))
      _ -> Left (Error.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

headerRow :: Graph.Graph -> Core.Term -> Either Error.DecodingError Tabular.HeaderRow
headerRow cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Tabular.HeaderRow b) (Helpers.decodeList (\cx -> \raw -> Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Error.DecodingError "expected string literal")
        _ -> Left (Error.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) cx (Core.wrappedTermBody v0))
      _ -> Left (Error.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

table :: (Graph.Graph -> Core.Term -> Either Error.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Error.DecodingError (Tabular.Table t0)
table v cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->  
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "header" (Helpers.decodeMaybe headerRow) fieldMap cx) (\field_header -> Eithers.bind (Helpers.requireField "data" (Helpers.decodeList (dataRow v)) fieldMap cx) (\field_data -> Right (Tabular.Table {
          Tabular.tableHeader = field_header,
          Tabular.tableData = field_data}))))
      _ -> Left (Error.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

tableType :: Graph.Graph -> Core.Term -> Either Error.DecodingError Tabular.TableType
tableType cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->  
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "name" Relational.relationName fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "columns" (Helpers.decodeList columnType) fieldMap cx) (\field_columns -> Right (Tabular.TableType {
          Tabular.tableTypeName = field_name,
          Tabular.tableTypeColumns = field_columns}))))
      _ -> Left (Error.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)
