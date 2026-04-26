-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.relational

module Hydra.Decode.Relational where
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Relational as Relational
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
columnName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.ColumnName
columnName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.ColumnName b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
columnSchema :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.ColumnSchema t0)
columnSchema t cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" columnName fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "domain" t fieldMap cx) (\field_domain -> Right (Relational.ColumnSchema {
          Relational.columnSchemaName = field_name,
          Relational.columnSchemaDomain = field_domain}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
foreignKey :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.ForeignKey
foreignKey cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "foreignRelation" relationName fieldMap cx) (\field_foreignRelation -> Eithers.bind (ExtractCore.requireField "keys" (ExtractCore.decodeMap columnName columnName) fieldMap cx) (\field_keys -> Right (Relational.ForeignKey {
          Relational.foreignKeyForeignRelation = field_foreignRelation,
          Relational.foreignKeyKeys = field_keys}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
primaryKey :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.PrimaryKey
primaryKey cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.PrimaryKey b) (ExtractCore.decodeList columnName cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
relation :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.Relation t0)
relation v cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.Relation b) (ExtractCore.decodeList (row v) cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
relationName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.RelationName
relationName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.RelationName b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
relationSchema :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.RelationSchema t0)
relationSchema t cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" relationName fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "columns" (ExtractCore.decodeList (columnSchema t)) fieldMap cx) (\field_columns -> Eithers.bind (ExtractCore.requireField "primaryKeys" (ExtractCore.decodeList primaryKey) fieldMap cx) (\field_primaryKeys -> Eithers.bind (ExtractCore.requireField "foreignKeys" (ExtractCore.decodeList foreignKey) fieldMap cx) (\field_foreignKeys -> Right (Relational.RelationSchema {
          Relational.relationSchemaName = field_name,
          Relational.relationSchemaColumns = field_columns,
          Relational.relationSchemaPrimaryKeys = field_primaryKeys,
          Relational.relationSchemaForeignKeys = field_foreignKeys}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
relationship :: Ord t0 => ((Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.Relationship t0))
relationship v cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.Relationship b) (ExtractCore.decodeSet (ExtractCore.decodeMap columnName v) cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
row :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.Row t0)
row v cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.Row b) (ExtractCore.decodeList v cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
