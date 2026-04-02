-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.relational

module Hydra.Decode.Relational where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Relational as Relational
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

columnName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.ColumnName
columnName cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.ColumnName b) ((\raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

columnSchema :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.ColumnSchema t0)
columnSchema t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "name" columnName fieldMap cx) (\field_name -> Eithers.bind (Core_.requireField "domain" t fieldMap cx) (\field_domain -> Right (Relational.ColumnSchema {
          Relational.columnSchemaName = field_name,
          Relational.columnSchemaDomain = field_domain}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

foreignKey :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.ForeignKey
foreignKey cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "foreignRelation" relationName fieldMap cx) (\field_foreignRelation -> Eithers.bind (Core_.requireField "keys" (Core_.decodeMap columnName columnName) fieldMap cx) (\field_keys -> Right (Relational.ForeignKey {
          Relational.foreignKeyForeignRelation = field_foreignRelation,
          Relational.foreignKeyKeys = field_keys}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

primaryKey :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.PrimaryKey
primaryKey cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.PrimaryKey b) (Core_.decodeList columnName cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

relation :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.Relation t0)
relation v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.Relation b) (Core_.decodeList (row v) cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

relationName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.RelationName
relationName cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.RelationName b) ((\raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

relationSchema :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.RelationSchema t0)
relationSchema t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "name" relationName fieldMap cx) (\field_name -> Eithers.bind (Core_.requireField "columns" (Core_.decodeList (columnSchema t)) fieldMap cx) (\field_columns -> Eithers.bind (Core_.requireField "primaryKeys" (Core_.decodeList primaryKey) fieldMap cx) (\field_primaryKeys -> Eithers.bind (Core_.requireField "foreignKeys" (Core_.decodeList foreignKey) fieldMap cx) (\field_foreignKeys -> Right (Relational.RelationSchema {
          Relational.relationSchemaName = field_name,
          Relational.relationSchemaColumns = field_columns,
          Relational.relationSchemaPrimaryKeys = field_primaryKeys,
          Relational.relationSchemaForeignKeys = field_foreignKeys}))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

relationship :: Ord t0 => ((Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.Relationship t0))
relationship v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.Relationship b) (Core_.decodeSet (Core_.decodeMap columnName v) cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

row :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.Row t0)
row v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.Row b) (Core_.decodeList v cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)
