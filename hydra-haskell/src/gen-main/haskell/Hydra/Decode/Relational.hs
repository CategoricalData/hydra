-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.relational

module Hydra.Decode.Relational where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Helpers as Helpers
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
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.ColumnName b) ((\raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

columnSchema :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.ColumnSchema t0)
columnSchema t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "name" columnName fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "domain" t fieldMap cx) (\field_domain -> Right (Relational.ColumnSchema {
          Relational.columnSchemaName = field_name,
          Relational.columnSchemaDomain = field_domain}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

foreignKey :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.ForeignKey
foreignKey cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "foreignRelation" relationName fieldMap cx) (\field_foreignRelation -> Eithers.bind (Helpers.requireField "keys" (Helpers.decodeMap columnName columnName) fieldMap cx) (\field_keys -> Right (Relational.ForeignKey {
          Relational.foreignKeyForeignRelation = field_foreignRelation,
          Relational.foreignKeyKeys = field_keys}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

primaryKey :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.PrimaryKey
primaryKey cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.PrimaryKey b) (Helpers.decodeList columnName cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

relation :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.Relation t0)
relation v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.Relation b) (Helpers.decodeList (row v) cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

relationName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Relational.RelationName
relationName cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.RelationName b) ((\raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

relationSchema :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.RelationSchema t0)
relationSchema t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "name" relationName fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "columns" (Helpers.decodeList (columnSchema t)) fieldMap cx) (\field_columns -> Eithers.bind (Helpers.requireField "primaryKeys" (Helpers.decodeList primaryKey) fieldMap cx) (\field_primaryKeys -> Eithers.bind (Helpers.requireField "foreignKeys" (Helpers.decodeList foreignKey) fieldMap cx) (\field_foreignKeys -> Right (Relational.RelationSchema {
          Relational.relationSchemaName = field_name,
          Relational.relationSchemaColumns = field_columns,
          Relational.relationSchemaPrimaryKeys = field_primaryKeys,
          Relational.relationSchemaForeignKeys = field_foreignKeys}))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

relationship :: Ord t0 => ((Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.Relationship t0))
relationship v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.Relationship b) (Helpers.decodeSet (Helpers.decodeMap columnName v) cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

row :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Relational.Row t0)
row v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Relational.Row b) (Helpers.decodeList v cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)
