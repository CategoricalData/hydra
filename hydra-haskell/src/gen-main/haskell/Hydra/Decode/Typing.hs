-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.typing

module Hydra.Decode.Typing where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Context as Context
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

functionStructure :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Typing.FunctionStructure t0)
functionStructure env cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "typeParams" (Helpers.decodeList Core_.name) fieldMap cx) (\field_typeParams -> Eithers.bind (Helpers.requireField "params" (Helpers.decodeList Core_.name) fieldMap cx) (\field_params -> Eithers.bind (Helpers.requireField "bindings" (Helpers.decodeList Core_.binding) fieldMap cx) (\field_bindings -> Eithers.bind (Helpers.requireField "body" Core_.term fieldMap cx) (\field_body -> Eithers.bind (Helpers.requireField "domains" (Helpers.decodeList Core_.type_) fieldMap cx) (\field_domains -> Eithers.bind (Helpers.requireField "codomain" (Helpers.decodeMaybe Core_.type_) fieldMap cx) (\field_codomain -> Eithers.bind (Helpers.requireField "environment" env fieldMap cx) (\field_environment -> Right (Typing.FunctionStructure {
          Typing.functionStructureTypeParams = field_typeParams,
          Typing.functionStructureParams = field_params,
          Typing.functionStructureBindings = field_bindings,
          Typing.functionStructureBody = field_body,
          Typing.functionStructureDomains = field_domains,
          Typing.functionStructureCodomain = field_codomain,
          Typing.functionStructureEnvironment = field_environment})))))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

inferenceResult :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.InferenceResult
inferenceResult cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "term" Core_.term fieldMap cx) (\field_term -> Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Eithers.bind (Helpers.requireField "subst" typeSubst fieldMap cx) (\field_subst -> Eithers.bind (Helpers.requireField "classConstraints" (Helpers.decodeMap Core_.name Core_.typeVariableMetadata) fieldMap cx) (\field_classConstraints -> Eithers.bind (Helpers.requireField "context" Context.context fieldMap cx) (\field_context -> Right (Typing.InferenceResult {
          Typing.inferenceResultTerm = field_term,
          Typing.inferenceResultType = field_type,
          Typing.inferenceResultSubst = field_subst,
          Typing.inferenceResultClassConstraints = field_classConstraints,
          Typing.inferenceResultContext = field_context})))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

termSubst :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.TermSubst
termSubst cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Typing.TermSubst b) (Helpers.decodeMap Core_.name Core_.term cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

typeConstraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.TypeConstraint
typeConstraint cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "left" Core_.type_ fieldMap cx) (\field_left -> Eithers.bind (Helpers.requireField "right" Core_.type_ fieldMap cx) (\field_right -> Eithers.bind (Helpers.requireField "comment" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_comment -> Right (Typing.TypeConstraint {
          Typing.typeConstraintLeft = field_left,
          Typing.typeConstraintRight = field_right,
          Typing.typeConstraintComment = field_comment})))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

typeSubst :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.TypeSubst
typeSubst cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Typing.TypeSubst b) (Helpers.decodeMap Core_.name Core_.type_ cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)
