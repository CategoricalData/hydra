-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.typing

module Hydra.Decode.Typing where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Context as Context
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core__
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
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "typeParams" (Core__.decodeList Core_.name) fieldMap cx) (\field_typeParams -> Eithers.bind (Core__.requireField "params" (Core__.decodeList Core_.name) fieldMap cx) (\field_params -> Eithers.bind (Core__.requireField "bindings" (Core__.decodeList Core_.binding) fieldMap cx) (\field_bindings -> Eithers.bind (Core__.requireField "body" Core_.term fieldMap cx) (\field_body -> Eithers.bind (Core__.requireField "domains" (Core__.decodeList Core_.type_) fieldMap cx) (\field_domains -> Eithers.bind (Core__.requireField "codomain" (Core__.decodeMaybe Core_.type_) fieldMap cx) (\field_codomain -> Eithers.bind (Core__.requireField "environment" env fieldMap cx) (\field_environment -> Right (Typing.FunctionStructure {
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
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "term" Core_.term fieldMap cx) (\field_term -> Eithers.bind (Core__.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Eithers.bind (Core__.requireField "subst" typeSubst fieldMap cx) (\field_subst -> Eithers.bind (Core__.requireField "classConstraints" (Core__.decodeMap Core_.name Core_.typeVariableMetadata) fieldMap cx) (\field_classConstraints -> Eithers.bind (Core__.requireField "context" Context.context fieldMap cx) (\field_context -> Right (Typing.InferenceResult {
          Typing.inferenceResultTerm = field_term,
          Typing.inferenceResultType = field_type,
          Typing.inferenceResultSubst = field_subst,
          Typing.inferenceResultClassConstraints = field_classConstraints,
          Typing.inferenceResultContext = field_context})))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

termSubst :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.TermSubst
termSubst cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Typing.TermSubst b) (Core__.decodeMap Core_.name Core_.term cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

typeConstraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.TypeConstraint
typeConstraint cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core__.toFieldMap v0
        in (Eithers.bind (Core__.requireField "left" Core_.type_ fieldMap cx) (\field_left -> Eithers.bind (Core__.requireField "right" Core_.type_ fieldMap cx) (\field_right -> Eithers.bind (Core__.requireField "comment" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
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
      Core.TermWrap v0 -> Eithers.map (\b -> Typing.TypeSubst b) (Core__.decodeMap Core_.name Core_.type_ cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)
