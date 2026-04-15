-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.typing

module Hydra.Decode.Typing where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Context as Context
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

functionStructure :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Typing.FunctionStructure t0)
functionStructure env cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "typeParams" (ExtractCore.decodeList DecodeCore.name) fieldMap cx) (\field_typeParams -> Eithers.bind (ExtractCore.requireField "params" (ExtractCore.decodeList DecodeCore.name) fieldMap cx) (\field_params -> Eithers.bind (ExtractCore.requireField "bindings" (ExtractCore.decodeList DecodeCore.binding) fieldMap cx) (\field_bindings -> Eithers.bind (ExtractCore.requireField "body" DecodeCore.term fieldMap cx) (\field_body -> Eithers.bind (ExtractCore.requireField "domains" (ExtractCore.decodeList DecodeCore.type_) fieldMap cx) (\field_domains -> Eithers.bind (ExtractCore.requireField "codomain" (ExtractCore.decodeMaybe DecodeCore.type_) fieldMap cx) (\field_codomain -> Eithers.bind (ExtractCore.requireField "environment" env fieldMap cx) (\field_environment -> Right (Typing.FunctionStructure {
          Typing.functionStructureTypeParams = field_typeParams,
          Typing.functionStructureParams = field_params,
          Typing.functionStructureBindings = field_bindings,
          Typing.functionStructureBody = field_body,
          Typing.functionStructureDomains = field_domains,
          Typing.functionStructureCodomain = field_codomain,
          Typing.functionStructureEnvironment = field_environment})))))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

inferenceResult :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.InferenceResult
inferenceResult cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "term" DecodeCore.term fieldMap cx) (\field_term -> Eithers.bind (ExtractCore.requireField "type" DecodeCore.type_ fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "subst" typeSubst fieldMap cx) (\field_subst -> Eithers.bind (ExtractCore.requireField "classConstraints" (ExtractCore.decodeMap DecodeCore.name DecodeCore.typeVariableMetadata) fieldMap cx) (\field_classConstraints -> Eithers.bind (ExtractCore.requireField "context" Context.context fieldMap cx) (\field_context -> Right (Typing.InferenceResult {
          Typing.inferenceResultTerm = field_term,
          Typing.inferenceResultType = field_type,
          Typing.inferenceResultSubst = field_subst,
          Typing.inferenceResultClassConstraints = field_classConstraints,
          Typing.inferenceResultContext = field_context})))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

termSubst :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.TermSubst
termSubst cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Typing.TermSubst b) (ExtractCore.decodeMap DecodeCore.name DecodeCore.term cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)

typeConstraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.TypeConstraint
typeConstraint cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "left" DecodeCore.type_ fieldMap cx) (\field_left -> Eithers.bind (ExtractCore.requireField "right" DecodeCore.type_ fieldMap cx) (\field_right -> Eithers.bind (ExtractCore.requireField "comment" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_comment -> Right (Typing.TypeConstraint {
          Typing.typeConstraintLeft = field_left,
          Typing.typeConstraintRight = field_right,
          Typing.typeConstraintComment = field_comment})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

typeSubst :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Typing.TypeSubst
typeSubst cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Typing.TypeSubst b) (ExtractCore.decodeMap DecodeCore.name DecodeCore.type_ cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
