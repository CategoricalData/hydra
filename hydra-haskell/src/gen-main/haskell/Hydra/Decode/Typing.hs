-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.typing

module Hydra.Decode.Typing where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

inferenceContext :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.InferenceContext)
inferenceContext cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "schemaTypes" (Helpers.decodeMap Core_.name Core_.typeScheme) fieldMap cx) (\field_schemaTypes -> Eithers.bind (Helpers.requireField "primitiveTypes" (Helpers.decodeMap Core_.name Core_.typeScheme) fieldMap cx) (\field_primitiveTypes -> Eithers.bind (Helpers.requireField "dataTypes" (Helpers.decodeMap Core_.name Core_.typeScheme) fieldMap cx) (\field_dataTypes -> Eithers.bind (Helpers.requireField "classConstraints" (Helpers.decodeMap Core_.name Core_.typeVariableMetadata) fieldMap cx) (\field_classConstraints -> Eithers.bind (Helpers.requireField "debug" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralBoolean v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected boolean literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_debug -> Right (Typing.InferenceContext {
      Typing.inferenceContextSchemaTypes = field_schemaTypes,
      Typing.inferenceContextPrimitiveTypes = field_primitiveTypes,
      Typing.inferenceContextDataTypes = field_dataTypes,
      Typing.inferenceContextClassConstraints = field_classConstraints,
      Typing.inferenceContextDebug = field_debug})))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.typing.InferenceContext"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

inferenceResult :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.InferenceResult)
inferenceResult cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "term" Core_.term fieldMap cx) (\field_term -> Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Eithers.bind (Helpers.requireField "subst" typeSubst fieldMap cx) (\field_subst -> Eithers.bind (Helpers.requireField "classConstraints" (Helpers.decodeMap Core_.name Core_.typeVariableMetadata) fieldMap cx) (\field_classConstraints -> Right (Typing.InferenceResult {
      Typing.inferenceResultTerm = field_term,
      Typing.inferenceResultType = field_type,
      Typing.inferenceResultSubst = field_subst,
      Typing.inferenceResultClassConstraints = field_classConstraints}))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.typing.InferenceResult"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

termSubst :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.TermSubst)
termSubst cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Typing.TermSubst b) (Helpers.decodeMap Core_.name Core_.term cx (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.typing.TermSubst"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeConstraint :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.TypeConstraint)
typeConstraint cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "left" Core_.type_ fieldMap cx) (\field_left -> Eithers.bind (Helpers.requireField "right" Core_.type_ fieldMap cx) (\field_right -> Eithers.bind (Helpers.requireField "comment" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_comment -> Right (Typing.TypeConstraint {
      Typing.typeConstraintLeft = field_left,
      Typing.typeConstraintRight = field_right,
      Typing.typeConstraintComment = field_comment})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.typing.TypeConstraint"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeContext :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.TypeContext)
typeContext cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "types" (Helpers.decodeMap Core_.name Core_.type_) fieldMap cx) (\field_types -> Eithers.bind (Helpers.requireField "metadata" (Helpers.decodeMap Core_.name Core_.term) fieldMap cx) (\field_metadata -> Eithers.bind (Helpers.requireField "typeVariables" (Helpers.decodeSet Core_.name) fieldMap cx) (\field_typeVariables -> Eithers.bind (Helpers.requireField "lambdaVariables" (Helpers.decodeSet Core_.name) fieldMap cx) (\field_lambdaVariables -> Eithers.bind (Helpers.requireField "inferenceContext" inferenceContext fieldMap cx) (\field_inferenceContext -> Right (Typing.TypeContext {
      Typing.typeContextTypes = field_types,
      Typing.typeContextMetadata = field_metadata,
      Typing.typeContextTypeVariables = field_typeVariables,
      Typing.typeContextLambdaVariables = field_lambdaVariables,
      Typing.typeContextInferenceContext = field_inferenceContext})))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.typing.TypeContext"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeSubst :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.TypeSubst)
typeSubst cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Typing.TypeSubst b) (Helpers.decodeMap Core_.name Core_.type_ cx (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.typing.TypeSubst"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
