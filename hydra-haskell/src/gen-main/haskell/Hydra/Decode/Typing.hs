-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.typing

module Hydra.Decode.Typing where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

inferenceContext :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.InferenceContext)
inferenceContext cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_schemaTypes -> Eithers.either (\err -> Left err) (\field_primitiveTypes -> Eithers.either (\err -> Left err) (\field_dataTypes -> Eithers.either (\err -> Left err) (\field_classConstraints -> Eithers.either (\err -> Left err) (\field_debug -> Right (Typing.InferenceContext {
      Typing.inferenceContextSchemaTypes = field_schemaTypes,
      Typing.inferenceContextPrimitiveTypes = field_primitiveTypes,
      Typing.inferenceContextDataTypes = field_dataTypes,
      Typing.inferenceContextClassConstraints = field_classConstraints,
      Typing.inferenceContextDebug = field_debug})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "debug",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralBoolean v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected boolean literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "debug") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "classConstraints",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.typeVariableMetadata cx rawVal)) (Core_.name cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "classConstraints") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "dataTypes",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.typeScheme cx rawVal)) (Core_.name cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "dataTypes") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "primitiveTypes",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.typeScheme cx rawVal)) (Core_.name cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "primitiveTypes") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "schemaTypes",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.typeScheme cx rawVal)) (Core_.name cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "schemaTypes") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.typing.InferenceContext"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

inferenceResult :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.InferenceResult)
inferenceResult cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_term -> Eithers.either (\err -> Left err) (\field_type -> Eithers.either (\err -> Left err) (\field_subst -> Eithers.either (\err -> Left err) (\field_classConstraints -> Right (Typing.InferenceResult {
      Typing.inferenceResultTerm = field_term,
      Typing.inferenceResultType = field_type,
      Typing.inferenceResultSubst = field_subst,
      Typing.inferenceResultClassConstraints = field_classConstraints})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "classConstraints",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.typeVariableMetadata cx rawVal)) (Core_.name cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "classConstraints") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "subst",
      " in record"]))) (\fieldTerm -> typeSubst cx fieldTerm) (Maps.lookup (Core.Name "subst") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "type",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "type") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "term",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "term") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.typing.InferenceResult"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

termSubst :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.TermSubst)
termSubst cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Typing.TermSubst b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermMap v2 ->  
      let pairs = (Maps.toList v2) 
          decodePair = (\kv ->  
                  let rawKey = (Pairs.first kv) 
                      rawVal = (Pairs.second kv)
                  in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.term cx rawVal)) (Core_.name cx rawKey)))
      in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
    _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.typing.TermSubst"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeConstraint :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.TypeConstraint)
typeConstraint cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_left -> Eithers.either (\err -> Left err) (\field_right -> Eithers.either (\err -> Left err) (\field_comment -> Right (Typing.TypeConstraint {
      Typing.typeConstraintLeft = field_left,
      Typing.typeConstraintRight = field_right,
      Typing.typeConstraintComment = field_comment})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "comment",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "comment") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "right",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "right") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "left",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "left") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.typing.TypeConstraint"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeContext :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.TypeContext)
typeContext cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_types -> Eithers.either (\err -> Left err) (\field_metadata -> Eithers.either (\err -> Left err) (\field_typeVariables -> Eithers.either (\err -> Left err) (\field_lambdaVariables -> Eithers.either (\err -> Left err) (\field_inferenceContext -> Right (Typing.TypeContext {
      Typing.typeContextTypes = field_types,
      Typing.typeContextMetadata = field_metadata,
      Typing.typeContextTypeVariables = field_typeVariables,
      Typing.typeContextLambdaVariables = field_lambdaVariables,
      Typing.typeContextInferenceContext = field_inferenceContext})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "inferenceContext",
      " in record"]))) (\fieldTerm -> inferenceContext cx fieldTerm) (Maps.lookup (Core.Name "inferenceContext") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "lambdaVariables",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermSet v2 ->  
        let elements = (Sets.toList v2)
        in (Eithers.either (\err -> Left err) (\decodedElems -> Right (Sets.fromList decodedElems)) (Eithers.mapList (Core_.name cx) elements))
      _ -> (Left (Util.DecodingError "expected set"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "lambdaVariables") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeVariables",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermSet v2 ->  
        let elements = (Sets.toList v2)
        in (Eithers.either (\err -> Left err) (\decodedElems -> Right (Sets.fromList decodedElems)) (Eithers.mapList (Core_.name cx) elements))
      _ -> (Left (Util.DecodingError "expected set"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "typeVariables") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "metadata",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.term cx rawVal)) (Core_.name cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "metadata") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "types",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.type_ cx rawVal)) (Core_.name cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "types") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.typing.TypeContext"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeSubst :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Typing.TypeSubst)
typeSubst cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Typing.TypeSubst b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermMap v2 ->  
      let pairs = (Maps.toList v2) 
          decodePair = (\kv ->  
                  let rawKey = (Pairs.first kv) 
                      rawVal = (Pairs.second kv)
                  in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (Core_.type_ cx rawVal)) (Core_.name cx rawKey)))
      in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
    _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.typing.TypeSubst"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
