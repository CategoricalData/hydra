-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.typing

module Hydra.Encode.Typing where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Context as Context
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

functionStructure :: (t0 -> Core.Term) -> Typing.FunctionStructure t0 -> Core.Term
functionStructure env x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map EncodeCore.name xs)) (Typing.functionStructureTypeParams x))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map EncodeCore.name xs)) (Typing.functionStructureParams x))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map EncodeCore.binding xs)) (Typing.functionStructureBindings x))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (EncodeCore.term (Typing.functionStructureBody x))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map EncodeCore.type_ xs)) (Typing.functionStructureDomains x))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map EncodeCore.type_ opt)) (Typing.functionStructureCodomain x))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (env (Typing.functionStructureEnvironment x))}]})

inferenceResult :: Typing.InferenceResult -> Core.Term
inferenceResult x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (EncodeCore.term (Typing.inferenceResultTerm x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (EncodeCore.type_ (Typing.inferenceResultType x))},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (typeSubst (Typing.inferenceResultSubst x))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap EncodeCore.name EncodeCore.typeVariableMetadata m)) (Typing.inferenceResultClassConstraints x))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Context.context (Typing.inferenceResultContext x))}]})

termSubst :: Typing.TermSubst -> Core.Term
termSubst x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typing.TermSubst"),
      Core.wrappedTermBody = ((\m -> Core.TermMap (Maps.bimap EncodeCore.name EncodeCore.term m)) (Typing.unTermSubst x))})

typeConstraint :: Typing.TypeConstraint -> Core.Term
typeConstraint x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (EncodeCore.type_ (Typing.typeConstraintLeft x))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (EncodeCore.type_ (Typing.typeConstraintRight x))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Typing.typeConstraintComment x))}]})

typeSubst :: Typing.TypeSubst -> Core.Term
typeSubst x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typing.TypeSubst"),
      Core.wrappedTermBody = ((\m -> Core.TermMap (Maps.bimap EncodeCore.name EncodeCore.type_ m)) (Typing.unTypeSubst x))})
