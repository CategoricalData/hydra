-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.typing

module Hydra.Encode.Typing where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

functionStructure :: ((t0 -> Core.Term) -> Typing.FunctionStructure t0 -> Core.Term)
functionStructure env x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map Core_.name xs)) (Typing.functionStructureTypeParams x))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map Core_.name xs)) (Typing.functionStructureParams x))},
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map Core_.binding xs)) (Typing.functionStructureBindings x))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core_.term (Typing.functionStructureBody x))},
    Core.Field {
      Core.fieldName = (Core.Name "domains"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map Core_.type_ xs)) (Typing.functionStructureDomains x))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map Core_.type_ opt)) (Typing.functionStructureCodomain x))},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (env (Typing.functionStructureEnvironment x))}]}))

inferenceContext :: (Typing.InferenceContext -> Core.Term)
inferenceContext x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.InferenceContext"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.typeScheme m)) (Typing.inferenceContextSchemaTypes x))},
    Core.Field {
      Core.fieldName = (Core.Name "primitiveTypes"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.typeScheme m)) (Typing.inferenceContextPrimitiveTypes x))},
    Core.Field {
      Core.fieldName = (Core.Name "dataTypes"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.typeScheme m)) (Typing.inferenceContextDataTypes x))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.typeVariableMetadata m)) (Typing.inferenceContextClassConstraints x))},
    Core.Field {
      Core.fieldName = (Core.Name "debug"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralBoolean x)) (Typing.inferenceContextDebug x))}]}))

inferenceResult :: (Typing.InferenceResult -> Core.Term)
inferenceResult x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Core_.term (Typing.inferenceResultTerm x))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core_.type_ (Typing.inferenceResultType x))},
    Core.Field {
      Core.fieldName = (Core.Name "subst"),
      Core.fieldTerm = (typeSubst (Typing.inferenceResultSubst x))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.typeVariableMetadata m)) (Typing.inferenceResultClassConstraints x))}]}))

termSubst :: (Typing.TermSubst -> Core.Term)
termSubst x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.typing.TermSubst"),
  Core.wrappedTermBody = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.term m)) (Typing.unTermSubst x))}))

typeConstraint :: (Typing.TypeConstraint -> Core.Term)
typeConstraint x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Core_.type_ (Typing.typeConstraintLeft x))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Core_.type_ (Typing.typeConstraintRight x))},
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Typing.typeConstraintComment x))}]}))

typeContext :: (Typing.TypeContext -> Core.Term)
typeContext x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.TypeContext"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "types"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.type_ m)) (Typing.typeContextTypes x))},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.term m)) (Typing.typeContextMetadata x))},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = ((\s -> Core.TermSet (Sets.map Core_.name s)) (Typing.typeContextTypeVariables x))},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = ((\s -> Core.TermSet (Sets.map Core_.name s)) (Typing.typeContextLambdaVariables x))},
    Core.Field {
      Core.fieldName = (Core.Name "letVariables"),
      Core.fieldTerm = ((\s -> Core.TermSet (Sets.map Core_.name s)) (Typing.typeContextLetVariables x))},
    Core.Field {
      Core.fieldName = (Core.Name "inferenceContext"),
      Core.fieldTerm = (inferenceContext (Typing.typeContextInferenceContext x))}]}))

typeSubst :: (Typing.TypeSubst -> Core.Term)
typeSubst x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.typing.TypeSubst"),
  Core.wrappedTermBody = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.type_ m)) (Typing.unTypeSubst x))}))
