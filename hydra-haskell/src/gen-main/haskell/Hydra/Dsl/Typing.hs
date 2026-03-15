-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.typing

module Hydra.Dsl.Typing where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

functionStructure :: ([Core.Name] -> [Core.Name] -> [Core.Binding] -> Core.Term -> [Core.Type] -> Maybe Core.Type -> t0 -> Typing.FunctionStructure t0)
functionStructure typeParams params bindings body domains codomain environment = Typing.FunctionStructure {
  Typing.functionStructureTypeParams = typeParams,
  Typing.functionStructureParams = params,
  Typing.functionStructureBindings = bindings,
  Typing.functionStructureBody = body,
  Typing.functionStructureDomains = domains,
  Typing.functionStructureCodomain = codomain,
  Typing.functionStructureEnvironment = environment}

functionStructureTypeParams :: (Typing.FunctionStructure t0 -> [Core.Name])
functionStructureTypeParams = Typing.functionStructureTypeParams

functionStructureParams :: (Typing.FunctionStructure t0 -> [Core.Name])
functionStructureParams = Typing.functionStructureParams

functionStructureBindings :: (Typing.FunctionStructure t0 -> [Core.Binding])
functionStructureBindings = Typing.functionStructureBindings

functionStructureBody :: (Typing.FunctionStructure t0 -> Core.Term)
functionStructureBody = Typing.functionStructureBody

functionStructureDomains :: (Typing.FunctionStructure t0 -> [Core.Type])
functionStructureDomains = Typing.functionStructureDomains

functionStructureCodomain :: (Typing.FunctionStructure t0 -> Maybe Core.Type)
functionStructureCodomain = Typing.functionStructureCodomain

functionStructureEnvironment :: (Typing.FunctionStructure t0 -> t0)
functionStructureEnvironment = Typing.functionStructureEnvironment

functionStructureWithTypeParams :: (Typing.FunctionStructure t0 -> [Core.Name] -> Typing.FunctionStructure t0)
functionStructureWithTypeParams original newVal = Typing.FunctionStructure {
  Typing.functionStructureTypeParams = newVal,
  Typing.functionStructureParams = (Typing.functionStructureParams original),
  Typing.functionStructureBindings = (Typing.functionStructureBindings original),
  Typing.functionStructureBody = (Typing.functionStructureBody original),
  Typing.functionStructureDomains = (Typing.functionStructureDomains original),
  Typing.functionStructureCodomain = (Typing.functionStructureCodomain original),
  Typing.functionStructureEnvironment = (Typing.functionStructureEnvironment original)}

functionStructureWithParams :: (Typing.FunctionStructure t0 -> [Core.Name] -> Typing.FunctionStructure t0)
functionStructureWithParams original newVal = Typing.FunctionStructure {
  Typing.functionStructureTypeParams = (Typing.functionStructureTypeParams original),
  Typing.functionStructureParams = newVal,
  Typing.functionStructureBindings = (Typing.functionStructureBindings original),
  Typing.functionStructureBody = (Typing.functionStructureBody original),
  Typing.functionStructureDomains = (Typing.functionStructureDomains original),
  Typing.functionStructureCodomain = (Typing.functionStructureCodomain original),
  Typing.functionStructureEnvironment = (Typing.functionStructureEnvironment original)}

functionStructureWithBindings :: (Typing.FunctionStructure t0 -> [Core.Binding] -> Typing.FunctionStructure t0)
functionStructureWithBindings original newVal = Typing.FunctionStructure {
  Typing.functionStructureTypeParams = (Typing.functionStructureTypeParams original),
  Typing.functionStructureParams = (Typing.functionStructureParams original),
  Typing.functionStructureBindings = newVal,
  Typing.functionStructureBody = (Typing.functionStructureBody original),
  Typing.functionStructureDomains = (Typing.functionStructureDomains original),
  Typing.functionStructureCodomain = (Typing.functionStructureCodomain original),
  Typing.functionStructureEnvironment = (Typing.functionStructureEnvironment original)}

functionStructureWithBody :: (Typing.FunctionStructure t0 -> Core.Term -> Typing.FunctionStructure t0)
functionStructureWithBody original newVal = Typing.FunctionStructure {
  Typing.functionStructureTypeParams = (Typing.functionStructureTypeParams original),
  Typing.functionStructureParams = (Typing.functionStructureParams original),
  Typing.functionStructureBindings = (Typing.functionStructureBindings original),
  Typing.functionStructureBody = newVal,
  Typing.functionStructureDomains = (Typing.functionStructureDomains original),
  Typing.functionStructureCodomain = (Typing.functionStructureCodomain original),
  Typing.functionStructureEnvironment = (Typing.functionStructureEnvironment original)}

functionStructureWithDomains :: (Typing.FunctionStructure t0 -> [Core.Type] -> Typing.FunctionStructure t0)
functionStructureWithDomains original newVal = Typing.FunctionStructure {
  Typing.functionStructureTypeParams = (Typing.functionStructureTypeParams original),
  Typing.functionStructureParams = (Typing.functionStructureParams original),
  Typing.functionStructureBindings = (Typing.functionStructureBindings original),
  Typing.functionStructureBody = (Typing.functionStructureBody original),
  Typing.functionStructureDomains = newVal,
  Typing.functionStructureCodomain = (Typing.functionStructureCodomain original),
  Typing.functionStructureEnvironment = (Typing.functionStructureEnvironment original)}

functionStructureWithCodomain :: (Typing.FunctionStructure t0 -> Maybe Core.Type -> Typing.FunctionStructure t0)
functionStructureWithCodomain original newVal = Typing.FunctionStructure {
  Typing.functionStructureTypeParams = (Typing.functionStructureTypeParams original),
  Typing.functionStructureParams = (Typing.functionStructureParams original),
  Typing.functionStructureBindings = (Typing.functionStructureBindings original),
  Typing.functionStructureBody = (Typing.functionStructureBody original),
  Typing.functionStructureDomains = (Typing.functionStructureDomains original),
  Typing.functionStructureCodomain = newVal,
  Typing.functionStructureEnvironment = (Typing.functionStructureEnvironment original)}

functionStructureWithEnvironment :: (Typing.FunctionStructure t0 -> t1 -> Typing.FunctionStructure t1)
functionStructureWithEnvironment original newVal = Typing.FunctionStructure {
  Typing.functionStructureTypeParams = (Typing.functionStructureTypeParams original),
  Typing.functionStructureParams = (Typing.functionStructureParams original),
  Typing.functionStructureBindings = (Typing.functionStructureBindings original),
  Typing.functionStructureBody = (Typing.functionStructureBody original),
  Typing.functionStructureDomains = (Typing.functionStructureDomains original),
  Typing.functionStructureCodomain = (Typing.functionStructureCodomain original),
  Typing.functionStructureEnvironment = newVal}

inferenceResult :: (Core.Term -> Core.Type -> Typing.TypeSubst -> M.Map Core.Name Core.TypeVariableMetadata -> Context.Context -> Typing.InferenceResult)
inferenceResult term type_ subst classConstraints context = Typing.InferenceResult {
  Typing.inferenceResultTerm = term,
  Typing.inferenceResultType = type_,
  Typing.inferenceResultSubst = subst,
  Typing.inferenceResultClassConstraints = classConstraints,
  Typing.inferenceResultContext = context}

inferenceResultTerm :: (Typing.InferenceResult -> Core.Term)
inferenceResultTerm = Typing.inferenceResultTerm

inferenceResultType :: (Typing.InferenceResult -> Core.Type)
inferenceResultType = Typing.inferenceResultType

inferenceResultSubst :: (Typing.InferenceResult -> Typing.TypeSubst)
inferenceResultSubst = Typing.inferenceResultSubst

inferenceResultClassConstraints :: (Typing.InferenceResult -> M.Map Core.Name Core.TypeVariableMetadata)
inferenceResultClassConstraints = Typing.inferenceResultClassConstraints

inferenceResultContext :: (Typing.InferenceResult -> Context.Context)
inferenceResultContext = Typing.inferenceResultContext

inferenceResultWithTerm :: (Typing.InferenceResult -> Core.Term -> Typing.InferenceResult)
inferenceResultWithTerm original newVal = Typing.InferenceResult {
  Typing.inferenceResultTerm = newVal,
  Typing.inferenceResultType = (Typing.inferenceResultType original),
  Typing.inferenceResultSubst = (Typing.inferenceResultSubst original),
  Typing.inferenceResultClassConstraints = (Typing.inferenceResultClassConstraints original),
  Typing.inferenceResultContext = (Typing.inferenceResultContext original)}

inferenceResultWithType :: (Typing.InferenceResult -> Core.Type -> Typing.InferenceResult)
inferenceResultWithType original newVal = Typing.InferenceResult {
  Typing.inferenceResultTerm = (Typing.inferenceResultTerm original),
  Typing.inferenceResultType = newVal,
  Typing.inferenceResultSubst = (Typing.inferenceResultSubst original),
  Typing.inferenceResultClassConstraints = (Typing.inferenceResultClassConstraints original),
  Typing.inferenceResultContext = (Typing.inferenceResultContext original)}

inferenceResultWithSubst :: (Typing.InferenceResult -> Typing.TypeSubst -> Typing.InferenceResult)
inferenceResultWithSubst original newVal = Typing.InferenceResult {
  Typing.inferenceResultTerm = (Typing.inferenceResultTerm original),
  Typing.inferenceResultType = (Typing.inferenceResultType original),
  Typing.inferenceResultSubst = newVal,
  Typing.inferenceResultClassConstraints = (Typing.inferenceResultClassConstraints original),
  Typing.inferenceResultContext = (Typing.inferenceResultContext original)}

inferenceResultWithClassConstraints :: (Typing.InferenceResult -> M.Map Core.Name Core.TypeVariableMetadata -> Typing.InferenceResult)
inferenceResultWithClassConstraints original newVal = Typing.InferenceResult {
  Typing.inferenceResultTerm = (Typing.inferenceResultTerm original),
  Typing.inferenceResultType = (Typing.inferenceResultType original),
  Typing.inferenceResultSubst = (Typing.inferenceResultSubst original),
  Typing.inferenceResultClassConstraints = newVal,
  Typing.inferenceResultContext = (Typing.inferenceResultContext original)}

inferenceResultWithContext :: (Typing.InferenceResult -> Context.Context -> Typing.InferenceResult)
inferenceResultWithContext original newVal = Typing.InferenceResult {
  Typing.inferenceResultTerm = (Typing.inferenceResultTerm original),
  Typing.inferenceResultType = (Typing.inferenceResultType original),
  Typing.inferenceResultSubst = (Typing.inferenceResultSubst original),
  Typing.inferenceResultClassConstraints = (Typing.inferenceResultClassConstraints original),
  Typing.inferenceResultContext = newVal}

termSubst :: (M.Map Core.Name Core.Term -> Typing.TermSubst)
termSubst x = (Typing.TermSubst x)

unTermSubst :: (Typing.TermSubst -> M.Map Core.Name Core.Term)
unTermSubst = Typing.unTermSubst

typeConstraint :: (Core.Type -> Core.Type -> String -> Typing.TypeConstraint)
typeConstraint left right comment = Typing.TypeConstraint {
  Typing.typeConstraintLeft = left,
  Typing.typeConstraintRight = right,
  Typing.typeConstraintComment = comment}

typeConstraintLeft :: (Typing.TypeConstraint -> Core.Type)
typeConstraintLeft = Typing.typeConstraintLeft

typeConstraintRight :: (Typing.TypeConstraint -> Core.Type)
typeConstraintRight = Typing.typeConstraintRight

typeConstraintComment :: (Typing.TypeConstraint -> String)
typeConstraintComment = Typing.typeConstraintComment

typeConstraintWithLeft :: (Typing.TypeConstraint -> Core.Type -> Typing.TypeConstraint)
typeConstraintWithLeft original newVal = Typing.TypeConstraint {
  Typing.typeConstraintLeft = newVal,
  Typing.typeConstraintRight = (Typing.typeConstraintRight original),
  Typing.typeConstraintComment = (Typing.typeConstraintComment original)}

typeConstraintWithRight :: (Typing.TypeConstraint -> Core.Type -> Typing.TypeConstraint)
typeConstraintWithRight original newVal = Typing.TypeConstraint {
  Typing.typeConstraintLeft = (Typing.typeConstraintLeft original),
  Typing.typeConstraintRight = newVal,
  Typing.typeConstraintComment = (Typing.typeConstraintComment original)}

typeConstraintWithComment :: (Typing.TypeConstraint -> String -> Typing.TypeConstraint)
typeConstraintWithComment original newVal = Typing.TypeConstraint {
  Typing.typeConstraintLeft = (Typing.typeConstraintLeft original),
  Typing.typeConstraintRight = (Typing.typeConstraintRight original),
  Typing.typeConstraintComment = newVal}

typeSubst :: (M.Map Core.Name Core.Type -> Typing.TypeSubst)
typeSubst x = (Typing.TypeSubst x)

unTypeSubst :: (Typing.TypeSubst -> M.Map Core.Name Core.Type)
unTypeSubst = Typing.unTypeSubst
