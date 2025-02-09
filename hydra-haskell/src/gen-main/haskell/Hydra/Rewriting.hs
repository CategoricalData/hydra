-- | Utilities for type and term rewriting and analysis.

module Hydra.Rewriting where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Strip as Strip
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Fold over a term, traversing its subterms in the specified order
foldOverTerm :: (Coders.TraversalOrder -> (x -> Core.Term -> x) -> x -> Core.Term -> x)
foldOverTerm order fld b0 term = ((\x -> case x of
  Coders.TraversalOrderPre -> (Lists.foldl (foldOverTerm order fld) (fld b0 term) (subterms term))
  Coders.TraversalOrderPost -> (fld (Lists.foldl (foldOverTerm order fld) b0 (subterms term)) term)) order)

-- | Fold over a type, traversing its subtypes in the specified order
foldOverType :: (Coders.TraversalOrder -> (x -> Core.Type -> x) -> x -> Core.Type -> x)
foldOverType order fld b0 typ = ((\x -> case x of
  Coders.TraversalOrderPre -> (Lists.foldl (foldOverType order fld) (fld b0 typ) (subtypes typ))
  Coders.TraversalOrderPost -> (fld (Lists.foldl (foldOverType order fld) b0 (subtypes typ)) typ)) order)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a term
freeVariablesInTerm :: (Core.Term -> Set Core.Name)
freeVariablesInTerm term =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInTerm t)) Sets.empty (subterms term))
  in ((\x -> case x of
    Core.TermFunction v1 -> ((\x -> case x of
      Core.FunctionLambda v2 -> (Sets.remove (Core.lambdaParameter v2) (freeVariablesInTerm (Core.lambdaBody v2)))
      _ -> dfltVars) v1)
    Core.TermVariable v1 -> (Sets.singleton v1)
    _ -> dfltVars) term)

-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: (Core.Type -> Set Core.Name)
freeVariablesInType typ =  
  let dfltVars = (Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (subtypes typ))
  in ((\x -> case x of
    Core.TypeLambda v1 -> (Sets.remove (Core.lambdaTypeParameter v1) (freeVariablesInType (Core.lambdaTypeBody v1)))
    Core.TypeVariable v1 -> (Sets.singleton v1)
    _ -> dfltVars) typ)

-- | Check whether a term is a lambda, possibly nested within let and/or annotation terms
isLambda :: (Core.Term -> Bool)
isLambda term = ((\x -> case x of
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda _ -> True
    _ -> False) v1)
  Core.TermLet v1 -> (isLambda (Core.letEnvironment v1))
  _ -> False) (Strip.fullyStripTerm term))

-- | Find the children of a given term
subterms :: (Core.Term -> [Core.Term])
subterms x = case x of
  Core.TermAnnotated v1 -> [
    Core.annotatedTermSubject v1]
  Core.TermApplication v1 -> [
    Core.applicationFunction v1,
    (Core.applicationArgument v1)]
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationList v3 -> [
        v3]
      Core.EliminationOptional v3 -> [
        Core.optionalCasesNothing v3,
        (Core.optionalCasesJust v3)]
      Core.EliminationUnion v3 -> (Lists.concat2 ((\x -> case x of
        Nothing -> []
        Just v4 -> [
          v4]) (Core.caseStatementDefault v3)) (Lists.map Core.fieldTerm (Core.caseStatementCases v3)))
      _ -> []) v2)
    Core.FunctionLambda v2 -> [
      Core.lambdaBody v2]
    _ -> []) v1)
  Core.TermLet v1 -> (Lists.cons (Core.letEnvironment v1) (Lists.map Core.letBindingTerm (Core.letBindings v1)))
  Core.TermList v1 -> v1
  Core.TermLiteral _ -> []
  Core.TermMap v1 -> (Lists.concat (Lists.map (\p -> [
    fst p,
    (snd p)]) (Maps.toList v1)))
  Core.TermOptional v1 -> ((\x -> case x of
    Nothing -> []
    Just v2 -> [
      v2]) v1)
  Core.TermProduct v1 -> v1
  Core.TermRecord v1 -> (Lists.map Core.fieldTerm (Core.recordFields v1))
  Core.TermSet v1 -> (Sets.toList v1)
  Core.TermSum v1 -> [
    Core.sumTerm v1]
  Core.TermTypeAbstraction v1 -> [
    Core.typeAbstractionBody v1]
  Core.TermTypeApplication v1 -> [
    Core.typedTermTerm v1]
  Core.TermTyped v1 -> [
    Core.typedTermTerm v1]
  Core.TermUnion v1 -> [
    Core.fieldTerm (Core.injectionField v1)]
  Core.TermVariable _ -> []
  Core.TermWrap v1 -> [
    Core.wrappedTermObject v1]

-- | Find the children of a given term
subtermsWithAccessors :: (Core.Term -> [(Mantle.TermAccessor, Core.Term)])
subtermsWithAccessors x = case x of
  Core.TermAnnotated v1 -> [
    (Mantle.TermAccessorAnnotatedSubject, (Core.annotatedTermSubject v1))]
  Core.TermApplication v1 -> [
    (Mantle.TermAccessorApplicationFunction, (Core.applicationFunction v1)),
    (Mantle.TermAccessorApplicationArgument, (Core.applicationArgument v1))]
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationList v3 -> [
        (Mantle.TermAccessorListFold, v3)]
      Core.EliminationOptional v3 -> [
        (Mantle.TermAccessorOptionalCasesNothing, (Core.optionalCasesNothing v3)),
        (Mantle.TermAccessorOptionalCasesJust, (Core.optionalCasesJust v3))]
      Core.EliminationUnion v3 -> (Lists.concat2 ((\x -> case x of
        Nothing -> []
        Just v4 -> [
          (Mantle.TermAccessorUnionCasesDefault, v4)]) (Core.caseStatementDefault v3)) (Lists.map (\f -> (Mantle.TermAccessorUnionCasesBranch (Core.fieldName f), (Core.fieldTerm f))) (Core.caseStatementCases v3)))
      _ -> []) v2)
    Core.FunctionLambda v2 -> [
      (Mantle.TermAccessorLambdaBody, (Core.lambdaBody v2))]
    _ -> []) v1)
  Core.TermLet v1 -> (Lists.cons (Mantle.TermAccessorLetEnvironment, (Core.letEnvironment v1)) (Lists.map (\b -> (Mantle.TermAccessorLetBinding (Core.letBindingName b), (Core.letBindingTerm b))) (Core.letBindings v1)))
  Core.TermList v1 -> (Lists.map (\e -> (Mantle.TermAccessorListElement 0, e)) v1)
  Core.TermLiteral _ -> []
  Core.TermMap v1 -> (Lists.concat (Lists.map (\p -> [
    (Mantle.TermAccessorMapKey 0, (fst p)),
    (Mantle.TermAccessorMapValue 0, (snd p))]) (Maps.toList v1)))
  Core.TermOptional v1 -> ((\x -> case x of
    Nothing -> []
    Just v2 -> [
      (Mantle.TermAccessorOptionalTerm, v2)]) v1)
  Core.TermProduct v1 -> (Lists.map (\e -> (Mantle.TermAccessorProductTerm 0, e)) v1)
  Core.TermRecord v1 -> (Lists.map (\f -> (Mantle.TermAccessorRecordField (Core.fieldName f), (Core.fieldTerm f))) (Core.recordFields v1))
  Core.TermSet v1 -> (Lists.map (\e -> (Mantle.TermAccessorListElement 0, e)) (Sets.toList v1))
  Core.TermSum v1 -> [
    (Mantle.TermAccessorSumTerm, (Core.sumTerm v1))]
  Core.TermTypeAbstraction v1 -> [
    (Mantle.TermAccessorTypeAbstractionBody, (Core.typeAbstractionBody v1))]
  Core.TermTypeApplication v1 -> [
    (Mantle.TermAccessorTypeApplicationTerm, (Core.typedTermTerm v1))]
  Core.TermTyped v1 -> [
    (Mantle.TermAccessorTypedTerm, (Core.typedTermTerm v1))]
  Core.TermUnion v1 -> [
    (Mantle.TermAccessorInjectionTerm, (Core.fieldTerm (Core.injectionField v1)))]
  Core.TermVariable _ -> []
  Core.TermWrap v1 -> [
    (Mantle.TermAccessorWrappedTerm, (Core.wrappedTermObject v1))]

-- | Find the children of a given type expression
subtypes :: (Core.Type -> [Core.Type])
subtypes x = case x of
  Core.TypeAnnotated v1 -> [
    Core.annotatedTypeSubject v1]
  Core.TypeApplication v1 -> [
    Core.applicationTypeFunction v1,
    (Core.applicationTypeArgument v1)]
  Core.TypeFunction v1 -> [
    Core.functionTypeDomain v1,
    (Core.functionTypeCodomain v1)]
  Core.TypeLambda v1 -> [
    Core.lambdaTypeBody v1]
  Core.TypeList v1 -> [
    v1]
  Core.TypeLiteral _ -> []
  Core.TypeMap v1 -> [
    Core.mapTypeKeys v1,
    (Core.mapTypeValues v1)]
  Core.TypeOptional v1 -> [
    v1]
  Core.TypeProduct v1 -> v1
  Core.TypeRecord v1 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v1))
  Core.TypeSet v1 -> [
    v1]
  Core.TypeSum v1 -> v1
  Core.TypeUnion v1 -> (Lists.map Core.fieldTypeType (Core.rowTypeFields v1))
  Core.TypeVariable _ -> []
  Core.TypeWrap v1 -> [
    Core.wrappedTypeObject v1]
