-- | Variable substitution in type and term expressions.

module Hydra.Substitution where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Compose two type substitutions
composeTypeSubst :: (Typing.TypeSubst -> Typing.TypeSubst -> Typing.TypeSubst)
composeTypeSubst s1 s2 =  
  let isExtra = (\k -> \v -> Maybes.isNothing (Maps.lookup k (Typing.unTypeSubst s1))) 
      withExtra = (Maps.filterWithKey isExtra (Typing.unTypeSubst s2))
  in (Typing.TypeSubst (Maps.union withExtra (Maps.map (substInType s2) (Typing.unTypeSubst s1))))

-- | Compose a list of type substitutions
composeTypeSubstList :: ([Typing.TypeSubst] -> Typing.TypeSubst)
composeTypeSubstList = (Lists.foldl composeTypeSubst idTypeSubst)

-- | The identity type substitution
idTypeSubst :: Typing.TypeSubst
idTypeSubst = (Typing.TypeSubst Maps.empty)

-- | Create a type substitution with a single variable mapping
singletonTypeSubst :: (Core.Name -> Core.Type -> Typing.TypeSubst)
singletonTypeSubst v t = (Typing.TypeSubst (Maps.singleton v t))

-- | Apply a type substitution to a type constraint
substituteInConstraint :: (Typing.TypeSubst -> Typing.TypeConstraint -> Typing.TypeConstraint)
substituteInConstraint subst c = Typing.TypeConstraint {
  Typing.typeConstraintLeft = (substInType subst (Typing.typeConstraintLeft c)),
  Typing.typeConstraintRight = (substInType subst (Typing.typeConstraintRight c)),
  Typing.typeConstraintComment = (Typing.typeConstraintComment c)}

-- | Apply a type substitution to a list of type constraints
substituteInConstraints :: (Typing.TypeSubst -> [Typing.TypeConstraint] -> [Typing.TypeConstraint])
substituteInConstraints subst cs = (Lists.map (substituteInConstraint subst) cs)

-- | Apply a type substitution to an inference context
substInContext :: (Typing.TypeSubst -> Typing.InferenceContext -> Typing.InferenceContext)
substInContext subst cx = Typing.InferenceContext {
  Typing.inferenceContextSchemaTypes = (Typing.inferenceContextSchemaTypes cx),
  Typing.inferenceContextPrimitiveTypes = (Typing.inferenceContextPrimitiveTypes cx),
  Typing.inferenceContextDataTypes = (Maps.map (substInTypeScheme subst) (Typing.inferenceContextDataTypes cx)),
  Typing.inferenceContextDebug = (Typing.inferenceContextDebug cx)}

-- | Apply a term substitution to a term
substituteInTerm :: (Typing.TermSubst -> Core.Term -> Core.Term)
substituteInTerm subst term0 =  
  let s = (Typing.unTermSubst subst) 
      rewrite = (\recurse -> \term ->  
              let withLambda = (\l ->  
                      let v = (Core.lambdaParameter l) 
                          subst2 = (Typing.TermSubst (Maps.remove v s))
                      in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = v,
                        Core.lambdaDomain = (Core.lambdaDomain l),
                        Core.lambdaBody = (substituteInTerm subst2 (Core.lambdaBody l))})))) 
                  withLet = (\lt ->  
                          let bindings = (Core.letBindings lt) 
                              names = (Sets.fromList (Lists.map Core.bindingName bindings))
                              subst2 = (Typing.TermSubst (Maps.filterWithKey (\k -> \v -> Logic.not (Sets.member k names)) s))
                              rewriteBinding = (\b -> Core.Binding {
                                      Core.bindingName = (Core.bindingName b),
                                      Core.bindingTerm = (substituteInTerm subst2 (Core.bindingTerm b)),
                                      Core.bindingType = (Core.bindingType b)})
                          in (Core.TermLet (Core.Let {
                            Core.letBindings = (Lists.map rewriteBinding bindings),
                            Core.letBody = (substituteInTerm subst2 (Core.letBody lt))})))
              in ((\x -> case x of
                Core.TermFunction v1 -> ((\x -> case x of
                  Core.FunctionLambda v2 -> (withLambda v2)
                  _ -> (recurse term)) v1)
                Core.TermLet v1 -> (withLet v1)
                Core.TermVariable v1 -> (Maybes.maybe (recurse term) (\sterm -> sterm) (Maps.lookup v1 s))
                _ -> (recurse term)) term))
  in (Rewriting.rewriteTerm rewrite term0)

-- | Apply a type substitution to a type
substInType :: (Typing.TypeSubst -> Core.Type -> Core.Type)
substInType subst typ0 =  
  let rewrite = (\recurse -> \typ -> (\x -> case x of
          Core.TypeForall v1 -> (Maybes.maybe (recurse typ) (\styp -> Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.forallTypeParameter v1),
            Core.forallTypeBody = (substInType (removeVar (Core.forallTypeParameter v1)) (Core.forallTypeBody v1))})) (Maps.lookup (Core.forallTypeParameter v1) (Typing.unTypeSubst subst)))
          Core.TypeVariable v1 -> (Maybes.maybe typ (\styp -> styp) (Maps.lookup v1 (Typing.unTypeSubst subst)))
          _ -> (recurse typ)) typ) 
      removeVar = (\v -> Typing.TypeSubst (Maps.remove v (Typing.unTypeSubst subst)))
  in (Rewriting.rewriteType rewrite typ0)

-- | Apply a type substitution to a type scheme
substInTypeScheme :: (Typing.TypeSubst -> Core.TypeScheme -> Core.TypeScheme)
substInTypeScheme subst ts = Core.TypeScheme {
  Core.typeSchemeVariables = (Core.typeSchemeVariables ts),
  Core.typeSchemeType = (substInType subst (Core.typeSchemeType ts))}

-- | Apply a type substitution to the type annotations within a term
substTypesInTerm :: (Typing.TypeSubst -> Core.Term -> Core.Term)
substTypesInTerm subst term0 =  
  let rewrite = (\recurse -> \term ->  
          let dflt = (recurse term) 
              forElimination = (\elm -> (\x -> case x of
                      Core.EliminationProduct v1 -> (forTupleProjection v1)
                      _ -> dflt) elm)
              forFunction = (\f -> (\x -> case x of
                      Core.FunctionElimination v1 -> (forElimination v1)
                      Core.FunctionLambda v1 -> (forLambda v1)
                      _ -> dflt) f)
              forLambda = (\l -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.lambdaParameter l),
                      Core.lambdaDomain = (Maybes.map (substInType subst) (Core.lambdaDomain l)),
                      Core.lambdaBody = (substTypesInTerm subst (Core.lambdaBody l))})))
              forLet = (\l ->  
                      let rewriteBinding = (\b -> Core.Binding {
                              Core.bindingName = (Core.bindingName b),
                              Core.bindingTerm = (substTypesInTerm subst (Core.bindingTerm b)),
                              Core.bindingType = (Maybes.map (substInTypeScheme subst) (Core.bindingType b))})
                      in (Core.TermLet (Core.Let {
                        Core.letBindings = (Lists.map rewriteBinding (Core.letBindings l)),
                        Core.letBody = (substTypesInTerm subst (Core.letBody l))})))
              forTupleProjection = (\tp -> Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                      Core.tupleProjectionArity = (Core.tupleProjectionArity tp),
                      Core.tupleProjectionIndex = (Core.tupleProjectionIndex tp),
                      Core.tupleProjectionDomain = (Maybes.map (\types -> Lists.map (substInType subst) types) (Core.tupleProjectionDomain tp))}))))
              forTypeApplication = (\tt -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (substTypesInTerm subst (Core.typeApplicationTermBody tt)),
                      Core.typeApplicationTermType = (substInType subst (Core.typeApplicationTermType tt))}))
              forTypeLambda = (\ta ->  
                      let param = (Core.typeLambdaParameter ta) 
                          subst2 = (Typing.TypeSubst (Maps.remove param (Typing.unTypeSubst subst)))
                      in (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = param,
                        Core.typeLambdaBody = (substTypesInTerm subst2 (Core.typeLambdaBody ta))})))
          in ((\x -> case x of
            Core.TermFunction v1 -> (forFunction v1)
            Core.TermLet v1 -> (forLet v1)
            Core.TermTypeApplication v1 -> (forTypeApplication v1)
            Core.TermTypeLambda v1 -> (forTypeLambda v1)
            _ -> dflt) term))
  in (Rewriting.rewriteTerm rewrite term0)
