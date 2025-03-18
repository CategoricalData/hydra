-- | Utilities for type and term substitution.

module Hydra.Substitution where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Typing as Typing
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

composeTypeSubst :: (Typing.TypeSubst -> Typing.TypeSubst -> Typing.TypeSubst)
composeTypeSubst s1 s2 =  
  let isExtra = (\k -> \v -> Optionals.isNothing (Maps.lookup k (Typing.unTypeSubst s1))) 
      withExtra = (Maps.filterWithKey isExtra (Typing.unTypeSubst s2))
  in (Typing.TypeSubst (Maps.union withExtra (Maps.map (substInType s2) (Typing.unTypeSubst s1))))

composeTypeSubstList :: ([Typing.TypeSubst] -> Typing.TypeSubst)
composeTypeSubstList = (Lists.foldl composeTypeSubst emptyTypeSubst)

emptyTypeSubst :: Typing.TypeSubst
emptyTypeSubst = (Typing.TypeSubst Maps.empty)

singletonTypeSubst :: (Core.Name -> Core.Type -> Typing.TypeSubst)
singletonTypeSubst v t = (Typing.TypeSubst (Maps.singleton v t))

substituteInConstraint :: (Typing.TypeSubst -> Typing.TypeConstraint -> Typing.TypeConstraint)
substituteInConstraint subst c = Typing.TypeConstraint {
  Typing.typeConstraintLeft = (substInType subst (Typing.typeConstraintLeft c)),
  Typing.typeConstraintRight = (substInType subst (Typing.typeConstraintRight c)),
  Typing.typeConstraintComment = (Typing.typeConstraintComment c)}

substituteInConstraints :: (Typing.TypeSubst -> [Typing.TypeConstraint] -> [Typing.TypeConstraint])
substituteInConstraints subst cs = (Lists.map (substituteInConstraint subst) cs)

substInContext :: (Typing.TypeSubst -> Typing.InferenceContext -> Typing.InferenceContext)
substInContext subst cx = Typing.InferenceContext {
  Typing.inferenceContextSchemaTypes = (Typing.inferenceContextSchemaTypes cx),
  Typing.inferenceContextPrimitiveTypes = (Typing.inferenceContextPrimitiveTypes cx),
  Typing.inferenceContextDataTypes = (Maps.map (substInTypeScheme subst) (Typing.inferenceContextDataTypes cx)),
  Typing.inferenceContextDebug = (Typing.inferenceContextDebug cx)}

substituteInTerm :: (Typing.TermSubst -> Core.Term -> Core.Term)
substituteInTerm subst =  
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
                              names = (Sets.fromList (Lists.map Core.letBindingName bindings))
                              subst2 = (Typing.TermSubst (Maps.filterWithKey (\k -> \v -> Logic.not (Sets.contains k names)) s))
                              rewriteBinding = (\b -> Core.LetBinding {
                                      Core.letBindingName = (Core.letBindingName b),
                                      Core.letBindingTerm = (substituteInTerm subst2 (Core.letBindingTerm b)),
                                      Core.letBindingType = (Core.letBindingType b)})
                          in (Core.TermLet (Core.Let {
                            Core.letBindings = (Lists.map rewriteBinding bindings),
                            Core.letEnvironment = (substituteInTerm subst2 (Core.letEnvironment lt))})))
              in ((\x -> case x of
                Core.TermFunction v1 -> ((\x -> case x of
                  Core.FunctionLambda v2 -> (withLambda v2)
                  _ -> (recurse term)) v1)
                Core.TermLet v1 -> (withLet v1)
                Core.TermVariable v1 -> ((\x -> case x of
                  Nothing -> (recurse term)
                  Just v2 -> v2) (Maps.lookup v1 s))
                _ -> (recurse term)) term))
  in (Rewriting.rewriteTerm rewrite)

substInType :: (Typing.TypeSubst -> Core.Type -> Core.Type)
substInType subst =  
  let rewrite = (\recurse -> \typ -> (\x -> case x of
          Core.TypeLambda v1 -> ((\x -> case x of
            Nothing -> (recurse typ)
            Just _ -> (Core.TypeLambda (Core.LambdaType {
              Core.lambdaTypeParameter = (Core.lambdaTypeParameter v1),
              Core.lambdaTypeBody = (substInType (removeVar (Core.lambdaTypeParameter v1)) (Core.lambdaTypeBody v1))}))) (Maps.lookup (Core.lambdaTypeParameter v1) (Typing.unTypeSubst subst)))
          Core.TypeVariable v1 -> ((\x -> case x of
            Nothing -> typ
            Just v2 -> v2) (Maps.lookup v1 (Typing.unTypeSubst subst)))
          _ -> (recurse typ)) typ) 
      removeVar = (\v -> Typing.TypeSubst (Maps.remove v (Typing.unTypeSubst subst)))
  in (Rewriting.rewriteType rewrite)

substInTypeScheme :: (Typing.TypeSubst -> Core.TypeScheme -> Core.TypeScheme)
substInTypeScheme subst ts = Core.TypeScheme {
  Core.typeSchemeVariables = (Core.typeSchemeVariables ts),
  Core.typeSchemeType = (substInType subst (Core.typeSchemeType ts))}

substTypesInTerm :: (Typing.TypeSubst -> Core.Term -> Core.Term)
substTypesInTerm subst =  
  let rewrite = (\recurse -> \term ->  
          let forFunction = (\f -> (\x -> case x of
                  Core.FunctionLambda v1 -> (forLambda v1)
                  _ -> (recurse term)) f) 
              forLambda = (\l -> recurse (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.lambdaParameter l),
                      Core.lambdaDomain = (Optionals.map (substInType subst) (Core.lambdaDomain l)),
                      Core.lambdaBody = (Core.lambdaBody l)}))))
              forLet = (\l ->  
                      let rewriteBinding = (\b -> Core.LetBinding {
                              Core.letBindingName = (Core.letBindingName b),
                              Core.letBindingTerm = (Core.letBindingTerm b),
                              Core.letBindingType = (Optionals.map (substInTypeScheme subst) (Core.letBindingType b))})
                      in (recurse (Core.TermLet (Core.Let {
                        Core.letBindings = (Lists.map rewriteBinding (Core.letBindings l)),
                        Core.letEnvironment = (Core.letEnvironment l)}))))
              forTypeAbstraction = (\ta ->  
                      let param = (Core.typeAbstractionParameter ta) 
                          subst2 = (Typing.TypeSubst (Maps.remove param (Typing.unTypeSubst subst)))
                      in (Core.TermTypeAbstraction (Core.TypeAbstraction {
                        Core.typeAbstractionParameter = param,
                        Core.typeAbstractionBody = (substTypesInTerm subst2 (Core.typeAbstractionBody ta))})))
          in ((\x -> case x of
            Core.TermFunction v1 -> (forFunction v1)
            Core.TermLet v1 -> (forLet v1)
            Core.TermTypeAbstraction v1 -> (forTypeAbstraction v1)
            Core.TermTypeApplication v1 -> (recurse (Core.TermTypeApplication (Core.TypedTerm {
              Core.typedTermTerm = (Core.typedTermTerm v1),
              Core.typedTermType = (substInType subst (Core.typedTermType v1))})))
            _ -> (recurse term)) term))
  in (Rewriting.rewriteTerm rewrite)