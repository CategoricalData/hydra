-- | Variable substitution in type expressions
module Hydra.Staging.Substitution where

import Hydra.Core
import Hydra.Mantle
import Hydra.Staging.Rewriting
import Hydra.Inference

import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Maybe    as Y


-- | The composition S T of two substitution is the result of first applying S, then T.
composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeTypeSubst s1 s2 =
    TypeSubst $ M.union addExtra $ fmap (substInType s2) $ unTypeSubst s1
  where
    addExtra = M.filterWithKey (\v _ -> Y.isNothing $ M.lookup v $ unTypeSubst s1) $ unTypeSubst s2

composeTypeSubstList :: [TypeSubst] -> TypeSubst
composeTypeSubstList = L.foldl composeTypeSubst emptyTypeSubst

emptyTypeSubst = TypeSubst M.empty
tmpTypeSubst = TypeSubst M.empty

singletonTypeSubst :: Name -> Type -> TypeSubst
singletonTypeSubst v t = TypeSubst $ M.singleton v t

substituteInTerm :: TermSubst -> Term -> Term
substituteInTerm (TermSubst s) = rewriteTerm rewrite
  where
    rewrite recurse term = case term of
      TermFunction (FunctionLambda (Lambda v mt body)) ->
          TermFunction $ FunctionLambda $ Lambda v mt $ substituteInTerm subst2 body
        where
          subst2 = TermSubst $ M.delete v s
      TermLet (Let bindings env) -> TermLet $ Let (fmap rewriteBinding bindings) $ substituteInTerm subst2 env
        where
          names = fmap letBindingName bindings
          subst2 = TermSubst $ M.filterWithKey (\k _ -> not $ L.elem k names) s
          rewriteBinding (LetBinding name term mt) = LetBinding name (substituteInTerm subst2 term) mt
      TermVariable name -> case M.lookup name s of
        Just sterm -> sterm
        Nothing -> recurse term
      _ -> recurse term

substInContext :: TypeSubst -> InferenceContext -> InferenceContext
substInContext subst cx = cx {
  inferenceContextDataTypes = fmap (substInTypeScheme subst) $ inferenceContextDataTypes cx}

substInType :: TypeSubst -> Type -> Type
substInType subst = rewriteType rewrite
  where
    rewrite recurse typ = case typ of
      TypeLambda (LambdaType v body) -> case M.lookup v (unTypeSubst subst) of
        Nothing -> recurse typ
        Just styp -> TypeLambda $ LambdaType v $ substInType subst2 body
          where
            subst2 = TypeSubst $ M.delete v $ unTypeSubst subst
      TypeVariable v -> case M.lookup v (unTypeSubst subst) of
        Nothing -> typ
        Just styp -> styp
      _ -> recurse typ

substInTypeScheme :: TypeSubst -> TypeScheme -> TypeScheme
substInTypeScheme subst (TypeScheme vars typ) = TypeScheme vars $ substInType subst2 typ
  where
    subst2 = TypeSubst $ M.filterWithKey (\k _ -> not $ k `elem` vars) $ unTypeSubst subst

substInTypeSchemeLegacy :: TypeSubst -> TypeScheme -> TypeScheme
substInTypeSchemeLegacy subst (TypeScheme vars typ) = TypeScheme vars $ substInType subst typ

substituteInConstraint :: TypeSubst -> TypeConstraint -> TypeConstraint
substituteInConstraint subst (TypeConstraint t1 t2 ctx) = TypeConstraint (substInType subst t1) (substInType subst t2) ctx

substituteInConstraints :: TypeSubst -> [TypeConstraint] -> [TypeConstraint]
substituteInConstraints subst = fmap (substituteInConstraint subst)

substTypesInTerm :: TypeSubst -> Term -> Term
substTypesInTerm subst = rewriteTerm rewrite
  where
    rewrite recurse term = case term of
      -- TODO: injections and case statements need a domain field as well, similar to lambdas
      TermFunction (FunctionLambda (Lambda v mt body)) -> recurse $ TermFunction $ FunctionLambda $ Lambda v (fmap (substInType subst) mt) body
      TermLet (Let bindings env) -> recurse $ TermLet $ Let (fmap rewriteBinding bindings) env
        where
          rewriteBinding (LetBinding v e mt) = LetBinding v e $ fmap (substInTypeScheme subst) mt
      TermTypeAbstraction (TypeAbstraction param body) -> TermTypeAbstraction $ TypeAbstraction param $ substTypesInTerm subst2 body
        where
          subst2 = TypeSubst $ M.delete param $ unTypeSubst subst
      TermTypeApplication (TypedTerm trm typ) -> recurse $ TermTypeApplication $ TypedTerm trm $ substInType subst typ
      _ -> recurse term
