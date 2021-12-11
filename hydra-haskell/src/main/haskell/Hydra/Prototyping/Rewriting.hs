module Hydra.Prototyping.Rewriting (
  TraversalOrder(..),
  foldOverTerm,
  freeVariablesInTerm,
  replaceTerm,
  simplifyTerm,
  stripMeta,
  subterms,
  ) where

import Hydra.Core
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.CoreMeta

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data TraversalOrder = TraversalOrderPre | TraversalOrderPost

foldOverTerm :: TraversalOrder -> (b -> Term a -> b) -> b -> Term a -> b
foldOverTerm order fld b0 term = case order of
    TraversalOrderPre -> L.foldl (foldOverTerm order fld) (fld b0 term) children
    TraversalOrderPost -> fld (L.foldl (foldOverTerm order fld) b0 children) term
  where
    children = subterms term

freeVariablesInTerm :: Term a -> S.Set Variable
freeVariablesInTerm term = case termData term of
  ExpressionFunction (FunctionLambda (Lambda var body)) -> S.delete var $ freeVariablesInTerm body
  ExpressionVariable v -> S.fromList [v]
  _ -> L.foldl (\s t -> S.union s $ freeVariablesInTerm t) S.empty $ subterms term

replaceTerm :: Ord a => TraversalOrder -> (Term a -> Y.Maybe (Term a)) -> Term a -> Term a
replaceTerm order rep term = case order of
    TraversalOrderPre -> Y.maybe term recurse (rep term)
    TraversalOrderPost -> Y.fromMaybe term $ rep $ recurse term
  where
    replace = replaceTerm order rep
    replaceField f = f {fieldTerm = replace (fieldTerm f)}
    recurse term' = term' {termData = helper $ termData term'}
      where
        helper expr = case expr of
          ExpressionApplication (Application lhs rhs) -> ExpressionApplication $ Application (replace lhs) (replace rhs)
          ExpressionFunction fun -> ExpressionFunction $ case fun of
            FunctionCases fields -> FunctionCases $ replaceField <$> fields
            FunctionCompareTo other -> FunctionCompareTo $ replace other
            FunctionLambda (Lambda v body) -> FunctionLambda $ Lambda v $ replace body
            FunctionOptionalCases (OptionalCases nothing just) -> FunctionOptionalCases
              (OptionalCases (replace nothing) (replace just))
            _ -> fun
          ExpressionLet (Let v t1 t2) -> ExpressionLet $ Let v (replace t1) (replace t2)
          ExpressionList els -> ExpressionList $ replace <$> els
          ExpressionMap m -> ExpressionMap $ M.fromList $ (\(k, v) -> (replace k, replace v)) <$> M.toList m
          ExpressionNominal (NominalTerm name t) -> ExpressionNominal (NominalTerm name $ replace t)
          ExpressionOptional m -> ExpressionOptional $ replace <$> m
          ExpressionRecord fields -> ExpressionRecord $ replaceField <$> fields
          ExpressionSet s -> ExpressionSet $ S.fromList $ replace <$> S.toList s
          ExpressionUnion field -> ExpressionUnion $ replaceField field
          _ -> expr

simplifyTerm :: (Default a, Ord a) => Term a -> Term a
simplifyTerm = replaceTerm TraversalOrderPre simplify
  where
    simplify term = Just $ case termData term of
      ExpressionApplication (Application lhs rhs) -> case termData lhs of
        ExpressionFunction (FunctionLambda (Lambda var body)) -> if S.member var (freeVariablesInTerm body)
          then case termData rhs of
            ExpressionVariable v -> substituteVariable var v body
            _ -> term
          else body
        _ -> term
      _ -> term

stripMeta :: (Default a, Ord a) => Term a -> Term a
stripMeta = replaceTerm TraversalOrderPre $ \term -> Just $ term {termMeta = dflt}

substituteVariable :: (Default a, Ord a) => Variable -> Variable -> Term a -> Term a
substituteVariable from to = replaceTerm TraversalOrderPre replace
  where
    replace term = case termData term of
      ExpressionVariable x -> Just $ Term (ExpressionVariable $ if x == from then to else x) $ termMeta term
      ExpressionFunction (FunctionLambda (Lambda var _)) -> if var == from
        then Nothing
        else Just term
      _ -> Just term

subterms :: Term a -> [Term a]
subterms term = case termData term of
  ExpressionApplication (Application lhs rhs) -> [lhs, rhs]
  ExpressionFunction f -> case f of
    FunctionCases cases -> fieldTerm <$> cases
    FunctionCompareTo other -> [other]
    FunctionLambda (Lambda _ body) -> [body]
    FunctionOptionalCases (OptionalCases nothing just) -> [nothing, just]
    _ -> []
  ExpressionLet (Let _ t1 t2) -> [t1, t2]
  ExpressionList els -> els
  ExpressionMap m -> L.concat ((\(k, v) -> [k, v]) <$> M.toList m)
  ExpressionNominal (NominalTerm _ t) -> [t]
  ExpressionOptional m -> Y.maybeToList m
  ExpressionRecord fields -> fieldTerm <$> fields
  ExpressionSet s -> S.toList s
  ExpressionUnion field -> [fieldTerm field]
  _ -> []
