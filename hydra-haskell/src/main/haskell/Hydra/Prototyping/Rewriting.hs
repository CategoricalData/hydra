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

replaceTerm :: Ord a => TraversalOrder -> (Term a -> Term a) -> Term a -> Term a
replaceTerm order rep term = case order of
    TraversalOrderPre -> recurse $ rep term
    TraversalOrderPost -> rep $ recurse term
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

simplifyTerm :: Ord a => Term a -> Term a
simplifyTerm = replaceTerm TraversalOrderPre simplify
  where
    simplify term = case termData term of
      ExpressionApplication (Application lhs _) -> case termData lhs of
        ExpressionFunction (FunctionLambda (Lambda var body)) -> if S.member var (freeVariablesInTerm body)
          then term
          else Term (termData body) $ mergeMeta (termMeta term) (termMeta body)
        _ -> term
      _ -> term
    mergeMeta _ inner = inner -- For now, inner meta wins

stripMeta :: (Default a, Ord a) => Term a -> Term a
stripMeta = replaceTerm TraversalOrderPre $ \term -> term {termMeta = dflt}

subterms :: Term a -> [Term a]
subterms term = case termData term of
  ExpressionApplication (Application lhs rhs) -> [lhs, rhs]
  ExpressionFunction f -> case f of
    FunctionCases cases -> fieldTerm <$> cases
    FunctionCompareTo other -> [other]
    FunctionLambda (Lambda _ body) -> [body]
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
