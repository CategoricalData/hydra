module Hydra.Reduction where

import Hydra.Core
import Hydra.Impl.Haskell.Extras
import Hydra.Evaluation
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Rewriting


betaReduceTypeRecursively :: (Default m, Ord m, Show m) => Bool -> Context m -> Type m -> Type m
betaReduceTypeRecursively eager cx = rewriteType mapExpr id
  where
    mapExpr _ t = case typeExpr t of
        TypeExprApplication (TypeApplication lhs rhs) -> case typeExpr lhs' of
            TypeExprLambda (TypeLambda v body) -> recurse $ replaceFreeTypeVariable v rhs' body
            TypeExprNominal name -> recurse $ Types.apply t' rhs' -- nominal types are transparent
              where
                ResultSuccess t' = requireType cx name
            _ -> t
          where
            lhs' = recurse lhs
            rhs' = if eager then recurse rhs else rhs
        _ -> t
    recurse = betaReduceTypeRecursively eager cx
