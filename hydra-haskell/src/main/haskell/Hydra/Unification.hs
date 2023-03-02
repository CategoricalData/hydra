-- | Hindley-Milner style type unification

module Hydra.Unification (
  Constraint,
  solveConstraints,
) where

import Hydra.Basics
import Hydra.Common
import Hydra.Core
import Hydra.Lexical
import Hydra.Monads
import Hydra.Rewriting
import Hydra.Substitution
import Hydra.Dsl.Types as Types

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


type Constraint m = (Type m, Type m)

type Unifier m = (Subst m, [Constraint m])

bind :: (Eq m, Show m) => Name -> Type m -> GraphFlow m (Subst m)
bind a t | t == TypeVariable a = return M.empty
         | variableOccursInType a t = fail $ "infinite type for ?" ++ unName a ++ ": " ++ show t
         | otherwise = return $ M.singleton a t

solveConstraints :: (Eq m, Show m) => [Constraint m] -> GraphFlow m (Subst m)
solveConstraints cs = unificationSolver (M.empty, cs)

unificationSolver :: (Eq m, Show m) => Unifier m -> GraphFlow m (Subst m)
unificationSolver (su, cs) = case cs of
  [] -> return su
  ((t1, t2):rest) -> do
    su1  <- unify t1 t2
    unificationSolver (
      composeSubst su1 su,
      (\(t1, t2) -> (substituteInType su1 t1, substituteInType su1 t2)) <$> rest)

unify :: (Eq m, Show m) => Type m -> Type m -> GraphFlow m (Subst m)
unify t1' t2' = case (stripType t1', stripType t2') of
       -- Symmetric patterns
      (TypeApplication (ApplicationType lhs1 rhs1), TypeApplication (ApplicationType lhs2 rhs2)) ->
        unifyMany [lhs1, rhs1] [lhs2, rhs2]
      (TypeElement et1, TypeElement et2) -> unify et1 et2
      (TypeFunction (FunctionType dom cod), TypeFunction (FunctionType t3 t4)) -> unifyMany [dom, cod] [t3, t4]
      (TypeList lt1, TypeList lt2) -> unify lt1 lt2
      (TypeLiteral lt1, TypeLiteral lt2) -> verify $ lt1 == lt2
      (TypeMap (MapType k1 v1), TypeMap (MapType k2 v2)) -> unifyMany [k1, v1] [k2, v2]
      (TypeOptional ot1, TypeOptional ot2) -> unify ot1 ot2
      (TypeProduct types1, TypeProduct types2) -> unifyMany types1 types2
      (TypeRecord rt1, TypeRecord rt2) -> do
        verify (rowTypeTypeName rt1 == rowTypeTypeName rt2)
        verify (L.length (rowTypeFields rt1) == L.length (rowTypeFields rt2))
        unifyMany (fieldTypeType <$> rowTypeFields rt1) (fieldTypeType <$> rowTypeFields rt2)
      (TypeSet st1, TypeSet st2) -> unify st1 st2
      (TypeUnion rt1, TypeUnion rt2) -> verify (rowTypeTypeName rt1 == rowTypeTypeName rt2)
      (TypeLambda (LambdaType (Name v1) body1), TypeLambda (LambdaType (Name v2) body2)) ->
        unifyMany [Types.variable v1, body1] [Types.variable v2, body2]
      (TypeSum types1, TypeSum types2) -> unifyMany types1 types2
      (TypeWrap n1, TypeWrap n2) -> verify $ n1 == n2

      -- Asymmetric patterns
      (TypeVariable v, t2) -> bind v t2
      (t1, TypeVariable v) -> bind v t1

      -- TODO; temporary "slop", e.g. (record "RowType" ...) is allowed to unify with (wrap "RowType" @ "a")
      (TypeApplication (ApplicationType lhs rhs), t2) -> unify lhs t2
      (t1, TypeApplication (ApplicationType lhs rhs)) -> unify t1 lhs
      -- TODO; temporary "slop", e.g. (record "RowType" ...) is allowed to unify with (wrap "RowType")
      (TypeWrap _, _) -> return M.empty -- TODO
      (_, TypeWrap name) -> return M.empty -- TODO
      
      (l, r) -> fail $ "unexpected unification of " ++ show (typeVariant l) ++ " with " ++ show (typeVariant r) ++
        ":\n  " ++ show l ++ "\n  " ++ show r
  where
    verify b = if b then return M.empty else failUnification
    failUnification = fail $ "could not unify type " ++ show (stripType t1') ++ " with " ++ show (stripType t2')

unifyMany :: (Eq m, Show m) => [Type m] -> [Type m] -> GraphFlow m (Subst m)
unifyMany [] [] = return M.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyMany (substituteInType su1 <$> ts1) (substituteInType su1 <$> ts2)
     return (composeSubst su2 su1)
unifyMany t1 t2 = fail $ "unification mismatch between " ++ show t1 ++ " and " ++ show t2

variableOccursInType ::  Show m => Name -> Type m -> Bool
variableOccursInType a t = S.member a $ freeVariablesInType t
