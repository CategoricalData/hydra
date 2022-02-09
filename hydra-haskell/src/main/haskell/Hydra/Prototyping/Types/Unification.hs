module Hydra.Prototyping.Types.Unification (
  Constraint,
  Solve,
  Subst,
  TypeError(..),
  solveConstraints,
  unify,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.Types.Substitution
import Hydra.Prototyping.CoreDecoding

import Control.Monad.Except
import Control.Monad.Identity

import qualified Data.Map as M
import qualified Data.Set as S


type Constraint = (Type, Type)

type Solve a = ExceptT TypeError Identity a

data TypeError
  = UnificationFail Type Type
  | InfiniteType TypeVariable Type
  | UnboundVariable String
  | UnificationMismatch [Type] [Type]
  | ElementUndefined Name
  | InvalidTypeEncoding String deriving (Eq, Show)

type Unifier = (Subst, [Constraint])


bind ::  TypeVariable -> Type -> Solve Subst
bind a t | t == TypeVariable a = return M.empty
         | variableOccursInType a t = throwError $ InfiniteType a t
         | otherwise = return $ M.singleton a t

solveConstraints :: Show m => Context m -> [Constraint] -> Either TypeError Subst
solveConstraints cx cs = runIdentity $ runExceptT $ unificationSolver cx (M.empty, cs)

unificationSolver :: Show m => Context m -> Unifier -> Solve Subst
unificationSolver cx (su, cs) = case cs of
  [] -> return su
  ((t1, t2): cs0) -> do
    su1  <- unify cx t1 t2
    unificationSolver cx (
      composeSubst su1 su,
      (\(t1, t2) -> (substituteInType su1 t1, substituteInType su1 t2)) <$> cs0)

unify :: Show m => Context m -> Type -> Type -> Solve Subst
unify cx t1 t2 = if t1 == t2
    then return M.empty
    else case (t1, t2) of
      (TypeElement et1, TypeElement et2) -> unify cx et1 et2
      (TypeFunction (FunctionType t1 t2), TypeFunction (FunctionType t3 t4)) -> unifyMany cx [t1, t2] [t3, t4]
      (TypeList lt1, TypeList lt2) -> unify cx lt1 lt2
      (TypeMap (MapType k1 v1), TypeMap (MapType k2 v2)) -> unifyMany cx [k1, v1] [k2, v2]
--      (TypeNominal name, t) -> case M.lookup name (contextElements cx) of
--          Nothing -> throwError $ ElementUndefined name
--          Just (Element _ _ dat) -> case decodeType cx dat of
--            ResultFailure msg -> throwError $ InvalidTypeEncoding msg
--            ResultSuccess typ -> unify cx typ t
      (TypeOptional ot1, TypeOptional ot2) -> unify cx ot1 ot2
      (TypeRecord f1, TypeRecord f2) -> unifyRowType f1 f2
      (TypeSet st1, TypeSet st2) -> unify cx st1 st2
      (TypeUnion f1, TypeUnion f2) -> unifyRowType f1 f2
      (TypeUniversal (UniversalType v1 body1), TypeUniversal (UniversalType v2 body2)) -> unifyMany cx
        [TypeVariable v1, body1] [TypeVariable v2, body2]
      (TypeVariable v, t) -> bind v t
      (t, TypeVariable v) -> bind v t
      (TypeNominal name, t) -> return M.empty
      (t, TypeNominal name) -> unify cx (TypeNominal name) t
      _ -> throwError $ UnificationFail t1 t2
  where
    unifyRowType f1 f2 = unifyMany cx (fieldTypeType <$> f1) (fieldTypeType <$> f2)

unifyMany :: Show m => Context m -> [Type] -> [Type] -> Solve Subst
unifyMany _ [] [] = return M.empty
unifyMany cx (t1 : ts1) (t2 : ts2) =
  do su1 <- unify cx t1 t2
     su2 <- unifyMany cx (substituteInType su1 <$> ts1) (substituteInType su1 <$> ts2)
     return (composeSubst su2 su1)
unifyMany _ t1 t2 = throwError $ UnificationMismatch t1 t2

variableOccursInType ::  TypeVariable -> Type -> Bool
variableOccursInType a t = S.member a $ freeVariablesInType t
