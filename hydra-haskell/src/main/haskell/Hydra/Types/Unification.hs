module Hydra.Types.Unification (
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
import Hydra.Types.Substitution
import Hydra.CoreDecoding
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Default

import Control.Monad.Except
import Control.Monad.Identity

import qualified Data.Map as M
import qualified Data.Set as S


type Constraint m = (Type m, Type m)

type Solve a m = ExceptT (TypeError m) Identity a

data TypeError m
  = UnificationFail (Type m) (Type m)
  | InfiniteType TypeVariable (Type m)
  | UnboundVariable String
  | UnificationMismatch [Type m] [Type m]
  | ElementUndefined Name
  | InvalidTypeEncoding String deriving (Eq, Show)

type Unifier m = (Subst m, [Constraint m])


bind :: Eq m => TypeVariable -> Type m -> Solve (Subst m) m
bind a t | typeData t == TypeExprVariable a = return M.empty
         | variableOccursInType a t = throwError $ InfiniteType a t
         | otherwise = return $ M.singleton a t

solveConstraints :: (Default m, Eq m, Show m) => Context m -> [Constraint m] -> Either (TypeError m) (Subst m)
solveConstraints cx cs = runIdentity $ runExceptT $ unificationSolver cx (M.empty, cs)

unificationSolver :: (Default m, Eq m, Show m) => Context m -> Unifier m -> Solve (Subst m) m
unificationSolver cx (su, cs) = case cs of
  [] -> return su
  ((t1, t2): cs0) -> do
    su1  <- unify cx t1 t2
    unificationSolver cx (
      composeSubst su1 su,
      (\(t1, t2) -> (substituteInType su1 t1, substituteInType su1 t2)) <$> cs0)

unify :: (Default m, Eq m, Show m) => Context m -> Type m -> Type m -> Solve (Subst m) m
unify cx t1 t2 = if typeData t1 == typeData t2
    then return M.empty
    else case (typeData t1, typeData t2) of
      (TypeExprElement et1, TypeExprElement et2) -> unify cx et1 et2
      (TypeExprFunction (FunctionType t1 t2), TypeExprFunction (FunctionType t3 t4)) -> unifyMany cx [t1, t2] [t3, t4]
      (TypeExprList lt1, TypeExprList lt2) -> unify cx lt1 lt2
      (TypeExprMap (MapType k1 v1), TypeExprMap (MapType k2 v2)) -> unifyMany cx [k1, v1] [k2, v2]
--      (TypeExprNominal name, t) -> case M.lookup name (contextElements cx) of
--          Nothing -> throwError $ ElementUndefined name
--          Just (Element _ _ dat) -> case decodeType cx dat of
--            ResultFailure msg -> throwError $ InvalidTypeEncoding msg
--            ResultSuccess typ -> unify cx typ t
      (TypeExprOptional ot1, TypeExprOptional ot2) -> unify cx ot1 ot2
      (TypeExprRecord f1, TypeExprRecord f2) -> unifyRowType f1 f2
      (TypeExprSet st1, TypeExprSet st2) -> unify cx st1 st2
      (TypeExprUnion f1, TypeExprUnion f2) -> unifyRowType f1 f2
      (TypeExprUniversal (UniversalType v1 body1), TypeExprUniversal (UniversalType v2 body2)) -> unifyMany cx
        [Types.variable v1, body1] [Types.variable v2, body2]
      (TypeExprVariable v, _) -> bind v t2
      (_, TypeExprVariable v) -> bind v t1
      (TypeExprNominal name, _) -> return M.empty
      (_, TypeExprNominal name) -> unify cx (Types.nominal name) t1
      _ -> throwError $ UnificationFail t1 t2
  where
    unifyRowType f1 f2 = unifyMany cx (fieldTypeType <$> f1) (fieldTypeType <$> f2)

unifyMany :: (Default m, Eq m, Show m) => Context m -> [Type m] -> [Type m] -> Solve (Subst m) m
unifyMany _ [] [] = return M.empty
unifyMany cx (t1 : ts1) (t2 : ts2) =
  do su1 <- unify cx t1 t2
     su2 <- unifyMany cx (substituteInType su1 <$> ts1) (substituteInType su1 <$> ts2)
     return (composeSubst su2 su1)
unifyMany _ t1 t2 = throwError $ UnificationMismatch t1 t2

variableOccursInType ::  TypeVariable -> Type m -> Bool
variableOccursInType a t = S.member a $ freeVariablesInType t
