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
import Hydra.Types.Substitution
import Hydra.Impl.Haskell.Dsl.Types as Types

import Control.Monad.Except
import Control.Monad.Identity

import qualified Data.Map as M
import qualified Data.Set as S


type Constraint m = (Type m, Type m)

type Solve a m = ExceptT (TypeError m) Identity a

data TypeError m
  = UnificationFail (Type m) (Type m)
  | InfiniteType VariableType (Type m)
  | UnboundVariable Variable
  | UnificationMismatch [Type m] [Type m]
  | ElementUndefined Name
  | InvalidTypeEncoding String deriving (Eq, Show)

type Unifier m = (Subst m, [Constraint m])


bind :: Eq m => VariableType -> Type m -> Solve (Subst m) m
bind a t | t == TypeVariable a = return M.empty
         | variableOccursInType a t = throwError $ InfiniteType a t
         | otherwise = return $ M.singleton a t

solveConstraints :: (Eq m, Show m) => Context m -> [Constraint m] -> Either (TypeError m) (Subst m)
solveConstraints cx cs = runIdentity $ runExceptT $ unificationSolver cx (M.empty, cs)

unificationSolver :: (Eq m, Show m) => Context m -> Unifier m -> Solve (Subst m) m
unificationSolver scx (su, cs) = case cs of
  [] -> return su
  ((t1, t2): cs0) -> do
    su1  <- unify scx t1 t2
    unificationSolver scx (
      composeSubst su1 su,
      (\(t1, t2) -> (substituteInType su1 t1, substituteInType su1 t2)) <$> cs0)

unify :: (Eq m, Show m) => Context m -> Type m -> Type m -> Solve (Subst m) m
unify cx t1 t2 = if t1 == t2
    then return M.empty
    else case (t1, t2) of
      (TypeAnnotated (Annotated at _), _) -> unify cx at t2
      (_, TypeAnnotated (Annotated at _)) -> unify cx t1 at
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
      (TypeLambda (LambdaType (VariableType v1) body1), TypeLambda (LambdaType (VariableType v2) body2)) -> unifyMany cx
        [Types.variable v1, body1] [Types.variable v2, body2]
      (TypeVariable v, _) -> bind v t2
      (_, TypeVariable v) -> bind v t1
      (TypeNominal name, _) -> return M.empty
      (_, TypeNominal name) -> unify cx (Types.nominal name) t1
      _ -> throwError $ UnificationFail t1 t2
  where
    unifyRowType f1 f2 = unifyMany cx (fieldTypeType <$> f1) (fieldTypeType <$> f2)

unifyMany :: (Eq m, Show m) => Context m -> [Type m] -> [Type m] -> Solve (Subst m) m
unifyMany _ [] [] = return M.empty
unifyMany cx (t1 : ts1) (t2 : ts2) =
  do su1 <- unify cx t1 t2
     su2 <- unifyMany cx (substituteInType su1 <$> ts1) (substituteInType su1 <$> ts2)
     return (composeSubst su2 su1)
unifyMany _ t1 t2 = throwError $ UnificationMismatch t1 t2

variableOccursInType ::  VariableType -> Type m -> Bool
variableOccursInType a t = S.member a $ freeVariablesInType t
