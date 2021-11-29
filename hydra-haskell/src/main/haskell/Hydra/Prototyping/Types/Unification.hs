module Hydra.Prototyping.Types.Unification (
  Constraint,
  Solve,
  Subst,
  TypeError(..),
  unify,
) where

import Hydra.Core
import Hydra.Prototyping.Types.Substitution

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
  | UnificationMismatch [Type] [Type] deriving Show

bind ::  TypeVariable -> Type -> Solve Subst
bind a t | t == TypeVariable a = return M.empty
         | variableOccursInType a t = throwError $ InfiniteType a t
         | otherwise = return $ M.singleton a t

unify :: Type -> Type -> Solve Subst
unify t1 t2 = if t1 == t2
    then return M.empty
    else case (t1, t2) of
      (TypeElement et1, TypeElement et2) -> unify et1 et2
      (TypeFunction (FunctionType t1 t2), TypeFunction (FunctionType t3 t4)) -> unifyMany [t1, t2] [t3, t4]
      (TypeList lt1, TypeList lt2) -> unify lt1 lt2
      (TypeMap (MapType k1 v1), TypeMap (MapType k2 v2)) -> unifyMany [k1, v1] [k2, v2]
--      (TypeNominal name1, TypeNominal name2) -> TODO: not necessary until named polytypes are supported
      (TypeOptional ot1, TypeOptional ot2) -> unify ot1 ot2
      (TypeRecord f1, TypeRecord f2) -> unifyRowType f1 f2
      (TypeSet st1, TypeSet st2) -> unify st1 st2
      (TypeUnion f1, TypeUnion f2) -> unifyRowType f1 f2
--      (TypeUniversal... ) -> TODO: TypeUniversal is not yet used
      (TypeVariable v, t) -> bind v t
      (t, TypeVariable v) -> bind v t
      _ -> throwError $ UnificationFail t1 t2
  where
    unifyRowType f1 f2 = unifyMany (fieldTypeType <$> f1) (fieldTypeType <$> f2)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return M.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyMany (sustituteVariablesInType su1 <$> ts1) (sustituteVariablesInType su1 <$> ts2)
     return (composeSubstitutions su2 su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

variableOccursInType ::  TypeVariable -> Type -> Bool
variableOccursInType a t = S.member a $ freeVariablesInType t
