-- | Hindley-Milner style type unification

module Hydra.Unification (
  solveConstraints
) where

import Hydra.Variants
import Hydra.Strip
import Hydra.Compute
import Hydra.Core
import Hydra.Tools.Lexical
import Hydra.Mantle
import Hydra.Printing
import Hydra.Tools.Rewriting
import Hydra.Inference.Substitution
import Hydra.Rewriting
import Hydra.Dsl.Types as Types
import Hydra.Lib.Io

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- Note: type variables in Hydra are allowed to bind to type expressions which contain the variable;
--       i.e. type recursion by name is allowed.
bind :: Name -> Type -> Flow s Subst
bind name typ = do
  if typ == TypeVariable name
  then return M.empty
  else if variableOccursInType name typ
--     then fail $ "infinite type for " ++ unName name ++ ": " ++ show typ
    then return M.empty
    else return $ M.singleton name typ

solveConstraints :: [TypeConstraint] -> Flow s Subst
solveConstraints cs = unificationSolver M.empty cs

unificationSolver :: Subst -> [TypeConstraint] -> Flow s Subst
unificationSolver su cs = case cs of
  [] -> return su
  ((TypeConstraint t1 t2 _):rest) -> do
    su1  <- unify t1 t2
    unificationSolver
      (composeSubst su1 su)
      ((\(TypeConstraint t1 t2 ctx) -> (TypeConstraint (substituteInType su1 t1) (substituteInType su1 t2) ctx)) <$> rest)

unify :: Type -> Type -> Flow s Subst
unify ltyp rtyp = do
--     withTrace ("unify " ++ show ltyp ++ " with " ++ show rtyp) $
     case (stripType ltyp, stripType rtyp) of
       -- Symmetric patterns
      (TypeApplication (ApplicationType lhs1 rhs1), TypeApplication (ApplicationType lhs2 rhs2)) ->
        unifyMany [lhs1, rhs1] [lhs2, rhs2]
      (TypeFunction (FunctionType dom1 cod1), TypeFunction (FunctionType dom2 cod2)) ->
        unifyMany [dom1, cod1] [dom2, cod2]
      (TypeList lt1, TypeList lt2) -> unify lt1 lt2
      (TypeLiteral lt1, TypeLiteral lt2) -> verify "different literal types" $ lt1 == lt2
      (TypeMap (MapType k1 v1), TypeMap (MapType k2 v2)) -> unifyMany [k1, v1] [k2, v2]
      (TypeOptional ot1, TypeOptional ot2) -> unify ot1 ot2
      (TypeProduct types1, TypeProduct types2) -> unifyMany types1 types2
      (TypeRecord rt1, TypeRecord rt2) -> do
        verify "different record type names" (rowTypeTypeName rt1 == rowTypeTypeName rt2)
        verify "different number of record fields" (L.length (rowTypeFields rt1) == L.length (rowTypeFields rt2))
        unifyMany (fieldTypeType <$> rowTypeFields rt1) (fieldTypeType <$> rowTypeFields rt2)
      (TypeSet st1, TypeSet st2) -> unify st1 st2
      (TypeUnion rt1, TypeUnion rt2) -> verify "different union type names" (rowTypeTypeName rt1 == rowTypeTypeName rt2)
      (TypeLambda (LambdaType (Name v1) body1), TypeLambda (LambdaType (Name v2) body2)) ->
        unifyMany [Types.var v1, body1] [Types.var v2, body2]
      (TypeSum types1, TypeSum types2) -> unifyMany types1 types2
      (TypeWrap n1, TypeWrap n2) -> verify "different wrapper type names" $ n1 == n2

      -- Asymmetric patterns
      (TypeVariable v1, TypeVariable v2) -> bindWeakest v1 v2
      (TypeVariable v, t2) -> bind v t2
      (t1, TypeVariable v) -> bind v t1

      -- TODO; temporary "slop", e.g. (record "RowType" ...) is allowed to unify with (wrap "RowType" @ "a")
      (TypeApplication (ApplicationType lhs rhs), t2) -> unify lhs t2
      (t1, TypeApplication (ApplicationType lhs rhs)) -> unify t1 lhs
      (TypeLambda (LambdaType _ body), t2) -> unify body t2
      (t1, TypeLambda (LambdaType _ body)) -> unify t1 body
      -- TODO; temporary "slop", e.g. (record "RowType" ...) is allowed to unify with (wrap "RowType")
      (TypeWrap _, _) -> return M.empty -- TODO
      (_, TypeWrap name) -> return M.empty -- TODO

      (l, r) -> fail $ "unification of " ++ show (typeVariant l) ++ " with " ++ show (typeVariant r) ++
        ":\n  " ++ showType l ++
        "\n  " ++ showType r
  where
    verify reason b = if b then return M.empty else failUnification reason
    failUnification reason = fail $ "could not unify types (reason: " ++ reason ++ "):\n\t"
      ++ showType (stripType ltyp) ++ "\n\t"
      ++ showType (stripType rtyp) ++ "\n"
--     failUnification = fail $ "could not unify type " ++ describeType (stripType ltyp) ++ " with " ++ describeType (stripType rtyp)
    bindWeakest v1 v2 = if isWeak v1
        then bind v1 (TypeVariable v2)
        else bind v2 (TypeVariable v1)
      where
        isWeak v = L.head (unName v) == 't' -- TODO: use a convention like _xxx for temporarily variables, then normalize and replace them

unifyMany :: [Type] -> [Type] -> Flow s Subst
unifyMany [] [] = return M.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyMany (substituteInType su1 <$> ts1) (substituteInType su1 <$> ts2)
     return (composeSubst su2 su1)
unifyMany t1 t2 = fail $ "unification mismatch between " ++ show t1 ++ " and " ++ show t2

variableOccursInType :: Name -> Type -> Bool
variableOccursInType a t = S.member a $ freeVariablesInType t
