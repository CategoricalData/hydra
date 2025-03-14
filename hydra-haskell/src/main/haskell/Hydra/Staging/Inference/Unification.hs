-- | Type unification using an adaptation Robinson's algorithm

module Hydra.Staging.Inference.Unification where

import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.Lib.Flows as Flows
import Hydra.Lib.Io
import Hydra.Inference
import Hydra.Rewriting
import Hydra.Strip
import Hydra.Staging.Inference.Substitution

import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Maybe    as Y


-- | Determine whether a type variable appears within a type expression.
--   No distinction is made between free and bound type variables.
variableOccursInType :: Name -> Type -> Bool
variableOccursInType var = foldOverType TraversalOrderPre tryType False
  where
    tryType b typ = case typ of
      TypeVariable v -> b || v == var
      _ -> b

{-
Robinson's algorithm, following https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf
Specifically this is an implementation of the following rules:
 * Unify({(x, t)} ∪ E) = {t/x} Unify(E{t/x}) if x ∉ FV(t)
 * Unify(∅) = I (the identity substitution x ↦ x)
 * Unify({(x, x)} ∪ E) = Unify(E)
 * Unify({(f(s1, ..., sn), f(t1, ..., tn))} ∪ E) = Unify({(s1, t1), ..., (sn, tn)} ∪ E)
-}
unifyTypeConstraints :: M.Map Name TypeScheme -> [TypeConstraint] -> Flow s TypeSubst
unifyTypeConstraints schemaTypes constraints = case constraints of
  [] -> Flows.pure emptyTypeSubst
  ((TypeConstraint left right comment):rest) -> do
      result <- case sleft of
       TypeVariable name -> case sright of
           TypeVariable name2 -> if name == name2
             then unifyTypeConstraints schemaTypes rest
             -- Avoid replacing schema type references with temporary type variables.
             else if Y.isJust (M.lookup name schemaTypes)
             then if Y.isJust (M.lookup name2 schemaTypes)
               then Flows.fail $ "attempted to unify schema names " ++ unName name ++ " and " ++ unName name2
               else bind name2 sleft
             else bind name sright
           _ -> tryBinding name sright
       _ -> case sright of
         TypeVariable name -> tryBinding name sleft
         _ -> do
           constraints2 <- joinTypes sleft sright comment
           unifyTypeConstraints schemaTypes $ constraints2 ++ rest
      return result
    where
      sleft = stripType left
      sright = stripType right
      -- TODO: this occurrence check is expensive; consider delaying it until the time of substitution
      tryBinding v t = if variableOccursInType v t
        then Flows.fail $ "Variable " ++ unName v ++ " appears free in type " ++ showType t
          ++ " (" ++ comment ++ ")"
        else bind v t
      bind v t = composeTypeSubst subst <$> unifyTypeConstraints schemaTypes (substituteInConstraints subst rest)
        where
          subst = singletonTypeSubst v t

unifyTypeLists :: M.Map Name TypeScheme -> [Type] -> [Type] -> String -> Flow s TypeSubst
unifyTypeLists schemaTypes l r comment = unifyTypeConstraints schemaTypes $ L.zipWith toConstraint l r
  where
    toConstraint l r = TypeConstraint l r comment

unifyTypes :: M.Map Name TypeScheme -> Type -> Type -> String -> Flow s TypeSubst
unifyTypes schemaTypes l r comment = unifyTypeConstraints schemaTypes [TypeConstraint l r comment]

joinTypes :: Type -> Type -> String -> Flow s [TypeConstraint]
joinTypes left right comment = case sleft of
    TypeApplication (ApplicationType lhs1 rhs1) -> case sright of
      TypeApplication (ApplicationType lhs2 rhs2) -> Flows.pure [
        joinOne lhs1 lhs2,
        joinOne rhs1 rhs2]
      _ -> cannotUnify
    TypeFunction (FunctionType domleft codleft) -> case sright of
      TypeFunction (FunctionType domright codright) -> Flows.pure [
        joinOne domleft domright,
        joinOne codleft codright]
      _ -> cannotUnify
    TypeList eleft -> case sright of
      TypeList eright -> Flows.pure [joinOne eleft eright]
      _ -> cannotUnify
    TypeLiteral ltleft -> assertEqual
    TypeMap (MapType kleft vleft) -> case sright of
      TypeMap (MapType kright vright) -> Flows.pure [
        joinOne kleft kright,
        joinOne vleft vright]
      _ -> cannotUnify
    TypeOptional eleft -> case sright of
      TypeOptional eright -> Flows.pure [joinOne eleft eright]
      _ -> cannotUnify
    TypeProduct lefts -> case sright of
      TypeProduct rights -> joinList lefts rights
      _ -> cannotUnify
    TypeRecord rtleft -> case sright of
      TypeRecord rtright -> joinRowTypes rtleft rtright
      _ -> cannotUnify
    TypeSet eleft -> case sright of
      TypeSet eright -> Flows.pure [joinOne eleft eright]
      _ -> cannotUnify
    TypeSum lefts -> case sright of
      TypeSum rights -> joinList lefts rights
      _ -> cannotUnify
    TypeUnion rtleft -> case sright of
      TypeUnion rtright -> joinRowTypes rtleft rtright
      _ -> cannotUnify
    TypeWrap (WrappedType nameLeft eleft) -> case sright of
      TypeWrap (WrappedType nameRight eright) -> if nameLeft /= nameRight
        then cannotUnify
        else Flows.pure [joinOne eleft eright]
      _ -> cannotUnify
    -- TypeAnnotated, TypeLambda, TypeVariable should not appear here
    _ -> cannotUnify
  where
    sleft = stripType left
    sright = stripType right
    joinOne l r = TypeConstraint l r $ "join types; " ++ comment
    cannotUnify = Flows.fail $ "Cannot unify " ++ showType sleft ++ " with " ++ showType sright
    assertEqual = if sleft == sright
      then Flows.pure []
      else cannotUnify
    joinList lefts rights = if L.length lefts == L.length rights
      then Flows.pure $ L.zipWith joinOne lefts rights
      else cannotUnify
    fieldNames = fmap fieldTypeName
    fieldTypes = fmap fieldTypeType
    joinRowTypes (RowType tnameLeft fieldsLeft) (RowType tnameRight fieldsRight) = if tnameLeft /= tnameRight
      then cannotUnify
      else if (fieldNames fieldsLeft) /= (fieldNames fieldsRight)
      then cannotUnify
      else joinList (fieldTypes fieldsLeft) (fieldTypes fieldsRight)
