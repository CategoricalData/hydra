-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for type unification.

module Hydra.Unification where

import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Join two types, producing a list of type constraints.The comment is used to provide context for the constraints.
joinTypes :: Context.Context -> Core.Type -> Core.Type -> String -> Either (Context.InContext Errors.UnificationError) [Typing.TypeConstraint]
joinTypes cx left right comment =

      let sleft = Rewriting.deannotateType left
          sright = Rewriting.deannotateType right
          joinOne =
                  \l -> \r -> Typing.TypeConstraint {
                    Typing.typeConstraintLeft = l,
                    Typing.typeConstraintRight = r,
                    Typing.typeConstraintComment = (Strings.cat2 "join types; " comment)}
          cannotUnify =
                  Left (Context.InContext {
                    Context.inContextObject = Errors.UnificationError {
                      Errors.unificationErrorLeftType = sleft,
                      Errors.unificationErrorRightType = sright,
                      Errors.unificationErrorMessage = (Strings.cat2 (Strings.cat2 (Strings.cat2 "cannot unify " (Core_.type_ sleft)) " with ") (Core_.type_ sright))},
                    Context.inContextContext = cx})
          assertEqual = Logic.ifElse (Equality.equal sleft sright) (Right []) cannotUnify
          joinList =
                  \lefts -> \rights -> Logic.ifElse (Equality.equal (Lists.length lefts) (Lists.length rights)) (Right (Lists.zipWith joinOne lefts rights)) cannotUnify
          joinRowTypes =
                  \left -> \right -> Logic.ifElse (Logic.and (Equality.equal (Lists.length (Lists.map Core.fieldTypeName left)) (Lists.length (Lists.map Core.fieldTypeName right))) (Lists.foldl Logic.and True (Lists.zipWith (\left -> \right -> Equality.equal (Core.unName left) (Core.unName right)) (Lists.map Core.fieldTypeName left) (Lists.map Core.fieldTypeName right)))) (joinList (Lists.map Core.fieldTypeType left) (Lists.map Core.fieldTypeType right)) cannotUnify
      in case sleft of
        Core.TypeApplication v0 -> case sright of
          Core.TypeApplication v1 -> Right [
            joinOne (Core.applicationTypeFunction v0) (Core.applicationTypeFunction v1),
            (joinOne (Core.applicationTypeArgument v0) (Core.applicationTypeArgument v1))]
          _ -> cannotUnify
        Core.TypeEither v0 -> case sright of
          Core.TypeEither v1 -> Right [
            joinOne (Core.eitherTypeLeft v0) (Core.eitherTypeLeft v1),
            (joinOne (Core.eitherTypeRight v0) (Core.eitherTypeRight v1))]
          _ -> cannotUnify
        Core.TypeFunction v0 -> case sright of
          Core.TypeFunction v1 -> Right [
            joinOne (Core.functionTypeDomain v0) (Core.functionTypeDomain v1),
            (joinOne (Core.functionTypeCodomain v0) (Core.functionTypeCodomain v1))]
          _ -> cannotUnify
        Core.TypeList v0 -> case sright of
          Core.TypeList v1 -> Right [
            joinOne v0 v1]
          _ -> cannotUnify
        Core.TypeLiteral _ -> assertEqual
        Core.TypeMap v0 -> case sright of
          Core.TypeMap v1 -> Right [
            joinOne (Core.mapTypeKeys v0) (Core.mapTypeKeys v1),
            (joinOne (Core.mapTypeValues v0) (Core.mapTypeValues v1))]
          _ -> cannotUnify
        Core.TypeMaybe v0 -> case sright of
          Core.TypeMaybe v1 -> Right [
            joinOne v0 v1]
          _ -> cannotUnify
        Core.TypePair v0 -> case sright of
          Core.TypePair v1 -> Right [
            joinOne (Core.pairTypeFirst v0) (Core.pairTypeFirst v1),
            (joinOne (Core.pairTypeSecond v0) (Core.pairTypeSecond v1))]
          _ -> cannotUnify
        Core.TypeRecord v0 -> case sright of
          Core.TypeRecord v1 -> joinRowTypes v0 v1
          _ -> cannotUnify
        Core.TypeSet v0 -> case sright of
          Core.TypeSet v1 -> Right [
            joinOne v0 v1]
          _ -> cannotUnify
        Core.TypeUnion v0 -> case sright of
          Core.TypeUnion v1 -> joinRowTypes v0 v1
          _ -> cannotUnify
        Core.TypeUnit -> case sright of
          Core.TypeUnit -> Right []
          _ -> cannotUnify
        Core.TypeWrap v0 -> case sright of
          Core.TypeWrap v1 -> Right [
            joinOne v0 v1]
          _ -> cannotUnify
        _ -> cannotUnify

-- | Robinson's algorithm, following https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf
-- | Specifically this is an implementation of the following rules:
-- |   * Unify({(x, t)} ∪ E) = {t/x} Unify(E{t/x}) if x ∉ FV(t)
-- |   * Unify(∅) = I (the identity substitution x ↦ x)
-- |   * Unify({(x, x)} ∪ E) = Unify(E)
-- |   * Unify({(f(s1, ..., sn), f(t1, ..., tn))} ∪ E) = Unify({(s1, t1), ..., (sn, tn)} ∪ E))
unifyTypeConstraints :: Context.Context -> M.Map Core.Name t0 -> [Typing.TypeConstraint] -> Either (Context.InContext Errors.UnificationError) Typing.TypeSubst
unifyTypeConstraints cx schemaTypes constraints =

      let withConstraint =
              \c -> \rest ->
                let sleft = Rewriting.deannotateType (Typing.typeConstraintLeft c)
                    sright = Rewriting.deannotateType (Typing.typeConstraintRight c)
                    comment = Typing.typeConstraintComment c
                    bind =
                            \v -> \t ->
                              let subst = Substitution.singletonTypeSubst v t
                                  withResult = \s -> Substitution.composeTypeSubst subst s
                              in (Eithers.map withResult (unifyTypeConstraints cx schemaTypes (Substitution.substituteInConstraints subst rest)))
                    tryBinding =
                            \v -> \t -> Logic.ifElse (variableOccursInType v t) (Left (Context.InContext {
                              Context.inContextObject = Errors.UnificationError {
                                Errors.unificationErrorLeftType = sleft,
                                Errors.unificationErrorRightType = sright,
                                Errors.unificationErrorMessage = (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "Variable " (Core.unName v)) " appears free in type ") (Core_.type_ t)) " (") comment) ")")},
                              Context.inContextContext = cx})) (bind v t)
                    noVars =

                              let withConstraints = \constraints2 -> unifyTypeConstraints cx schemaTypes (Lists.concat2 constraints2 rest)
                              in (Eithers.bind (joinTypes cx sleft sright comment) withConstraints)
                    dflt =
                            case sright of
                              Core.TypeVariable v0 -> tryBinding v0 sleft
                              _ -> noVars
                in case sleft of
                  Core.TypeVariable v0 -> case sright of
                    Core.TypeVariable v1 -> Logic.ifElse (Equality.equal (Core.unName v0) (Core.unName v1)) (unifyTypeConstraints cx schemaTypes rest) (Logic.ifElse (Maybes.isJust (Maps.lookup v0 schemaTypes)) (Logic.ifElse (Maybes.isJust (Maps.lookup v1 schemaTypes)) (Left (Context.InContext {
                      Context.inContextObject = Errors.UnificationError {
                        Errors.unificationErrorLeftType = sleft,
                        Errors.unificationErrorRightType = sright,
                        Errors.unificationErrorMessage = (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "Attempted to unify schema names " (Core.unName v0)) " and ") (Core.unName v1)) " (") comment) ")")},
                      Context.inContextContext = cx})) (bind v1 sleft)) (bind v0 sright))
                    _ -> tryBinding v0 sright
                  _ -> dflt
      in (Logic.ifElse (Lists.null constraints) (Right Substitution.idTypeSubst) (withConstraint (Lists.head constraints) (Lists.tail constraints)))

unifyTypeLists :: Context.Context -> M.Map Core.Name t0 -> [Core.Type] -> [Core.Type] -> String -> Either (Context.InContext Errors.UnificationError) Typing.TypeSubst
unifyTypeLists cx schemaTypes l r comment =

      let toConstraint =
              \l -> \r -> Typing.TypeConstraint {
                Typing.typeConstraintLeft = l,
                Typing.typeConstraintRight = r,
                Typing.typeConstraintComment = comment}
      in (unifyTypeConstraints cx schemaTypes (Lists.zipWith toConstraint l r))

unifyTypes :: Context.Context -> M.Map Core.Name t0 -> Core.Type -> Core.Type -> String -> Either (Context.InContext Errors.UnificationError) Typing.TypeSubst
unifyTypes cx schemaTypes l r comment =
    unifyTypeConstraints cx schemaTypes [
      Typing.TypeConstraint {
        Typing.typeConstraintLeft = l,
        Typing.typeConstraintRight = r,
        Typing.typeConstraintComment = comment}]

-- | Determine whether a type variable appears within a type expression.No distinction is made between free and bound type variables.
variableOccursInType :: Core.Name -> Core.Type -> Bool
variableOccursInType var typ0 =

      let tryType =
              \b -> \typ -> case typ of
                Core.TypeVariable v0 -> Logic.or b (Equality.equal (Core.unName v0) (Core.unName var))
                _ -> b
      in (Rewriting.foldOverType Coders.TraversalOrderPre tryType False typ0)
