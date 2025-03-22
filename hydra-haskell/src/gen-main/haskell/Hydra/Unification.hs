-- | Utilities for type unification.

module Hydra.Unification where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Io as Io
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Strip as Strip
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

joinTypes :: (Core.Type -> Core.Type -> String -> Compute.Flow t0 [Typing.TypeConstraint])
joinTypes left right comment =  
  let sleft = (Strip.stripType left) 
      sright = (Strip.stripType right)
      joinOne = (\l -> \r -> Typing.TypeConstraint {
              Typing.typeConstraintLeft = l,
              Typing.typeConstraintRight = r,
              Typing.typeConstraintComment = (Strings.cat [
                "join types; ",
                comment])})
      cannotUnify = (Flows.fail (Strings.cat [
              Strings.cat [
                Strings.cat [
                  "Cannot unify ",
                  (Io.showType sleft)],
                " with "],
              (Io.showType sright)]))
      assertEqual = (Logic.ifElse (Equality.equalType sleft sright) (Flows.pure []) cannotUnify)
      joinList = (\lefts -> \rights -> Logic.ifElse (Equality.equalInt32 (Lists.length lefts) (Lists.length rights)) (Flows.pure (Lists.zipWith joinOne lefts rights)) cannotUnify)
      joinRowTypes = (\left -> \right -> Logic.ifElse (Logic.and (Equality.equalString (Core.unName (Core.rowTypeTypeName left)) (Core.unName (Core.rowTypeTypeName right))) (Logic.and (Equality.equalInt32 (Lists.length (Lists.map Core.fieldTypeName (Core.rowTypeFields left))) (Lists.length (Lists.map Core.fieldTypeName (Core.rowTypeFields right)))) (Lists.foldl Logic.and True (Lists.zipWith (\left -> \right -> Equality.equalString (Core.unName left) (Core.unName right)) (Lists.map Core.fieldTypeName (Core.rowTypeFields left)) (Lists.map Core.fieldTypeName (Core.rowTypeFields right)))))) (joinList (Lists.map Core.fieldTypeType (Core.rowTypeFields left)) (Lists.map Core.fieldTypeType (Core.rowTypeFields right))) cannotUnify)
  in ((\x -> case x of
    Core.TypeApplication v1 -> ((\x -> case x of
      Core.TypeApplication v2 -> (Flows.pure [
        joinOne (Core.applicationTypeFunction v1) (Core.applicationTypeFunction v2),
        (joinOne (Core.applicationTypeArgument v1) (Core.applicationTypeArgument v2))])
      _ -> cannotUnify) sright)
    Core.TypeFunction v1 -> ((\x -> case x of
      Core.TypeFunction v2 -> (Flows.pure [
        joinOne (Core.functionTypeDomain v1) (Core.functionTypeDomain v2),
        (joinOne (Core.functionTypeCodomain v1) (Core.functionTypeCodomain v2))])
      _ -> cannotUnify) sright)
    Core.TypeList v1 -> ((\x -> case x of
      Core.TypeList v2 -> (Flows.pure [
        joinOne v1 v2])
      _ -> cannotUnify) sright)
    Core.TypeLiteral _ -> assertEqual
    Core.TypeMap v1 -> ((\x -> case x of
      Core.TypeMap v2 -> (Flows.pure [
        joinOne (Core.mapTypeKeys v1) (Core.mapTypeKeys v2),
        (joinOne (Core.mapTypeValues v1) (Core.mapTypeValues v2))])
      _ -> cannotUnify) sright)
    Core.TypeOptional v1 -> ((\x -> case x of
      Core.TypeOptional v2 -> (Flows.pure [
        joinOne v1 v2])
      _ -> cannotUnify) sright)
    Core.TypeProduct v1 -> ((\x -> case x of
      Core.TypeProduct v2 -> (joinList v1 v2)
      _ -> cannotUnify) sright)
    Core.TypeRecord v1 -> ((\x -> case x of
      Core.TypeRecord v2 -> (joinRowTypes v1 v2)
      _ -> cannotUnify) sright)
    Core.TypeSet v1 -> ((\x -> case x of
      Core.TypeSet v2 -> (Flows.pure [
        joinOne v1 v2])
      _ -> cannotUnify) sright)
    Core.TypeSum v1 -> ((\x -> case x of
      Core.TypeSum v2 -> (joinList v1 v2)
      _ -> cannotUnify) sright)
    Core.TypeUnion v1 -> ((\x -> case x of
      Core.TypeUnion v2 -> (joinRowTypes v1 v2)
      _ -> cannotUnify) sright)
    Core.TypeWrap v1 -> ((\x -> case x of
      Core.TypeWrap v2 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.wrappedTypeTypeName v1)) (Core.unName (Core.wrappedTypeTypeName v2))) (Flows.pure [
        joinOne (Core.wrappedTypeObject v1) (Core.wrappedTypeObject v2)]) cannotUnify)
      _ -> cannotUnify) sright)
    _ -> cannotUnify) sleft)

unifyTypeConstraints :: (M.Map Core.Name t1 -> [Typing.TypeConstraint] -> Compute.Flow t0 Typing.TypeSubst)
unifyTypeConstraints schemaTypes constraints =  
  let withConstraint = (\c -> \rest ->  
          let sleft = (Strip.stripType (Typing.typeConstraintLeft c)) 
              sright = (Strip.stripType (Typing.typeConstraintRight c))
              comment = (Typing.typeConstraintComment c)
              tryBinding = (\v -> \t -> Logic.ifElse (variableOccursInType v t) (Flows.fail (Strings.cat [
                      Strings.cat [
                        Strings.cat [
                          Strings.cat [
                            Strings.cat [
                              Strings.cat [
                                "Variable ",
                                (Core.unName v)],
                              " appears free in type "],
                            (Io.showType t)],
                          " ("],
                        comment],
                      ")"])) (bind v t))
              bind = (\v -> \t ->  
                      let subst = (Substitution.singletonTypeSubst v t) 
                          withResult = (\s -> Substitution.composeTypeSubst subst s)
                      in (Flows.map withResult (unifyTypeConstraints schemaTypes (Substitution.substituteInConstraints subst rest))))
              noVars =  
                      let withConstraints = (\constraints2 -> unifyTypeConstraints schemaTypes (Lists.concat2 constraints2 rest))
                      in (Flows.bind (joinTypes sleft sright comment) withConstraints)
          in ((\x -> case x of
            Core.TypeVariable v1 -> ((\x -> case x of
              Core.TypeVariable v2 -> (Logic.ifElse (Equality.equalString (Core.unName v1) (Core.unName v2)) (unifyTypeConstraints schemaTypes rest) (Logic.ifElse (Optionals.isJust (Maps.lookup v1 schemaTypes)) (Logic.ifElse (Optionals.isJust (Maps.lookup v2 schemaTypes)) (Flows.fail (Strings.cat [
                Strings.cat [
                  Strings.cat [
                    Strings.cat [
                      Strings.cat [
                        Strings.cat [
                          "Attempted to unify schema names ",
                          (Core.unName v1)],
                        " and "],
                      (Core.unName v2)],
                    " ("],
                  comment],
                ")"])) (bind v2 sleft)) (bind v1 sright)))
              _ -> (tryBinding v1 sright)) sright)
            _ -> ((\x -> case x of
              Core.TypeVariable v1 -> (tryBinding v1 sleft)
              _ -> noVars) sright)) sleft))
  in (Logic.ifElse (Lists.null constraints) (Flows.pure Substitution.idTypeSubst) (withConstraint (Lists.head constraints) (Lists.tail constraints)))

unifyTypeLists :: (M.Map Core.Name t1 -> [Core.Type] -> [Core.Type] -> String -> Compute.Flow t0 Typing.TypeSubst)
unifyTypeLists schemaTypes l r comment =  
  let toConstraint = (\l -> \r -> Typing.TypeConstraint {
          Typing.typeConstraintLeft = l,
          Typing.typeConstraintRight = r,
          Typing.typeConstraintComment = comment})
  in (unifyTypeConstraints schemaTypes (Lists.zipWith toConstraint l r))

unifyTypes :: (M.Map Core.Name t1 -> Core.Type -> Core.Type -> String -> Compute.Flow t0 Typing.TypeSubst)
unifyTypes schemaTypes l r comment = (unifyTypeConstraints schemaTypes [
  Typing.TypeConstraint {
    Typing.typeConstraintLeft = l,
    Typing.typeConstraintRight = r,
    Typing.typeConstraintComment = comment}])

-- | Determine whether a type variable appears within a type expression.No distinction is made between free and bound type variables.
variableOccursInType :: (Core.Name -> Core.Type -> Bool)
variableOccursInType var =  
  let tryType = (\b -> \typ -> (\x -> case x of
          Core.TypeVariable v1 -> (Logic.or b (Equality.equalString (Core.unName v1) (Core.unName var)))
          _ -> b) typ)
  in (Rewriting.foldOverType Coders.TraversalOrderPre tryType False)