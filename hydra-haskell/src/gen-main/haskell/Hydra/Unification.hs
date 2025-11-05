-- | Utilities for type unification.

module Hydra.Unification where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

joinTypes :: (Core.Type -> Core.Type -> String -> Compute.Flow t0 [Typing.TypeConstraint])
joinTypes left right comment =  
  let sleft = (Rewriting.deannotateType left)
  in  
    let sright = (Rewriting.deannotateType right)
    in  
      let joinOne = (\l -> \r -> Typing.TypeConstraint {
              Typing.typeConstraintLeft = l,
              Typing.typeConstraintRight = r,
              Typing.typeConstraintComment = (Strings.cat [
                "join types; ",
                comment])})
      in  
        let cannotUnify = (Flows.fail (Strings.cat [
                Strings.cat [
                  Strings.cat [
                    "cannot unify ",
                    (Core_.type_ sleft)],
                  " with "],
                (Core_.type_ sright)]))
        in  
          let assertEqual = (Logic.ifElse (Equality.equal sleft sright) (Flows.pure []) cannotUnify)
          in  
            let joinList = (\lefts -> \rights -> Logic.ifElse (Equality.equal (Lists.length lefts) (Lists.length rights)) (Flows.pure (Lists.zipWith joinOne lefts rights)) cannotUnify)
            in  
              let joinRowTypes = (\left -> \right -> Logic.ifElse (Logic.and (Equality.equal (Core.unName (Core.rowTypeTypeName left)) (Core.unName (Core.rowTypeTypeName right))) (Logic.and (Equality.equal (Lists.length (Lists.map Core.fieldTypeName (Core.rowTypeFields left))) (Lists.length (Lists.map Core.fieldTypeName (Core.rowTypeFields right)))) (Lists.foldl Logic.and True (Lists.zipWith (\left -> \right -> Equality.equal (Core.unName left) (Core.unName right)) (Lists.map Core.fieldTypeName (Core.rowTypeFields left)) (Lists.map Core.fieldTypeName (Core.rowTypeFields right)))))) (joinList (Lists.map Core.fieldTypeType (Core.rowTypeFields left)) (Lists.map Core.fieldTypeType (Core.rowTypeFields right))) cannotUnify)
              in ((\x -> case x of
                Core.TypeApplication v1 -> ((\x -> case x of
                  Core.TypeApplication v2 -> (Flows.pure [
                    joinOne (Core.applicationTypeFunction v1) (Core.applicationTypeFunction v2),
                    (joinOne (Core.applicationTypeArgument v1) (Core.applicationTypeArgument v2))])
                  _ -> cannotUnify) sright)
                Core.TypeEither v1 -> ((\x -> case x of
                  Core.TypeEither v2 -> (Flows.pure [
                    joinOne (Core.eitherTypeLeft v1) (Core.eitherTypeLeft v2),
                    (joinOne (Core.eitherTypeRight v1) (Core.eitherTypeRight v2))])
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
                Core.TypeMaybe v1 -> ((\x -> case x of
                  Core.TypeMaybe v2 -> (Flows.pure [
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
                Core.TypeUnit -> ((\x -> case x of
                  Core.TypeUnit -> (Flows.pure [])
                  _ -> cannotUnify) sright)
                Core.TypeWrap v1 -> ((\x -> case x of
                  Core.TypeWrap v2 -> (Logic.ifElse (Equality.equal (Core.unName (Core.wrappedTypeTypeName v1)) (Core.unName (Core.wrappedTypeTypeName v2))) (Flows.pure [
                    joinOne (Core.wrappedTypeBody v1) (Core.wrappedTypeBody v2)]) cannotUnify)
                  _ -> cannotUnify) sright)
                _ -> cannotUnify) sleft)

unifyTypeConstraints :: (M.Map Core.Name t0 -> [Typing.TypeConstraint] -> Compute.Flow t1 Typing.TypeSubst)
unifyTypeConstraints schemaTypes constraints =  
  let withConstraint = (\c -> \rest ->  
          let sleft = (Rewriting.deannotateType (Typing.typeConstraintLeft c))
          in  
            let sright = (Rewriting.deannotateType (Typing.typeConstraintRight c))
            in  
              let comment = (Typing.typeConstraintComment c)
              in  
                let bind = (\v -> \t ->  
                        let subst = (Substitution.singletonTypeSubst v t)
                        in  
                          let withResult = (\s -> Substitution.composeTypeSubst subst s)
                          in (Flows.map withResult (unifyTypeConstraints schemaTypes (Substitution.substituteInConstraints subst rest))))
                in  
                  let tryBinding = (\v -> \t -> Logic.ifElse (variableOccursInType v t) (Flows.fail (Strings.cat [
                          Strings.cat [
                            Strings.cat [
                              Strings.cat [
                                Strings.cat [
                                  Strings.cat [
                                    "Variable ",
                                    (Core.unName v)],
                                  " appears free in type "],
                                (Core_.type_ t)],
                              " ("],
                            comment],
                          ")"])) (bind v t))
                  in  
                    let noVars =  
                            let withConstraints = (\constraints2 -> unifyTypeConstraints schemaTypes (Lists.concat2 constraints2 rest))
                            in (Flows.bind (joinTypes sleft sright comment) withConstraints)
                    in  
                      let dflt = ((\x -> case x of
                              Core.TypeVariable v1 -> (tryBinding v1 sleft)
                              _ -> noVars) sright)
                      in ((\x -> case x of
                        Core.TypeVariable v1 -> ((\x -> case x of
                          Core.TypeVariable v2 -> (Logic.ifElse (Equality.equal (Core.unName v1) (Core.unName v2)) (unifyTypeConstraints schemaTypes rest) (Logic.ifElse (Maybes.isJust (Maps.lookup v1 schemaTypes)) (Logic.ifElse (Maybes.isJust (Maps.lookup v2 schemaTypes)) (Flows.fail (Strings.cat [
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
                        _ -> dflt) sleft))
  in (Logic.ifElse (Lists.null constraints) (Flows.pure Substitution.idTypeSubst) (withConstraint (Lists.head constraints) (Lists.tail constraints)))

unifyTypeLists :: (M.Map Core.Name t0 -> [Core.Type] -> [Core.Type] -> String -> Compute.Flow t1 Typing.TypeSubst)
unifyTypeLists schemaTypes l r comment =  
  let toConstraint = (\l -> \r -> Typing.TypeConstraint {
          Typing.typeConstraintLeft = l,
          Typing.typeConstraintRight = r,
          Typing.typeConstraintComment = comment})
  in (unifyTypeConstraints schemaTypes (Lists.zipWith toConstraint l r))

unifyTypes :: (M.Map Core.Name t0 -> Core.Type -> Core.Type -> String -> Compute.Flow t1 Typing.TypeSubst)
unifyTypes schemaTypes l r comment = (unifyTypeConstraints schemaTypes [
  Typing.TypeConstraint {
    Typing.typeConstraintLeft = l,
    Typing.typeConstraintRight = r,
    Typing.typeConstraintComment = comment}])

-- | Determine whether a type variable appears within a type expression.No distinction is made between free and bound type variables.
variableOccursInType :: (Core.Name -> Core.Type -> Bool)
variableOccursInType var typ0 =  
  let tryType = (\b -> \typ -> (\x -> case x of
          Core.TypeVariable v1 -> (Logic.or b (Equality.equal (Core.unName v1) (Core.unName var)))
          _ -> b) typ)
  in (Rewriting.foldOverType Coders.TraversalOrderPre tryType False typ0)
