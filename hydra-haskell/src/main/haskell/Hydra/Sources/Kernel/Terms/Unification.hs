{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Unification where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution


module_ :: Module
module_ = Module (Namespace "hydra.unification") elements
    [ShowCore.module_, Substitution.module_]
    kernelTypesModules $
    Just ("Utilities for type unification.")
  where
   elements = [
     el joinTypesDef,
     el unifyTypeConstraintsDef,
     el unifyTypeListsDef,
     el unifyTypesDef,
     el variableOccursInTypeDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

joinTypesDef :: TElement (Type -> Type -> String -> Flow s [TypeConstraint])
joinTypesDef = define "joinTypes" $
  doc ("Join two types, producing a list of type constraints."
    <> "The comment is used to provide context for the constraints.") $
  lambdas ["left", "right", "comment"] $ lets [
    "sleft">: ref Rewriting.deannotateTypeDef @@ var  "left",
    "sright">: ref Rewriting.deannotateTypeDef @@ var "right",
    "joinOne">: lambdas ["l", "r"] $ Typing.typeConstraint (var "l") (var "r") ("join types; " ++ var "comment"),
    "cannotUnify">: Flows.fail ("Cannot unify " ++ (ref ShowCore.typeDef @@ var "sleft") ++ " with " ++ (ref ShowCore.typeDef @@ var "sright")),
    "assertEqual">: Logic.ifElse
      (Equality.equal (var "sleft") (var "sright"))
      (Flows.pure $ list [])
      (var "cannotUnify"),
    "joinList">: lambdas ["lefts", "rights"] $ Logic.ifElse
      (Equality.equal (Lists.length (var "lefts")) (Lists.length (var "rights")))
      (Flows.pure $ Lists.zipWith (var "joinOne") (var "lefts") (var "rights"))
      (var "cannotUnify"),
    "joinRowTypes">: lambdas ["left", "right"] $ Logic.ifElse
      (Logic.and
        (Core.equalName_ (Core.rowTypeTypeName $ var "left") (Core.rowTypeTypeName $ var "right"))
        (Core.equalNameList_
          (Lists.map (unaryFunction Core.fieldTypeName) $ Core.rowTypeFields $ var "left")
          (Lists.map (unaryFunction Core.fieldTypeName) $ Core.rowTypeFields $ var "right")))
      (var "joinList"
        @@ (Lists.map (unaryFunction Core.fieldTypeType) $ Core.rowTypeFields $ var "left")
        @@ (Lists.map (unaryFunction Core.fieldTypeType) $ Core.rowTypeFields $ var "right"))
      (var "cannotUnify")] $
    cases _Type (var "sleft") (Just $ var "cannotUnify") [
      _Type_application>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_application>>: lambda "r" $ Flows.pure $ list [
          var "joinOne" @@ (Core.applicationTypeFunction $ var "l") @@ (Core.applicationTypeFunction $ var "r"),
          var "joinOne" @@ (Core.applicationTypeArgument $ var "l") @@ (Core.applicationTypeArgument $ var "r")]],
      _Type_function>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_function>>: lambda "r" $ Flows.pure $ list [
          var "joinOne" @@ (Core.functionTypeDomain $ var "l") @@ (Core.functionTypeDomain $ var "r"),
          var "joinOne" @@ (Core.functionTypeCodomain $ var "l") @@ (Core.functionTypeCodomain $ var "r")]],
      _Type_list>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_list>>: lambda "r" $ Flows.pure $ list [
          var "joinOne" @@ (var "l") @@ (var "r")]],
      _Type_literal>>: constant $ var "assertEqual",
      _Type_map>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_map>>: lambda "r" $ Flows.pure $ list [
          var "joinOne" @@ (Core.mapTypeKeys $ var "l") @@ (Core.mapTypeKeys $ var "r"),
          var "joinOne" @@ (Core.mapTypeValues $ var "l") @@ (Core.mapTypeValues $ var "r")]],
      _Type_optional>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_optional>>: lambda "r" $ Flows.pure $ list [
          var "joinOne" @@ (var "l") @@ (var "r")]],
      _Type_product>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_product>>: lambda "r" $ var "joinList" @@ (var "l") @@ (var "r")],
      _Type_record>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_record>>: lambda "r" $ var "joinRowTypes" @@ (var "l") @@ (var "r")],
      _Type_set>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_set>>: lambda "r" $ Flows.pure $ list [
          var "joinOne" @@ (var "l") @@ (var "r")]],
      _Type_sum>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_sum>>: lambda "r" $ var "joinList" @@ (var "l") @@ (var "r")],
      _Type_union>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_union>>: lambda "r" $ var "joinRowTypes" @@ (var "l") @@ (var "r")],
      _Type_unit>>: constant $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_unit>>: constant $ Flows.pure $ list []],
      _Type_wrap>>: lambda "l" $ cases _Type (var "sright") (Just $ var "cannotUnify") [
        _Type_wrap>>: lambda "r" $ Logic.ifElse
          (Core.equalName_ (Core.wrappedTypeTypeName $ var "l") (Core.wrappedTypeTypeName $ var "r"))
          (Flows.pure $ list [
            var "joinOne" @@ (Core.wrappedTypeObject $ var "l") @@ (Core.wrappedTypeObject $ var "r")])
          (var "cannotUnify")]]

unifyTypeConstraintsDef :: TElement (M.Map Name TypeScheme -> [TypeConstraint] -> Flow s TypeSubst)
unifyTypeConstraintsDef = define "unifyTypeConstraints" $
  doc (""
    <> "Robinson's algorithm, following https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf\n"
    <> "Specifically this is an implementation of the following rules:\n"
    <> "  * Unify({(x, t)} ∪ E) = {t/x} Unify(E{t/x}) if x ∉ FV(t)\n"
    <> "  * Unify(∅) = I (the identity substitution x ↦ x)\n"
    <> "  * Unify({(x, x)} ∪ E) = Unify(E)\n"
    <> "  * Unify({(f(s1, ..., sn), f(t1, ..., tn))} ∪ E) = Unify({(s1, t1), ..., (sn, tn)} ∪ E))") $
  lambdas ["schemaTypes", "constraints"] $ lets [
    "withConstraint">: lambdas ["c", "rest"] $ lets [
      "sleft">: ref Rewriting.deannotateTypeDef @@ (Typing.typeConstraintLeft $ var "c"),
      "sright">: ref Rewriting.deannotateTypeDef @@ (Typing.typeConstraintRight $ var "c"),
      "comment">: Typing.typeConstraintComment $ var "c",
      -- TODO: this occurrence check is expensive; consider delaying it until the time of substitution
      "tryBinding">: lambdas ["v", "t"] $ Logic.ifElse (ref variableOccursInTypeDef @@ var "v" @@ var "t")
        (Flows.fail $ "Variable " ++ (Core.unName $ var "v") ++ " appears free in type " ++ (ref ShowCore.typeDef @@ var "t")
          ++ " (" ++ var "comment" ++ ")")
        (var "bind" @@ var "v" @@ var "t"),
      "bind">: lambdas ["v", "t"] $ lets [
        "subst">: ref Substitution.singletonTypeSubstDef @@ var "v" @@ var "t",
        "withResult">: lambda "s" $ ref Substitution.composeTypeSubstDef @@ var "subst" @@ var "s"] $
        Flows.map (var "withResult") (ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ (ref Substitution.substituteInConstraintsDef @@ var "subst" @@ var "rest")),
      "noVars">: lets [
        "withConstraints">: lambda "constraints2" $ ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ (Lists.concat2 (var "constraints2") (var "rest"))] $
        Flows.bind (ref joinTypesDef @@ var "sleft" @@ var "sright" @@ var "comment") (var "withConstraints")] $
      cases _Type (var "sleft")
        (Just $ cases _Type (var "sright")
          (Just $ var "noVars") [
          _Type_variable>>: lambda "name" $ var "tryBinding" @@ var "name" @@ var "sleft"]) [
        _Type_variable>>: lambda "name" $ cases _Type (var "sright")
          (Just $ var "tryBinding" @@ var "name" @@ var "sright") [
          _Type_variable>>: lambda "name2" $ Logic.ifElse (Core.equalName_ (var "name") (var "name2"))
            (ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ var "rest")
            -- Avoid replacing schema type references with temporary type variables.
            (Logic.ifElse (Optionals.isJust $ Maps.lookup (var "name") (var "schemaTypes"))
              (Logic.ifElse (Optionals.isJust $ Maps.lookup (var "name2") (var "schemaTypes"))
                (Flows.fail $ "Attempted to unify schema names " ++ (Core.unName $ var "name") ++ " and " ++ (Core.unName $ var "name2")
                  ++ " (" ++ var "comment" ++ ")")
                (var "bind" @@ var "name2" @@ var "sleft"))
              (var "bind" @@ var "name" @@ var "sright"))]]] $
    Logic.ifElse
      (Lists.null $ var "constraints")
      (Flows.pure $ ref Substitution.idTypeSubstDef)
      (var "withConstraint" @@ (Lists.head $ var "constraints") @@ (Lists.tail $ var "constraints"))

unifyTypeListsDef :: TElement (M.Map Name TypeScheme -> [Type] -> [Type] -> String -> Flow s TypeSubst)
unifyTypeListsDef = define "unifyTypeLists" $
  lambdas ["schemaTypes", "l", "r", "comment"] $ lets [
    "toConstraint">: lambdas ["l", "r"] $ Typing.typeConstraint (var "l") (var "r") (var "comment")] $
    ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ (Lists.zipWith (var "toConstraint") (var "l") (var "r"))

unifyTypesDef :: TElement (M.Map Name TypeScheme -> Type -> Type -> String -> Flow s TypeSubst)
unifyTypesDef = define "unifyTypes" $
  lambdas ["schemaTypes", "l", "r", "comment"] $
    ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ list [Typing.typeConstraint (var "l") (var "r") (var "comment")]

variableOccursInTypeDef :: TElement (Name -> Type -> Bool)
variableOccursInTypeDef = define "variableOccursInType" $
  doc ("Determine whether a type variable appears within a type expression."
    <> "No distinction is made between free and bound type variables.") $
  lambda "var" $ lets [
    "tryType">: lambdas ["b", "typ"] $ match _Type (Just $ var "b") [
      _Type_variable>>: lambda "v" $ Logic.or (var "b") (Core.equalName_ (var "v") (var "var"))]
      @@ var "typ"] $
    ref Rewriting.foldOverTypeDef @@ Coders.traversalOrderPre @@ var "tryType" @@ false
