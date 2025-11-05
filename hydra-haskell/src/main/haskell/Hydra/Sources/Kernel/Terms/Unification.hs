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
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
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

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

joinTypesDef :: TBinding (Type -> Type -> String -> Flow s [TypeConstraint])
joinTypesDef = define "joinTypes" $
  doc ("Join two types, producing a list of type constraints."
    <> "The comment is used to provide context for the constraints.") $
  "left" ~> "right" ~> "comment" ~>
  "sleft" <~ ref Rewriting.deannotateTypeDef @@ var "left" $
  "sright" <~ ref Rewriting.deannotateTypeDef @@ var "right" $
  "joinOne" <~ ("l" ~> "r" ~> Typing.typeConstraint (var "l") (var "r") ("join types; " ++ var "comment")) $
  "cannotUnify" <~ Flows.fail ("cannot unify " ++ (ref ShowCore.typeDef @@ var "sleft") ++ " with " ++ (ref ShowCore.typeDef @@ var "sright")) $
  "assertEqual" <~ Logic.ifElse
    (Equality.equal (var "sleft") (var "sright"))
    (Flows.pure (list []))
    (var "cannotUnify") $
  "joinList" <~ ("lefts" ~> "rights" ~> Logic.ifElse
    (Equality.equal (Lists.length (var "lefts")) (Lists.length (var "rights")))
    (Flows.pure (Lists.zipWith (var "joinOne") (var "lefts") (var "rights")))
    (var "cannotUnify")) $
  "joinRowTypes" <~ ("left" ~> "right" ~> Logic.ifElse
    (Logic.and
      (Core.equalName_ (Core.rowTypeTypeName (var "left")) (Core.rowTypeTypeName (var "right")))
      (Core.equalNameList_
        (Lists.map (unaryFunction Core.fieldTypeName) (Core.rowTypeFields (var "left")))
        (Lists.map (unaryFunction Core.fieldTypeName) (Core.rowTypeFields (var "right")))))
    (var "joinList"
      @@ (Lists.map (unaryFunction Core.fieldTypeType) (Core.rowTypeFields (var "left")))
      @@ (Lists.map (unaryFunction Core.fieldTypeType) (Core.rowTypeFields (var "right"))))
    (var "cannotUnify")) $
  cases _Type (var "sleft") (Just (var "cannotUnify")) [
    _Type_application>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_application>>: "r" ~> Flows.pure (list [
        var "joinOne" @@ (Core.applicationTypeFunction (var "l")) @@ (Core.applicationTypeFunction (var "r")),
        var "joinOne" @@ (Core.applicationTypeArgument (var "l")) @@ (Core.applicationTypeArgument (var "r"))])],
    _Type_either>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_either>>: "r" ~> Flows.pure (list [
        var "joinOne" @@ (Core.eitherTypeLeft (var "l")) @@ (Core.eitherTypeLeft (var "r")),
        var "joinOne" @@ (Core.eitherTypeRight (var "l")) @@ (Core.eitherTypeRight (var "r"))])],
    _Type_function>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_function>>: "r" ~> Flows.pure (list [
        var "joinOne" @@ (Core.functionTypeDomain (var "l")) @@ (Core.functionTypeDomain (var "r")),
        var "joinOne" @@ (Core.functionTypeCodomain (var "l")) @@ (Core.functionTypeCodomain (var "r"))])],
    _Type_list>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_list>>: "r" ~> Flows.pure (list [
        var "joinOne" @@ (var "l") @@ (var "r")])],
    _Type_literal>>: constant (var "assertEqual"),
    _Type_map>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_map>>: "r" ~> Flows.pure (list [
        var "joinOne" @@ (Core.mapTypeKeys (var "l")) @@ (Core.mapTypeKeys (var "r")),
        var "joinOne" @@ (Core.mapTypeValues (var "l")) @@ (Core.mapTypeValues (var "r"))])],
    _Type_maybe>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_maybe>>: "r" ~> Flows.pure (list [
        var "joinOne" @@ (var "l") @@ (var "r")])],
    _Type_product>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_product>>: "r" ~> var "joinList" @@ (var "l") @@ (var "r")],
    _Type_record>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_record>>: "r" ~> var "joinRowTypes" @@ (var "l") @@ (var "r")],
    _Type_set>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_set>>: "r" ~> Flows.pure (list [
        var "joinOne" @@ (var "l") @@ (var "r")])],
    _Type_sum>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_sum>>: "r" ~> var "joinList" @@ (var "l") @@ (var "r")],
    _Type_union>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_union>>: "r" ~> var "joinRowTypes" @@ (var "l") @@ (var "r")],
    _Type_unit>>: constant (cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_unit>>: constant (Flows.pure (list []))]),
    _Type_wrap>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_wrap>>: "r" ~> Logic.ifElse
        (Core.equalName_ (Core.wrappedTypeTypeName (var "l")) (Core.wrappedTypeTypeName (var "r")))
        (Flows.pure (list [
          var "joinOne" @@ (Core.wrappedTypeBody (var "l")) @@ (Core.wrappedTypeBody (var "r"))]))
        (var "cannotUnify")]]

unifyTypeConstraintsDef :: TBinding (M.Map Name TypeScheme -> [TypeConstraint] -> Flow s TypeSubst)
unifyTypeConstraintsDef = define "unifyTypeConstraints" $
  doc (""
    <> "Robinson's algorithm, following https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf\n"
    <> "Specifically this is an implementation of the following rules:\n"
    <> "  * Unify({(x, t)} ∪ E) = {t/x} Unify(E{t/x}) if x ∉ FV(t)\n"
    <> "  * Unify(∅) = I (the identity substitution x ↦ x)\n"
    <> "  * Unify({(x, x)} ∪ E) = Unify(E)\n"
    <> "  * Unify({(f(s1, ..., sn), f(t1, ..., tn))} ∪ E) = Unify({(s1, t1), ..., (sn, tn)} ∪ E))") $
  "schemaTypes" ~> "constraints" ~>
  "withConstraint" <~ ("c" ~> "rest" ~>
    "sleft" <~ ref Rewriting.deannotateTypeDef @@ (Typing.typeConstraintLeft (var "c")) $
    "sright" <~ ref Rewriting.deannotateTypeDef @@ (Typing.typeConstraintRight (var "c")) $
    "comment" <~ Typing.typeConstraintComment (var "c") $
    "bind" <~ ("v" ~> "t" ~>
      "subst" <~ ref Substitution.singletonTypeSubstDef @@ var "v" @@ var "t" $
      "withResult" <~ ("s" ~> ref Substitution.composeTypeSubstDef @@ var "subst" @@ var "s") $
      Flows.map (var "withResult") (ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ (ref Substitution.substituteInConstraintsDef @@ var "subst" @@ var "rest"))) $
    "tryBinding" <~ ("v" ~> "t" ~> Logic.ifElse (ref variableOccursInTypeDef @@ var "v" @@ var "t")
      (Flows.fail ("Variable " ++ (Core.unName (var "v")) ++ " appears free in type " ++ (ref ShowCore.typeDef @@ var "t")
        ++ " (" ++ var "comment" ++ ")"))
      (var "bind" @@ var "v" @@ var "t")) $
    "noVars" <~ (
      "withConstraints" <~ ("constraints2" ~> ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ (Lists.concat2 (var "constraints2") (var "rest"))) $
      Flows.bind (ref joinTypesDef @@ var "sleft" @@ var "sright" @@ var "comment") (var "withConstraints")) $
    "dflt" <~ cases _Type (var "sright")
      (Just (var "noVars")) [
      _Type_variable>>: "name" ~> var "tryBinding" @@ var "name" @@ var "sleft"] $
    cases _Type (var "sleft")
      (Just $ var "dflt") [
      _Type_variable>>: "name" ~> cases _Type (var "sright")
        (Just (var "tryBinding" @@ var "name" @@ var "sright")) [
        _Type_variable>>: "name2" ~> Logic.ifElse (Core.equalName_ (var "name") (var "name2"))
          (ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ var "rest")
          (Logic.ifElse (Maybes.isJust (Maps.lookup (var "name") (var "schemaTypes")))
            (Logic.ifElse (Maybes.isJust (Maps.lookup (var "name2") (var "schemaTypes")))
              (Flows.fail ("Attempted to unify schema names " ++ (Core.unName (var "name")) ++ " and " ++ (Core.unName (var "name2"))
                ++ " (" ++ var "comment" ++ ")"))
              (var "bind" @@ var "name2" @@ var "sleft"))
            (var "bind" @@ var "name" @@ var "sright"))]]) $
  Logic.ifElse
    (Lists.null (var "constraints"))
    (Flows.pure (ref Substitution.idTypeSubstDef))
    (var "withConstraint" @@ (Lists.head (var "constraints")) @@ (Lists.tail (var "constraints")))

unifyTypeListsDef :: TBinding (M.Map Name TypeScheme -> [Type] -> [Type] -> String -> Flow s TypeSubst)
unifyTypeListsDef = define "unifyTypeLists" $
  "schemaTypes" ~> "l" ~> "r" ~> "comment" ~>
  "toConstraint" <~ ("l" ~> "r" ~> Typing.typeConstraint (var "l") (var "r") (var "comment")) $
  ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ (Lists.zipWith (var "toConstraint") (var "l") (var "r"))

unifyTypesDef :: TBinding (M.Map Name TypeScheme -> Type -> Type -> String -> Flow s TypeSubst)
unifyTypesDef = define "unifyTypes" $
  "schemaTypes" ~> "l" ~> "r" ~> "comment" ~>
  ref unifyTypeConstraintsDef @@ var "schemaTypes" @@ list [Typing.typeConstraint (var "l") (var "r") (var "comment")]

variableOccursInTypeDef :: TBinding (Name -> Type -> Bool)
variableOccursInTypeDef = define "variableOccursInType" $
  doc ("Determine whether a type variable appears within a type expression."
    <> "No distinction is made between free and bound type variables.") $
  "var" ~> "typ0" ~>
  "tryType" <~ ("b" ~> "typ" ~> match _Type (Just (var "b")) [
    _Type_variable>>: "v" ~> Logic.or (var "b") (Core.equalName_ (var "v") (var "var"))]
    @@ var "typ") $
  ref Rewriting.foldOverTypeDef @@ Coders.traversalOrderPre @@ var "tryType" @@ false @@ var "typ0"
