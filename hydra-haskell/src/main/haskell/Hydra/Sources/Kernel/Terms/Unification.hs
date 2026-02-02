
module Hydra.Sources.Kernel.Terms.Unification where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (joinTypes, unifyTypeConstraints, unifyTypeLists, unifyTypes, variableOccursInType)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution


ns :: Namespace
ns = Namespace "hydra.unification"

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns, ShowCore.ns, Substitution.ns]
    kernelTypesNamespaces $
    Just ("Utilities for type unification.")
  where
   elements = [
     toBinding joinTypes,
     toBinding unifyTypeConstraints,
     toBinding unifyTypeLists,
     toBinding unifyTypes,
     toBinding variableOccursInType]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

joinTypes :: TBinding (Type -> Type -> String -> Flow s [TypeConstraint])
joinTypes = define "joinTypes" $
  doc ("Join two types, producing a list of type constraints."
    <> "The comment is used to provide context for the constraints.") $
  "left" ~> "right" ~> "comment" ~>
  "sleft" <~ Rewriting.deannotateType @@ var "left" $
  "sright" <~ Rewriting.deannotateType @@ var "right" $
  "joinOne" <~ ("l" ~> "r" ~> Typing.typeConstraint (var "l") (var "r") ((string "join types; ") ++ var "comment")) $
  "cannotUnify" <~ Flows.fail ((string "cannot unify ") ++ (ShowCore.type_ @@ var "sleft") ++ (string " with ") ++ (ShowCore.type_ @@ var "sright")) $
  "assertEqual" <~ Logic.ifElse
    (Equality.equal (var "sleft") (var "sright"))
    (Flows.pure (list ([] :: [TTerm TypeConstraint])))
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
    _Type_pair>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_pair>>: "r" ~> Flows.pure (list [
        var "joinOne" @@ (Core.pairTypeFirst (var "l")) @@ (Core.pairTypeFirst (var "r")),
        var "joinOne" @@ (Core.pairTypeSecond (var "l")) @@ (Core.pairTypeSecond (var "r"))])],
    _Type_record>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_record>>: "r" ~> var "joinRowTypes" @@ (var "l") @@ (var "r")],
    _Type_set>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_set>>: "r" ~> Flows.pure (list [
        var "joinOne" @@ (var "l") @@ (var "r")])],
    _Type_union>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_union>>: "r" ~> var "joinRowTypes" @@ (var "l") @@ (var "r")],
    _Type_unit>>: constant (cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_unit>>: constant (Flows.pure (list ([] :: [TTerm TypeConstraint])))]),
    _Type_wrap>>: "l" ~> cases _Type (var "sright") (Just (var "cannotUnify")) [
      _Type_wrap>>: "r" ~> Logic.ifElse
        (Core.equalName_ (Core.wrappedTypeTypeName (var "l")) (Core.wrappedTypeTypeName (var "r")))
        (Flows.pure (list [
          var "joinOne" @@ (Core.wrappedTypeBody (var "l")) @@ (Core.wrappedTypeBody (var "r"))]))
        (var "cannotUnify")]]

unifyTypeConstraints :: TBinding (M.Map Name TypeScheme -> [TypeConstraint] -> Flow s TypeSubst)
unifyTypeConstraints = define "unifyTypeConstraints" $
  doc (""
    <> "Robinson's algorithm, following https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf\n"
    <> "Specifically this is an implementation of the following rules:\n"
    <> "  * Unify({(x, t)} ∪ E) = {t/x} Unify(E{t/x}) if x ∉ FV(t)\n"
    <> "  * Unify(∅) = I (the identity substitution x ↦ x)\n"
    <> "  * Unify({(x, x)} ∪ E) = Unify(E)\n"
    <> "  * Unify({(f(s1, ..., sn), f(t1, ..., tn))} ∪ E) = Unify({(s1, t1), ..., (sn, tn)} ∪ E))") $
  "schemaTypes" ~> "constraints" ~>
  "withConstraint" <~ ("c" ~> "rest" ~>
    "sleft" <~ Rewriting.deannotateType @@ (Typing.typeConstraintLeft (var "c")) $
    "sright" <~ Rewriting.deannotateType @@ (Typing.typeConstraintRight (var "c")) $
    "comment" <~ Typing.typeConstraintComment (var "c") $
    "bind" <~ ("v" ~> "t" ~>
      "subst" <~ Substitution.singletonTypeSubst @@ var "v" @@ var "t" $
      "withResult" <~ ("s" ~> Substitution.composeTypeSubst @@ var "subst" @@ var "s") $
      Flows.map (var "withResult") (unifyTypeConstraints @@ var "schemaTypes" @@ (Substitution.substituteInConstraints @@ var "subst" @@ var "rest"))) $
    "tryBinding" <~ ("v" ~> "t" ~> Logic.ifElse (variableOccursInType @@ var "v" @@ var "t")
      (Flows.fail ((string "Variable ") ++ (Core.unName (var "v")) ++ (string " appears free in type ") ++ (ShowCore.type_ @@ var "t")
        ++ (string " (") ++ var "comment" ++ (string ")")))
      (var "bind" @@ var "v" @@ var "t")) $
    "noVars" <~ (
      "withConstraints" <~ ("constraints2" ~> unifyTypeConstraints @@ var "schemaTypes" @@ (Lists.concat2 (var "constraints2") (var "rest"))) $
      Flows.bind (joinTypes @@ var "sleft" @@ var "sright" @@ var "comment") (var "withConstraints")) $
    "dflt" <~ cases _Type (var "sright")
      (Just (var "noVars")) [
      _Type_variable>>: "name" ~> var "tryBinding" @@ var "name" @@ var "sleft"] $
    cases _Type (var "sleft")
      (Just $ var "dflt") [
      _Type_variable>>: "name" ~> cases _Type (var "sright")
        (Just (var "tryBinding" @@ var "name" @@ var "sright")) [
        _Type_variable>>: "name2" ~> Logic.ifElse (Core.equalName_ (var "name") (var "name2"))
          (unifyTypeConstraints @@ var "schemaTypes" @@ var "rest")
          (Logic.ifElse (Maybes.isJust (Maps.lookup (var "name") (var "schemaTypes")))
            (Logic.ifElse (Maybes.isJust (Maps.lookup (var "name2") (var "schemaTypes")))
              (Flows.fail ((string "Attempted to unify schema names ") ++ (Core.unName (var "name")) ++ (string " and ") ++ (Core.unName (var "name2"))
                ++ (string " (") ++ var "comment" ++ (string ")")))
              (var "bind" @@ var "name2" @@ var "sleft"))
            (var "bind" @@ var "name" @@ var "sright"))]]) $
  Logic.ifElse
    (Lists.null (var "constraints"))
    (Flows.pure (Substitution.idTypeSubst))
    (var "withConstraint" @@ (Lists.head (var "constraints")) @@ (Lists.tail (var "constraints")))

unifyTypeLists :: TBinding (M.Map Name TypeScheme -> [Type] -> [Type] -> String -> Flow s TypeSubst)
unifyTypeLists = define "unifyTypeLists" $
  "schemaTypes" ~> "l" ~> "r" ~> "comment" ~>
  "toConstraint" <~ ("l" ~> "r" ~> Typing.typeConstraint (var "l") (var "r") (var "comment")) $
  unifyTypeConstraints @@ var "schemaTypes" @@ (Lists.zipWith (var "toConstraint") (var "l") (var "r"))

unifyTypes :: TBinding (M.Map Name TypeScheme -> Type -> Type -> String -> Flow s TypeSubst)
unifyTypes = define "unifyTypes" $
  "schemaTypes" ~> "l" ~> "r" ~> "comment" ~>
  unifyTypeConstraints @@ var "schemaTypes" @@ list [Typing.typeConstraint (var "l") (var "r") (var "comment")]

variableOccursInType :: TBinding (Name -> Type -> Bool)
variableOccursInType = define "variableOccursInType" $
  doc ("Determine whether a type variable appears within a type expression."
    <> "No distinction is made between free and bound type variables.") $
  "var" ~> "typ0" ~>
  "tryType" <~ ("b" ~> "typ" ~> match _Type (Just (var "b")) [
    _Type_variable>>: "v" ~> Logic.or (var "b") (Core.equalName_ (var "v") (var "var"))]
    @@ var "typ") $
  Rewriting.foldOverType @@ Coders.traversalOrderPre @@ var "tryType" @@ false @@ var "typ0"
