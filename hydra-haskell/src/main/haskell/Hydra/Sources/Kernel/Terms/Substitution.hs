{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Substitution where

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

module_ :: Module
module_ = Module (Namespace "hydra.substitution") elements
    [Rewriting.module_]
    kernelTypesModules $
    Just ("Variable substitution in type and term expressions.")
  where
   elements = [
     el composeTypeSubstDef,
     el composeTypeSubstListDef,
     el idTypeSubstDef,
     el singletonTypeSubstDef,
     el substituteInConstraintDef,
     el substituteInConstraintsDef,
     el substInContextDef,
     el substituteInTermDef,
     el substInTypeDef,
     el substInTypeSchemeDef,
     el substTypesInTermDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

composeTypeSubstDef :: TBinding (TypeSubst -> TypeSubst -> TypeSubst)
composeTypeSubstDef = define "composeTypeSubst" $
  doc "Compose two type substitutions" $
  lambdas ["s1", "s2"] $ lets [
    "isExtra">: lambdas ["k", "v"] $ Maybes.isNothing (Maps.lookup (var "k") (Typing.unTypeSubst $ var "s1")),
    "withExtra">: Maps.filterWithKey (var "isExtra") (Typing.unTypeSubst $ var "s2")] $
    Typing.typeSubst $ Maps.union (var "withExtra") $ Maps.map (ref substInTypeDef @@ var "s2") $ Typing.unTypeSubst $ var "s1"

composeTypeSubstListDef :: TBinding ([TypeSubst] -> TypeSubst)
composeTypeSubstListDef = define "composeTypeSubstList" $
  doc "Compose a list of type substitutions" $
  Phantoms.fold (ref composeTypeSubstDef) @@ ref idTypeSubstDef

idTypeSubstDef :: TBinding TypeSubst
idTypeSubstDef = define "idTypeSubst" $
  doc "The identity type substitution" $
  Typing.typeSubst Maps.empty

singletonTypeSubstDef :: TBinding (Name -> Type -> TypeSubst)
singletonTypeSubstDef = define "singletonTypeSubst" $
  doc "Create a type substitution with a single variable mapping" $
  lambdas ["v", "t"] $ Typing.typeSubst $ Maps.singleton (var "v") (var "t")

substituteInConstraintDef :: TBinding (TypeSubst -> TypeConstraint -> TypeConstraint)
substituteInConstraintDef = define "substituteInConstraint" $
  doc "Apply a type substitution to a type constraint" $
  lambdas ["subst", "c"] $ Typing.typeConstraint
    (ref substInTypeDef @@ var "subst" @@ (Typing.typeConstraintLeft $ var "c"))
    (ref substInTypeDef @@ var "subst" @@ (Typing.typeConstraintRight $ var "c"))
    (Typing.typeConstraintComment $ var "c")

substituteInConstraintsDef :: TBinding (TypeSubst -> [TypeConstraint] -> [TypeConstraint])
substituteInConstraintsDef = define "substituteInConstraints" $
  doc "Apply a type substitution to a list of type constraints" $
  lambdas ["subst", "cs"] $ Lists.map (ref substituteInConstraintDef @@ var "subst") (var "cs")

substInContextDef :: TBinding (TypeSubst -> InferenceContext -> InferenceContext)
substInContextDef = define "substInContext" $
  doc "Apply a type substitution to an inference context" $
  lambdas ["subst", "cx"] $ Typing.inferenceContextWithDataTypes
    (var "cx")
    (Maps.map (ref substInTypeSchemeDef @@ var "subst") (Typing.inferenceContextDataTypes $ var "cx"))

substituteInTermDef :: TBinding (TermSubst -> Term -> Term)
substituteInTermDef = define "substituteInTerm" $
  doc "Apply a term substitution to a term" $
  "subst" ~> "term0" ~> lets [
    "s">: Typing.unTermSubst $ var "subst",
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "withLambda">: lambda "l" $ lets [
        "v">: Core.lambdaParameter $ var "l",
        "subst2">: Typing.termSubst $ Maps.remove (var "v") (var "s")] $
        Core.termFunction $ Core.functionLambda $
          Core.lambda (var "v") (Core.lambdaDomain $ var "l") (ref substituteInTermDef @@ var "subst2" @@ (Core.lambdaBody $ var "l")),
      "withLet">: lambda "lt" $ lets [
        "bindings">: Core.letBindings $ var "lt",
        "names">: Sets.fromList $ Lists.map (unaryFunction Core.bindingName) (var "bindings"),
        "subst2">: Typing.termSubst $ Maps.filterWithKey (lambdas ["k", "v"] $ Logic.not $ Sets.member (var "k") (var "names")) (var "s"),
        "rewriteBinding">: lambda "b" $ Core.binding
          (Core.bindingName $ var "b")
          (ref substituteInTermDef @@ var "subst2" @@ (Core.bindingTerm $ var "b"))
          (Core.bindingType $ var "b")] $
        Core.termLet $ Core.let_
          (Lists.map (var "rewriteBinding") (var "bindings"))
          (ref substituteInTermDef @@ var "subst2" @@ (Core.letBody $ var "lt"))] $
      cases _Term (var "term")
        (Just $ var "recurse" @@ var "term") [
        _Term_function>>: lambda "fun" $ cases _Function (var "fun")
          (Just $ var "recurse" @@ var "term") [
          _Function_lambda>>: "l" ~> var "withLambda" @@ var "l"],
        _Term_let>>: "l" ~> var "withLet" @@ var "l",
        _Term_variable>>: lambda "name" $ Maybes.maybe
          (var "recurse" @@ var "term")
          (lambda "sterm" $ var "sterm")
          (Maps.lookup (var "name") (var "s"))]] $
    ref Rewriting.rewriteTermDef @@ var "rewrite" @@ var "term0"

-- W: subst'
substInTypeDef :: TBinding (TypeSubst -> Type -> Type)
substInTypeDef = define "substInType" $
  doc "Apply a type substitution to a type" $
  "subst" ~> "typ0" ~> lets [
    "rewrite">: lambdas ["recurse", "typ"] $ cases _Type (var "typ") (Just $ var "recurse" @@ var "typ") [
      _Type_forall>>: lambda "lt" $ Maybes.maybe
        (var "recurse" @@ var "typ")
        (lambda "styp" $ Core.typeForall $ Core.forallType
          (Core.forallTypeParameter $ var "lt")
          (ref substInTypeDef
            @@ (var "removeVar" @@ (Core.forallTypeParameter $ var "lt"))
            @@ (Core.forallTypeBody $ var "lt")))
        (Maps.lookup (Core.forallTypeParameter $ var "lt") (Typing.unTypeSubst $ var "subst")),
      _Type_variable>>: lambda "v" $ Maybes.maybe
        (var "typ")
        (lambda "styp" $ var "styp")
        (Maps.lookup (var "v") (Typing.unTypeSubst $ var "subst"))],
    "removeVar">: lambdas ["v"] $ Typing.typeSubst $ Maps.remove (var "v") (Typing.unTypeSubst $ var "subst")] $
    (ref Rewriting.rewriteTypeDef) @@ var "rewrite" @@ var "typ0"

substInTypeSchemeDef :: TBinding (TypeSubst -> TypeScheme -> TypeScheme)
substInTypeSchemeDef = define "substInTypeScheme" $
  doc "Apply a type substitution to a type scheme" $
  lambdas ["subst", "ts"] $ Core.typeScheme
    (Core.typeSchemeVariables $ var "ts")
    (ref substInTypeDef @@ var "subst" @@ (Core.typeSchemeType $ var "ts"))

substTypesInTermDef :: TBinding (TypeSubst -> Term -> Term)
substTypesInTermDef = define "substTypesInTerm" $
  doc "Apply a type substitution to the type annotations within a term" $
  "subst" ~> "term0" ~> lets [
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "dflt">: var "recurse" @@ var "term",
      "forElimination">: lambda "elm" $ cases _Elimination (var "elm")
        (Just $ var "dflt") [
        _Elimination_product>>: "tp" ~> var "forTupleProjection" @@ var "tp"],
      "forFunction">: lambda "f" $ cases _Function (var "f")
        (Just $ var "dflt") [
        _Function_elimination>>: "e" ~> var "forElimination" @@ var "e",
        _Function_lambda>>: "l" ~> var "forLambda" @@ var "l"],
      "forLambda">: lambda "l" $ Core.termFunction $ Core.functionLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        (Maybes.map (ref substInTypeDef @@ var "subst") $ Core.lambdaDomain $ var "l")
        (ref substTypesInTermDef @@ var "subst" @@ (Core.lambdaBody $ var "l")),
      "forLet">: lambda "l" $ lets [
        "rewriteBinding">: lambda "b" $ Core.binding
          (Core.bindingName $ var "b")
          (ref substTypesInTermDef @@ var "subst" @@ (Core.bindingTerm $ var "b"))
          (Maybes.map (ref substInTypeSchemeDef @@ var "subst") (Core.bindingType $ var "b"))] $
        Core.termLet $ Core.let_
          (Lists.map (var "rewriteBinding") (Core.letBindings $ var "l"))
          (ref substTypesInTermDef @@ var "subst" @@ (Core.letBody $ var "l")),
      "forTupleProjection">: lambda "tp" $
        Core.termFunction $ Core.functionElimination $ Core.eliminationProduct $ Core.tupleProjection
          (Core.tupleProjectionArity $ var "tp")
          (Core.tupleProjectionIndex $ var "tp")
          (Maybes.map (lambda "types" $ Lists.map (ref substInTypeDef @@ var "subst") (var "types")) (Core.tupleProjectionDomain $ var "tp")),
      "forTypeApplication">: lambda "tt" $
         Core.termTypeApplication $ Core.typeApplicationTerm
           (ref substTypesInTermDef @@ var "subst" @@ (Core.typeApplicationTermBody $ var "tt"))
           (ref substInTypeDef @@ var "subst" @@ (Core.typeApplicationTermType $ var "tt")),
      "forTypeLambda">: lambda "ta" $ lets [
        "param">: Core.typeLambdaParameter $ var "ta",
        "subst2">: Typing.typeSubst $ Maps.remove (var "param") (Typing.unTypeSubst $ var "subst")] $
        Core.termTypeLambda $ Core.typeLambda
          (var "param")
          (ref substTypesInTermDef @@ var "subst2" @@ (Core.typeLambdaBody $ var "ta"))] $
      cases _Term (var "term")
        (Just $ var "dflt") [
        _Term_function>>: "f" ~> var "forFunction" @@ var "f",
        _Term_let>>: "l" ~> var "forLet" @@ var "l",
        _Term_typeApplication>>: "ta" ~> var "forTypeApplication" @@ var "ta",
        _Term_typeLambda>>: "tl" ~> var "forTypeLambda" @@ var "tl"]] $
    ref Rewriting.rewriteTermDef @@ var "rewrite" @@ var "term0"
