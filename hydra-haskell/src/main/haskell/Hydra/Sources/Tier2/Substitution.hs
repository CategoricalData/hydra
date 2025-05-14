{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Substitution where

-- Standard Tier-2 imports
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Dsl.Phantoms        as Phantoms
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

import Hydra.Dsl.Compute
import qualified Hydra.Dsl.Typing as Typing
import Hydra.Sources.Tier2.Rewriting
import Hydra.Sources.Libraries


substitutionDefinition :: String -> TTerm a -> TElement a
substitutionDefinition = definitionInModule hydraSubstitutionModule

hydraSubstitutionModule :: Module
hydraSubstitutionModule = Module (Namespace "hydra.substitution") elements
    [hydraRewritingModule] [hydraCodersModule, hydraMantleModule, hydraTypingModule] $
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

composeTypeSubstDef :: TElement (TypeSubst -> TypeSubst -> TypeSubst)
composeTypeSubstDef = substitutionDefinition "composeTypeSubst" $
  lambdas ["s1", "s2"] $ lets [
    "isExtra">: lambdas ["k", "v"] $ Optionals.isNothing (Maps.lookup (var "k") (Typing.unTypeSubst @@ var "s1")),
    "withExtra">: Maps.filterWithKey (var "isExtra") (Typing.unTypeSubst @@ var "s2")] $
    Typing.typeSubst $ Maps.union (var "withExtra") $ Maps.map (ref substInTypeDef @@ var "s2") $ Typing.unTypeSubst @@ var "s1"

composeTypeSubstListDef :: TElement ([TypeSubst] -> TypeSubst)
composeTypeSubstListDef = substitutionDefinition "composeTypeSubstList" $
  Phantoms.fold (ref composeTypeSubstDef) @@ ref idTypeSubstDef

idTypeSubstDef :: TElement TypeSubst
idTypeSubstDef = substitutionDefinition "idTypeSubst" $
  Typing.typeSubst Maps.empty

singletonTypeSubstDef :: TElement (Name -> Type -> TypeSubst)
singletonTypeSubstDef = substitutionDefinition "singletonTypeSubst" $
  lambdas ["v", "t"] $ Typing.typeSubst $ Maps.singleton (var "v") (var "t")

substituteInConstraintDef :: TElement (TypeSubst -> TypeConstraint -> TypeConstraint)
substituteInConstraintDef = substitutionDefinition "substituteInConstraint" $
  lambdas ["subst", "c"] $ Typing.typeConstraint
    (ref substInTypeDef @@ var "subst" @@ (Typing.typeConstraintLeft @@ var "c"))
    (ref substInTypeDef @@ var "subst" @@ (Typing.typeConstraintRight @@ var "c"))
    (Typing.typeConstraintComment @@ var "c")

substituteInConstraintsDef :: TElement (TypeSubst -> [TypeConstraint] -> [TypeConstraint])
substituteInConstraintsDef = substitutionDefinition "substituteInConstraints" $
  lambdas ["subst", "cs"] $ Lists.map (ref substituteInConstraintDef @@ var "subst") (var "cs")

substInContextDef :: TElement (TypeSubst -> InferenceContext -> InferenceContext)
substInContextDef = substitutionDefinition "substInContext" $
  lambdas ["subst", "cx"] $ Typing.inferenceContextWithDataTypes
    (Maps.map (ref substInTypeSchemeDef @@ var "subst") (Typing.inferenceContextDataTypes @@ var "cx"))
    (var "cx")

substituteInTermDef :: TElement (TermSubst -> Term -> Term)
substituteInTermDef = substitutionDefinition "substituteInTerm" $
  lambda "subst" $ lets [
    "s">: Typing.unTermSubst @@ var "subst",
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "withLambda">: lambda "l" $ lets [
        "v">: Core.lambdaParameter @@ var "l",
        "subst2">: Typing.termSubst $ Maps.remove (var "v") (var "s")] $
        Core.termFunction $ Core.functionLambda $
          Core.lambda (var "v") (Core.lambdaDomain @@ var "l") (ref substituteInTermDef @@ var "subst2" @@ (Core.lambdaBody @@ var "l")),
      "withLet">: lambda "lt" $ lets [
        "bindings">: Core.letBindings @@ var "lt",
        "names">: Sets.fromList $ Lists.map Core.letBindingName (var "bindings"),
        "subst2">: Typing.termSubst $ Maps.filterWithKey (lambdas ["k", "v"] $ Logic.not $ Sets.contains (var "k") (var "names")) (var "s"),
        "rewriteBinding">: lambda "b" $ Core.letBinding
          (Core.letBindingName @@ var "b")
          (ref substituteInTermDef @@ var "subst2" @@ (Core.letBindingTerm @@ var "b"))
          (Core.letBindingType @@ var "b")] $
        Core.termLet $ Core.letExpression
          (Lists.map (var "rewriteBinding") (var "bindings"))
          (ref substituteInTermDef @@ var "subst2" @@ (Core.letEnvironment @@ var "lt"))] $
      cases _Term (var "term")
        (Just $ var "recurse" @@ var "term") [
        _Term_function>>: lambda "fun" $ cases _Function (var "fun")
          (Just $ var "recurse" @@ var "term") [
          _Function_lambda>>: var "withLambda"],
        _Term_let>>: var "withLet",
        _Term_variable>>: lambda "name" $ Optionals.maybe
          (var "recurse" @@ var "term")
          (lambda "sterm" $ var "sterm")
          (Maps.lookup (var "name") (var "s"))]] $
    ref rewriteTermDef @@ var "rewrite"

substInTypeDef :: TElement (TypeSubst -> Type -> Type)
substInTypeDef = substitutionDefinition "substInType" $
  lambda "subst" $ lets [
    "rewrite">: lambdas ["recurse", "typ"] $ cases _Type (var "typ") (Just $ var "recurse" @@ var "typ") [
      _Type_forall>>: lambda "lt" $ Optionals.maybe
        (var "recurse" @@ var "typ")
        (lambda "styp" $ Core.typeLambda $ Core.forallType
          (Core.forallTypeParameter @@ var "lt")
          (ref substInTypeDef
            @@ (var "removeVar" @@ (Core.forallTypeParameter @@ var "lt"))
            @@ (Core.forallTypeBody @@ var "lt")))
        (Maps.lookup (Core.forallTypeParameter @@ var "lt") (Typing.unTypeSubst @@ var "subst")),
      _Type_variable>>: lambda "v" $ Optionals.maybe
        (var "typ")
        (lambda "styp" $ var "styp")
        (Maps.lookup (var "v") (Typing.unTypeSubst @@ var "subst"))],
    "removeVar">: lambdas ["v"] $ Typing.typeSubst $ Maps.remove (var "v") (Typing.unTypeSubst @@ var "subst")] $
    (ref rewriteTypeDef) @@ var "rewrite"

substInTypeSchemeDef :: TElement (TypeSubst -> TypeScheme -> TypeScheme)
substInTypeSchemeDef = substitutionDefinition "substInTypeScheme" $
  lambdas ["subst", "ts"] $ Core.typeScheme
    (Core.typeSchemeVariables @@ var "ts")
    (ref substInTypeDef @@ var "subst" @@ (Core.typeSchemeType @@ var "ts"))

substTypesInTermDef :: TElement (TypeSubst -> Term -> Term)
substTypesInTermDef = substitutionDefinition "substTypesInTerm" $
  lambda "subst" $ lets [
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "forElimination">: lambda "elm" $ cases _Elimination (var "elm")
        (Just $ var "recurse" @@ var "term") [
        _Elimination_product>>: var "forTupleProjection"],
      "forFunction">: lambda "f" $ cases _Function (var "f")
        -- TODO: injections and case statements need a domain field as well, similar to lambdas
        (Just $ var "recurse" @@ var "term") [
        _Function_elimination>>: var "forElimination",
        _Function_lambda>>: var "forLambda"],
      "forLambda">: lambda "l" $ var "recurse" @@ (Core.termFunction $ Core.functionLambda $ Core.lambda
        (Core.lambdaParameter @@ var "l")
        (Optionals.map (ref substInTypeDef @@ var "subst") $ Core.lambdaDomain @@ var "l")
        (Core.lambdaBody @@ var "l")),
      "forLet">: lambda "l" $ lets [
        "rewriteBinding">: lambda "b" $ Core.letBinding
          (Core.letBindingName @@ var "b")
          (Core.letBindingTerm @@ var "b")
          (Optionals.map (ref substInTypeSchemeDef @@ var "subst") (Core.letBindingType @@ var "b"))] $
        var "recurse" @@ (Core.termLet $ Core.letExpression
          (Lists.map (var "rewriteBinding") (Core.letBindings @@ var "l"))
          (Core.letEnvironment @@ var "l")),
      "forTupleProjection">: lambda "tp" $ var "recurse" @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationProduct $ Core.tupleProjection
        (Core.tupleProjectionArity @@ var "tp")
        (Core.tupleProjectionIndex @@ var "tp")
        (Optionals.map (lambda "types" $ Lists.map (ref substInTypeDef @@ var "subst") (var "types")) (Core.tupleProjectionDomain @@ var "tp"))),
      "forTypeAbstraction">: lambda "ta" $ lets [
        "param">: Core.typeAbstractionParameter @@ var "ta",
        "subst2">: Typing.typeSubst $ Maps.remove (var "param") (Typing.unTypeSubst @@ var "subst")] $
        Core.termTypeAbstraction $ Core.typeAbstraction
          (var "param")
          (ref substTypesInTermDef @@ var "subst2" @@ (Core.typeAbstractionBody @@ var "ta"))] $
      cases _Term (var "term") (Just $ var "recurse" @@ var "term") [
        _Term_function>>: var "forFunction",
        _Term_let>>: var "forLet",
        _Term_typeAbstraction>>: var "forTypeAbstraction",
        _Term_typeApplication>>: lambda "tt" $ var "recurse" @@ (Core.termTypeApplication $ Core.typedTerm
          (Core.typedTermTerm @@ var "tt")
          (ref substInTypeDef @@ var "subst" @@ (Core.typedTermType @@ var "tt")))]] $
    ref rewriteTermDef @@ var "rewrite"
