
module Hydra.Sources.Kernel.Terms.Substitution where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  composeTypeSubst, composeTypeSubstList, idTypeSubst, singletonTypeSubst,
  substituteInConstraint, substituteInConstraints, substInClassConstraints, substInContext, substituteInTerm,
  substInType, substInTypeScheme, substTypesInTerm)
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
import qualified Hydra.Dsl.Meta.Testing       as Testing
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


ns :: Namespace
ns = Namespace "hydra.substitution"

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns]
    kernelTypesNamespaces $
    Just ("Variable substitution in type and term expressions.")
  where
   elements = [
     toBinding composeTypeSubst,
     toBinding composeTypeSubstList,
     toBinding idTypeSubst,
     toBinding singletonTypeSubst,
     toBinding substituteInConstraint,
     toBinding substituteInConstraints,
     toBinding substInClassConstraints,
     toBinding substInContext,
     toBinding substituteInTerm,
     toBinding substInType,
     toBinding substInTypeScheme,
     toBinding substTypesInTerm]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

composeTypeSubst :: TBinding (TypeSubst -> TypeSubst -> TypeSubst)
composeTypeSubst = define "composeTypeSubst" $
  doc "Compose two type substitutions" $
  lambdas ["s1", "s2"] $ lets [
    "isExtra">: lambdas ["k", "v"] $ Maybes.isNothing (Maps.lookup (var "k") (Typing.unTypeSubst $ var "s1")),
    "withExtra">: Maps.filterWithKey (var "isExtra") (Typing.unTypeSubst $ var "s2")] $
    Typing.typeSubst $ Maps.union (var "withExtra") $ Maps.map (substInType @@ var "s2") $ Typing.unTypeSubst $ var "s1"

composeTypeSubstList :: TBinding ([TypeSubst] -> TypeSubst)
composeTypeSubstList = define "composeTypeSubstList" $
  doc "Compose a list of type substitutions" $
  Phantoms.fold (composeTypeSubst) @@ idTypeSubst

idTypeSubst :: TBinding TypeSubst
idTypeSubst = define "idTypeSubst" $
  doc "The identity type substitution" $
  Typing.typeSubst Maps.empty

singletonTypeSubst :: TBinding (Name -> Type -> TypeSubst)
singletonTypeSubst = define "singletonTypeSubst" $
  doc "Create a type substitution with a single variable mapping" $
  lambdas ["v", "t"] $ Typing.typeSubst $ Maps.singleton (var "v") (var "t")

substituteInConstraint :: TBinding (TypeSubst -> TypeConstraint -> TypeConstraint)
substituteInConstraint = define "substituteInConstraint" $
  doc "Apply a type substitution to a type constraint" $
  lambdas ["subst", "c"] $ Typing.typeConstraint
    (substInType @@ var "subst" @@ (Typing.typeConstraintLeft $ var "c"))
    (substInType @@ var "subst" @@ (Typing.typeConstraintRight $ var "c"))
    (Typing.typeConstraintComment $ var "c")

substituteInConstraints :: TBinding (TypeSubst -> [TypeConstraint] -> [TypeConstraint])
substituteInConstraints = define "substituteInConstraints" $
  doc "Apply a type substitution to a list of type constraints" $
  lambdas ["subst", "cs"] $ Lists.map (substituteInConstraint @@ var "subst") (var "cs")

-- | Apply a type substitution to a map of class constraints.
-- When a type variable is mapped to another type variable, the constraint is transferred to the new variable.
-- When a type variable is mapped to a non-variable type, the constraint is dropped (the type is now concrete).
substInClassConstraints :: TBinding (TypeSubst -> M.Map Name TypeVariableMetadata -> M.Map Name TypeVariableMetadata)
substInClassConstraints = define "substInClassConstraints" $
  doc "Apply a type substitution to class constraints, renaming keys as needed" $
  "subst" ~> "constraints" ~>
  "substMap" <~ Typing.unTypeSubst (var "subst") $
  -- For each (varName, metadata) in constraints:
  -- 1. Look up varName in the substitution
  -- 2. If found and it maps to TypeVariable newName, add (newName, metadata) to result
  -- 3. If not found, keep (varName, metadata) in result
  -- 4. If found but maps to a non-variable type, drop the constraint
  Lists.foldl
    ("acc" ~> "pair" ~>
      "varName" <~ Pairs.first (var "pair") $
      "metadata" <~ Pairs.second (var "pair") $
      Maybes.maybe
        -- Not in substitution: keep original
        (Maps.insert (var "varName") (var "metadata") (var "acc"))
        -- In substitution: check if mapped to a type variable
        ("targetType" ~>
          cases _Type (var "targetType") (Just $ var "acc") [
            _Type_variable>>: "newVarName" ~>
              -- Mapped to a variable: use the new variable name
              Maybes.maybe
                -- No existing entry for newVarName: just insert
                (Maps.insert (var "newVarName") (var "metadata") (var "acc"))
                -- Existing entry: merge the class sets
                ("existing" ~>
                  "merged" <~ Core.typeVariableMetadata (Sets.union (Core.typeVariableMetadataClasses $ var "existing") (Core.typeVariableMetadataClasses $ var "metadata")) $
                  Maps.insert (var "newVarName") (var "merged") (var "acc"))
                (Maps.lookup (var "newVarName") (var "acc"))])
        (Maps.lookup (var "varName") (var "substMap")))
    Maps.empty
    (Maps.toList $ var "constraints")

substInContext :: TBinding (TypeSubst -> InferenceContext -> InferenceContext)
substInContext = define "substInContext" $
  doc "Apply a type substitution to an inference context" $
  lambdas ["subst", "cx"] $
    "newDataTypes" <~ Maps.map (substInTypeScheme @@ var "subst") (Typing.inferenceContextDataTypes $ var "cx") $
    "newClassConstraints" <~ substInClassConstraints @@ var "subst" @@ (Typing.inferenceContextClassConstraints $ var "cx") $
    Typing.inferenceContext
      (Typing.inferenceContextSchemaTypes $ var "cx")
      (Typing.inferenceContextPrimitiveTypes $ var "cx")
      (var "newDataTypes")
      (var "newClassConstraints")
      (Typing.inferenceContextDebug $ var "cx")

substituteInTerm :: TBinding (TermSubst -> Term -> Term)
substituteInTerm = define "substituteInTerm" $
  doc "Apply a term substitution to a term" $
  "subst" ~> "term0" ~> lets [
    "s">: Typing.unTermSubst $ var "subst",
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "withLambda">: lambda "l" $ lets [
        "v">: Core.lambdaParameter $ var "l",
        "subst2">: Typing.termSubst $ Maps.delete (var "v") (var "s")] $
        Core.termFunction $ Core.functionLambda $
          Core.lambda (var "v") (Core.lambdaDomain $ var "l") (substituteInTerm @@ var "subst2" @@ (Core.lambdaBody $ var "l")),
      "withLet">: lambda "lt" $ lets [
        "bindings">: Core.letBindings $ var "lt",
        "names">: Sets.fromList $ Lists.map (unaryFunction Core.bindingName) (var "bindings"),
        "subst2">: Typing.termSubst $ Maps.filterWithKey (lambdas ["k", "v"] $ Logic.not $ Sets.member (var "k") (var "names")) (var "s"),
        "rewriteBinding">: lambda "b" $ Core.binding
          (Core.bindingName $ var "b")
          (substituteInTerm @@ var "subst2" @@ (Core.bindingTerm $ var "b"))
          (Core.bindingType $ var "b")] $
        Core.termLet $ Core.let_
          (Lists.map (var "rewriteBinding") (var "bindings"))
          (substituteInTerm @@ var "subst2" @@ (Core.letBody $ var "lt"))] $
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
    Rewriting.rewriteTerm @@ var "rewrite" @@ var "term0"

-- W: subst'
substInType :: TBinding (TypeSubst -> Type -> Type)
substInType = define "substInType" $
  doc "Apply a type substitution to a type" $
  "subst" ~> "typ0" ~> lets [
    "rewrite">: lambdas ["recurse", "typ"] $ cases _Type (var "typ") (Just $ var "recurse" @@ var "typ") [
      _Type_forall>>: lambda "lt" $ Maybes.maybe
        (var "recurse" @@ var "typ")
        (lambda "styp" $ Core.typeForall $ Core.forallType
          (Core.forallTypeParameter $ var "lt")
          (substInType
            @@ (var "removeVar" @@ (Core.forallTypeParameter $ var "lt"))
            @@ (Core.forallTypeBody $ var "lt")))
        (Maps.lookup (Core.forallTypeParameter $ var "lt") (Typing.unTypeSubst $ var "subst")),
      _Type_variable>>: lambda "v" $ Maybes.maybe
        (var "typ")
        (lambda "styp" $ var "styp")
        (Maps.lookup (var "v") (Typing.unTypeSubst $ var "subst"))],
    "removeVar">: lambdas ["v"] $ Typing.typeSubst $ Maps.delete (var "v") (Typing.unTypeSubst $ var "subst")] $
    (Rewriting.rewriteType) @@ var "rewrite" @@ var "typ0"

substInTypeScheme :: TBinding (TypeSubst -> TypeScheme -> TypeScheme)
substInTypeScheme = define "substInTypeScheme" $
  doc "Apply a type substitution to a type scheme" $
  lambdas ["subst", "ts"] $ Core.typeScheme
    (Core.typeSchemeVariables $ var "ts")
    (substInType @@ var "subst" @@ (Core.typeSchemeType $ var "ts"))
    -- Also apply the substitution to the constraints
    (Maybes.map (substInClassConstraints @@ var "subst") (Core.typeSchemeConstraints $ var "ts"))

substTypesInTerm :: TBinding (TypeSubst -> Term -> Term)
substTypesInTerm = define "substTypesInTerm" $
  doc "Apply a type substitution to the type annotations within a term" $
  "subst" ~> "term0" ~> lets [
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "dflt">: var "recurse" @@ var "term",
      "forFunction">: lambda "f" $ cases _Function (var "f")
        (Just $ var "dflt") [
        _Function_elimination>>: "e" ~> var "dflt",
        _Function_lambda>>: "l" ~> var "forLambda" @@ var "l"],
      "forLambda">: lambda "l" $ Core.termFunction $ Core.functionLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        (Maybes.map (substInType @@ var "subst") $ Core.lambdaDomain $ var "l")
        (substTypesInTerm @@ var "subst" @@ (Core.lambdaBody $ var "l")),
      "forLet">: lambda "l" $ lets [
        "rewriteBinding">: lambda "b" $ Core.binding
          (Core.bindingName $ var "b")
          (substTypesInTerm @@ var "subst" @@ (Core.bindingTerm $ var "b"))
          (Maybes.map (substInTypeScheme @@ var "subst") (Core.bindingType $ var "b"))] $
        Core.termLet $ Core.let_
          (Lists.map (var "rewriteBinding") (Core.letBindings $ var "l"))
          (substTypesInTerm @@ var "subst" @@ (Core.letBody $ var "l")),
      "forTypeApplication">: lambda "tt" $
         Core.termTypeApplication $ Core.typeApplicationTerm
           (substTypesInTerm @@ var "subst" @@ (Core.typeApplicationTermBody $ var "tt"))
           (substInType @@ var "subst" @@ (Core.typeApplicationTermType $ var "tt")),
      "forTypeLambda">: lambda "ta" $ lets [
        "param">: Core.typeLambdaParameter $ var "ta",
        "subst2">: Typing.typeSubst $ Maps.delete (var "param") (Typing.unTypeSubst $ var "subst")] $
        Core.termTypeLambda $ Core.typeLambda
          (var "param")
          (substTypesInTerm @@ var "subst2" @@ (Core.typeLambdaBody $ var "ta"))] $
      cases _Term (var "term")
        (Just $ var "dflt") [
        _Term_function>>: "f" ~> var "forFunction" @@ var "f",
        _Term_let>>: "l" ~> var "forLet" @@ var "l",
        _Term_typeApplication>>: "ta" ~> var "forTypeApplication" @@ var "ta",
        _Term_typeLambda>>: "tl" ~> var "forTypeLambda" @@ var "tl"]] $
    Rewriting.rewriteTerm @@ var "rewrite" @@ var "term0"
