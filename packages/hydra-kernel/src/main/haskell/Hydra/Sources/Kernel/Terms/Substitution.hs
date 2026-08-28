{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Substitution where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  composeTypeSubst, composeTypeSubstNonEmpty, composeTypeSubstList, idTypeSubst, singletonTypeSubst, substituteInBinding,
  substituteInConstraint, substituteInConstraints, substInClassConstraints, substInContext, substituteInTerm,
  substInType, substInTypeNonEmpty, substInTypeScheme, substTypesInTerm)
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Ordering as Ordering
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Classes as Classes
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Variables as Variables


ns :: ModuleName
ns = ModuleName "hydra.substitution"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Classes.ns, Rewriting.ns, Variables.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just ("Variable substitution in type and term expressions."))}
  where
   definitions = [
     toDefinition composeTypeSubst,
     toDefinition composeTypeSubstList,
     toDefinition composeTypeSubstNonEmpty,
     toDefinition idTypeSubst,
     toDefinition singletonTypeSubst,
     toDefinition substInClassConstraints,
     toDefinition substInContext,
     toDefinition substInType,
     toDefinition substInTypeNonEmpty,
     toDefinition substInTypeScheme,
     toDefinition substTypesInTerm,
     toDefinition substituteInBinding,
     toDefinition substituteInConstraint,
     toDefinition substituteInConstraints,
     toDefinition substituteInTerm]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

composeTypeSubst :: TypedTermDefinition (TypeSubst -> TypeSubst -> TypeSubst)
composeTypeSubst = define "composeTypeSubst" $
  doc "Compose two type substitutions" $
  lambdas ["s1", "s2"] $
    -- Short-circuit: if s1 is empty, return s2
    Logic.ifElse (Maps.isEmpty $ Typing.unTypeSubst $ var "s1")
      (var "s2") $
    -- Short-circuit: if s2 is empty, return s1
    Logic.ifElse (Maps.isEmpty $ Typing.unTypeSubst $ var "s2")
      (var "s1") $
    -- Otherwise, compose normally
    composeTypeSubstNonEmpty @@ var "s1" @@ var "s2"

-- | Helper for composeTypeSubst when both substitutions are non-empty
composeTypeSubstList :: TypedTermDefinition ([TypeSubst] -> TypeSubst)
composeTypeSubstList = define "composeTypeSubstList" $
  doc "Compose a list of type substitutions" $
  Phantoms.fold (composeTypeSubst) @@ idTypeSubst

-- | Helper for composeTypeSubst when both substitutions are non-empty
composeTypeSubstNonEmpty :: TypedTermDefinition (TypeSubst -> TypeSubst -> TypeSubst)
composeTypeSubstNonEmpty = define "composeTypeSubstNonEmpty" $
  doc "Compose two non-empty type substitutions (internal helper)" $
  lambdas ["s1", "s2"] $ lets [
    "isExtra">: lambdas ["k", "v"] $ Optionals.isNone (Maps.lookup (var "k") (Typing.unTypeSubst $ var "s1")),
    "withExtra">: Maps.filterWithKey (var "isExtra") (Typing.unTypeSubst $ var "s2")] $
    Typing.typeSubst $ Maps.union (var "withExtra") $ Maps.map (substInType @@ var "s2") $ Typing.unTypeSubst $ var "s1"

idTypeSubst :: TypedTermDefinition TypeSubst
idTypeSubst = define "idTypeSubst" $
  doc "The identity type substitution" $
  Typing.typeSubst Maps.empty

singletonTypeSubst :: TypedTermDefinition (Name -> Type -> TypeSubst)
singletonTypeSubst = define "singletonTypeSubst" $
  doc "Create a type substitution with a single variable mapping" $
  lambdas ["v", "t"] $ Typing.typeSubst $ Maps.singleton (var "v") (var "t")

-- | Apply a type substitution to a map of class constraints.
-- When a type variable is mapped to another type variable, or to a type with genuine free type
-- variables (see "isGenuineVarName" below), the constraint is re-keyed/propagated to those
-- variables only. When a type variable is mapped to a type with no genuine free variables, the
-- variable is dead: any class it satisfies (e.g. ordering/equality, which are universal) is
-- dropped, since a later Inference.dischargeClassConstraints check against the same resolved type
-- could only reach the same verdict -- dropping precomputes that inevitable success. Any class it
-- does NOT satisfy is retained under the ORIGINAL variable name so discharge still rejects it (see
-- Classes.classIsSatisfiedByType and Inference.dischargeClassConstraints); this is the point at
-- which entailment becomes checkable, and a constrained variable resolving to a type that fails
-- its constraint must remain visible to that check, not silently vanish.
--
-- Pruning dead-and-satisfied entries here (rather than leaving them for a later pass) matters
-- because deadness is only decidable at the substitution moment: a retained entry's key is by
-- construction absent from every later substitution, so it is indistinguishable from a live
-- variable to any downstream code, and would otherwise be carried and re-walked for the rest of
-- inference. Left unpruned, and combined with "genuine free variable" propagation additionally
-- re-keying onto qualified nominal type names (which are never read by any consumer -- substs,
-- generalize, and discharge all ignore them), constraint maps grow in proportion to term size and
-- are rebuilt in full on every substitution, making whole-module constraint propagation quadratic
-- in term size (#702). Pruning keeps the map bounded by the number of currently-live constrained
-- variables, which is small (the polymorphic arity of the binding being inferred).
substInClassConstraints :: TypedTermDefinition (TypeSubst -> M.Map Name TypeVariableConstraints -> M.Map Name TypeVariableConstraints)
substInClassConstraints = define "substInClassConstraints" $
  doc "Apply a type substitution to class constraints, propagating to free variables or discharging/retaining on concrete resolution" $
  "subst" ~> "constraints" ~>
  "substMap" <~ Typing.unTypeSubst (var "subst") $
  Logic.ifElse (Logic.or (Maps.isEmpty (var "substMap" :: TypedTerm (M.Map Name Type))) (Maps.isEmpty (var "constraints" :: TypedTerm (M.Map Name TypeVariableConstraints))))
    (var "constraints" :: TypedTerm (M.Map Name TypeVariableConstraints)) $
  -- A qualified name (e.g. "hydra.core.Term") is a nominal type reference, not a genuine type
  -- variable; only single-part names are genuine (mirrors generalize's own isTypeVarName test).
  "isGenuineVarName" <~ ("name" ~>
    Ordering.lte (Lists.length $ Strings.splitOn (string ".") (Core.unName $ var "name")) (int32 1)) $
  -- Helper to insert a constraint, merging with existing if present
  "insertOrMerge" <~ ("varName" ~> "metadata" ~> "acc" ~>
    Optionals.match (Maps.lookup (var "varName" :: TypedTerm Name) (var "acc")) (Maps.insert (var "varName" :: TypedTerm Name) (var "metadata") (var "acc")) ("existing" ~>
        "merged" <~ Core.typeVariableConstraints (Sets.union (Core.typeVariableConstraintsClasses $ var "existing") (Core.typeVariableConstraintsClasses $ var "metadata")) $
        Maps.insert (var "varName" :: TypedTerm Name) (var "merged") (var "acc"))) $
  -- For each (varName, metadata) in constraints:
  -- 1. Look up varName in the substitution
  -- 2. If not found, keep (varName, metadata) in result
  -- 3. If found and the target type has genuine free variables, re-key the constraint to those
  --    variables only (dropping any vacuous nominal-name entries the naive free-variable set would add)
  -- 4. If found and the target type has NO genuine free variables (the variable is dead), keep only
  --    the classes it does NOT satisfy, retained under the original varName; drop the rest
  Lists.foldl
    ("acc" ~> "pair" ~>
      "varName" <~ Pairs.first (var "pair") $
      "metadata" <~ Pairs.second (var "pair") $
      Optionals.match
        (Maps.lookup (var "varName" :: TypedTerm Name) (var "substMap"))
        -- Not in substitution: keep original
        (var "insertOrMerge" @@ var "varName" @@ var "metadata" @@ var "acc")
        -- In substitution: re-key to genuine free variables, or discharge/retain if dead
        ("targetType" ~>
          "genuineVars" <~ Lists.filter (var "isGenuineVarName") (Sets.toList (Variables.freeVariablesInType @@ var "targetType")) $
          Logic.ifElse (Logic.not $ Lists.isEmpty (var "genuineVars"))
            (Lists.foldl
              ("acc2" ~> "freeVar" ~> var "insertOrMerge" @@ var "freeVar" @@ var "metadata" @@ var "acc2")
              (var "acc")
              (var "genuineVars"))
            -- Dead variable: drop classes the target type already satisfies (discharge would
            -- pass them anyway); retain only the unsatisfied ones, under the original name, so
            -- discharge still rejects them.
            ("unsatisfied" <~ Sets.filter
              ("c" ~> Logic.not $ Classes.classIsSatisfiedByType @@ (match _TypeClassConstraint (var "c") Nothing [_TypeClassConstraint_simple>>: "n" ~> var "n"]) @@ var "targetType")
              (Core.typeVariableConstraintsClasses $ var "metadata") $
             Logic.ifElse (Sets.isEmpty (var "unsatisfied" :: TypedTerm (S.Set TypeClassConstraint)))
               (var "acc")
               (var "insertOrMerge" @@ var "varName" @@ (Core.typeVariableConstraints $ var "unsatisfied") @@ var "acc"))))
    (Maps.empty :: TypedTerm (M.Map Name TypeVariableConstraints))
    (Maps.toList $ (var "constraints" :: TypedTerm (M.Map Name TypeVariableConstraints)))

substInContext :: TypedTermDefinition (TypeSubst -> Graph -> Graph)
substInContext = define "substInContext" $
  doc "Apply a type substitution to a graph's bound types and class constraints" $
  lambdas ["subst", "cx"] $
    "newBoundTypes" <~ Maps.map (substInTypeScheme @@ var "subst") (Graph.graphBoundTypes $ var "cx") $
    "newClassConstraints" <~ substInClassConstraints @@ var "subst" @@ (Graph.graphClassConstraints $ var "cx") $
    "cx2" <~ Graph.graphWithBoundTypes (var "cx") (var "newBoundTypes") $
    Graph.graphWithClassConstraints (var "cx2") (var "newClassConstraints")

-- W: subst'
substInType :: TypedTermDefinition (TypeSubst -> Type -> Type)
substInType = define "substInType" $
  doc "Apply a type substitution to a type" $
  "subst" ~> "typ0" ~>
    -- Short-circuit: if substitution is empty, return type unchanged
    Logic.ifElse (Maps.isEmpty $ Typing.unTypeSubst $ var "subst")
      (var "typ0") $
    -- Otherwise, apply the substitution
    substInTypeNonEmpty @@ var "subst" @@ var "typ0"

-- | Helper for substInType when substitution is non-empty
-- | Helper for substInType when substitution is non-empty
substInTypeNonEmpty :: TypedTermDefinition (TypeSubst -> Type -> Type)
substInTypeNonEmpty = define "substInTypeNonEmpty" $
  doc "Apply a non-empty type substitution to a type (internal helper)" $
  "subst" ~> "typ0" ~>
    lets [
      "rewrite">: lambdas ["recurse", "typ"] $ match _Type (var "typ") (Just $ var "recurse" @@ var "typ") [
        _Type_forall>>: lambda "lt" $ Optionals.match (Maps.lookup (Core.forallTypeParameter $ var "lt") (Typing.unTypeSubst $ var "subst")) (var "recurse" @@ var "typ") (lambda "styp" $ Core.typeForall $ Core.forallType
            (Core.forallTypeParameter $ var "lt")
            (substInType
              @@ (var "removeVar" @@ (Core.forallTypeParameter $ var "lt"))
              @@ (Core.forallTypeBody $ var "lt"))),
        _Type_variable>>: lambda "v" $ Optionals.match (Maps.lookup (var "v") (Typing.unTypeSubst $ var "subst")) (var "typ") (lambda "styp" $ var "styp")],
      "removeVar">: lambdas ["v"] $ Typing.typeSubst $ Maps.delete (var "v") (Typing.unTypeSubst $ var "subst")] $
      (Rewriting.rewriteType) @@ var "rewrite" @@ var "typ0"

substInTypeScheme :: TypedTermDefinition (TypeSubst -> TypeScheme -> TypeScheme)
substInTypeScheme = define "substInTypeScheme" $
  doc ("Apply a type substitution to a type scheme. The scheme's quantifier"
    <> " variables shadow the substitution: any name in typeSchemeVariables is"
    <> " removed from subst before substituting into the body and constraints."
    <> " Without this, a substitution like {t0 -> Foo} applied to"
    <> " `forall [t0]. t0 -> t0` would incorrectly replace the bound t0.") $
  lambdas ["subst", "ts"] $ lets [
    "scopedSubst">: Typing.typeSubst $ Lists.foldl
      (lambdas ["m", "v"] $ Maps.delete (var "v" :: TypedTerm Name) (var "m"))
      (Typing.unTypeSubst $ var "subst")
      (Core.typeSchemeVariables $ var "ts")] $
    Core.typeScheme
      (Core.typeSchemeVariables $ var "ts")
      (substInType @@ var "scopedSubst" @@ (Core.typeSchemeBody $ var "ts"))
      -- Also apply the substitution to the constraints
      (substInClassConstraints @@ var "scopedSubst" @@ (Core.typeSchemeConstraints $ var "ts"))

substTypesInTerm :: TypedTermDefinition (TypeSubst -> Term -> Term)
substTypesInTerm = define "substTypesInTerm" $
  doc "Apply a type substitution to the type annotations within a term" $
  "subst" ~> "term0" ~> lets [
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "dflt">: var "recurse" @@ var "term",
      "forLambda">: lambda "l" $ Core.termLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        (Optionals.map (substInType @@ var "subst") $ Core.lambdaDomain $ var "l")
        (substTypesInTerm @@ var "subst" @@ (Core.lambdaBody $ var "l")),
      "forLet">: lambda "l" $ lets [
        "rewriteBinding">: lambda "b" $ Core.binding
          (Core.bindingName $ var "b")
          (substTypesInTerm @@ var "subst" @@ (Core.bindingTerm $ var "b"))
          (Optionals.map (substInTypeScheme @@ var "subst") (Core.bindingTypeScheme $ var "b"))] $
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
      match _Term (var "term")
        (Just $ var "dflt") [
        _Term_lambda>>: "l" ~> var "forLambda" @@ var "l",
        _Term_let>>: "l" ~> var "forLet" @@ var "l",
        _Term_typeApplication>>: "ta" ~> var "forTypeApplication" @@ var "ta",
        _Term_typeLambda>>: "tl" ~> var "forTypeLambda" @@ var "tl"]] $
    Rewriting.rewriteTerm @@ var "rewrite" @@ var "term0"
substituteInBinding :: TypedTermDefinition (TermSubst -> Binding -> Binding)
substituteInBinding = define "substituteInBinding" $
  doc "Apply a term substitution to a binding" $
  "subst" ~> "b" ~> Core.bindingWithTerm (var "b") (substituteInTerm @@ var "subst" @@ (Core.bindingTerm $ var "b"))
substituteInConstraint :: TypedTermDefinition (TypeSubst -> TypeConstraint -> TypeConstraint)
substituteInConstraint = define "substituteInConstraint" $
  doc "Apply a type substitution to a type constraint" $
  lambdas ["subst", "c"] $ Typing.typeConstraint
    (substInType @@ var "subst" @@ (Typing.typeConstraintLeft $ var "c"))
    (substInType @@ var "subst" @@ (Typing.typeConstraintRight $ var "c"))
    (Typing.typeConstraintComment $ var "c")

substituteInConstraints :: TypedTermDefinition (TypeSubst -> [TypeConstraint] -> [TypeConstraint])
substituteInConstraints = define "substituteInConstraints" $
  doc "Apply a type substitution to a list of type constraints" $
  lambdas ["subst", "cs"] $ Lists.map (substituteInConstraint @@ var "subst") (var "cs")

-- | Apply a type substitution to a map of class constraints.
-- When a type variable is mapped to another type variable, the constraint is transferred to the new variable.
-- When a type variable is mapped to a complex type, the constraint is propagated to all free variables in that type.

substituteInTerm :: TypedTermDefinition (TermSubst -> Term -> Term)
substituteInTerm = define "substituteInTerm" $
  doc "Apply a term substitution to a term" $
  "subst" ~> "term0" ~> lets [
    "s">: Typing.unTermSubst $ var "subst",
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "withLambda">: lambda "l" $ lets [
        "v">: Core.lambdaParameter $ var "l",
        "subst2">: Typing.termSubst $ Maps.delete (var "v") (var "s")] $
        Core.termLambda $
          Core.lambda (var "v") (Core.lambdaDomain $ var "l") (substituteInTerm @@ var "subst2" @@ (Core.lambdaBody $ var "l")),
      "withLet">: lambda "lt" $ lets [
        "bindings">: Core.letBindings $ var "lt",
        "names">: Sets.fromList $ Lists.map (reify Core.bindingName) (var "bindings"),
        "subst2">: Typing.termSubst $ Maps.filterWithKey (lambdas ["k", "v"] $ Logic.not $ Sets.member (var "k" :: TypedTerm Name) (var "names")) (var "s"),
        "rewriteBinding">: lambda "b" $ Core.binding
          (Core.bindingName $ var "b")
          (substituteInTerm @@ var "subst2" @@ (Core.bindingTerm $ var "b"))
          (Core.bindingTypeScheme $ var "b")] $
        Core.termLet $ Core.let_
          (Lists.map (var "rewriteBinding") (var "bindings"))
          (substituteInTerm @@ var "subst2" @@ (Core.letBody $ var "lt"))] $
      match _Term (var "term")
        (Just $ var "recurse" @@ var "term") [
        _Term_lambda>>: "l" ~> var "withLambda" @@ var "l",
        _Term_let>>: "l" ~> var "withLet" @@ var "l",
        _Term_variable>>: lambda "name" $ Optionals.match (Maps.lookup (var "name" :: TypedTerm Name) (var "s")) (var "recurse" @@ var "term") (lambda "sterm" $ var "sterm")]] $
    Rewriting.rewriteTerm @@ var "rewrite" @@ var "term0"

-- W: subst'
