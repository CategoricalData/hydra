module Hydra.Sources.Kernel.Terms.Hoisting where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  augmentBindingsWithNewFreeVars,
  bindingIsPolymorphic, bindingUsesContextTypeVars,
  countVarOccurrences,
  hoistAllLetBindings, hoistCaseStatements, hoistCaseStatementsInGraph, hoistLetBindingsWithContext, hoistLetBindingsWithPredicate, hoistPolymorphicLetBindings, hoistSubterms,
  isApplicationFunction, isEliminationUnion, isLambdaBody, isUnionElimination,
  normalizePathForHoisting,
  rewriteAndFoldTermWithTypeContext, rewriteAndFoldTermWithTypeContextAndPath, rewriteTermWithTypeContext,
  shouldHoistAll, shouldHoistCaseStatement, shouldHoistPolymorphic, updateHoistState)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors    as Accessors
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Meta.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Coders       as Coders
import qualified Hydra.Dsl.Meta.Compute      as Compute
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Grammar      as Grammar
import qualified Hydra.Dsl.Grammars          as Grammars
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Meta.Json         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows    as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Meta.Module       as Module
import qualified Hydra.Dsl.Meta.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Meta.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Meta.Typing       as Typing
import qualified Hydra.Dsl.Meta.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution


ns :: Namespace
ns = Namespace "hydra.hoisting"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Lexical.ns, Rewriting.ns, Schemas.ns, Sorting.ns, Substitution.ns]
    kernelTypesNamespaces $
    Just "Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms."
  where
   elements = [
     toBinding augmentBindingsWithNewFreeVars,
     toBinding bindingIsPolymorphic,
     toBinding bindingUsesContextTypeVars,
     toBinding countVarOccurrences,
     toBinding hoistAllLetBindings,
     toBinding hoistCaseStatements,
     toBinding hoistCaseStatementsInGraph,
     toBinding hoistLetBindingsWithContext,
     toBinding hoistLetBindingsWithPredicate,
     toBinding hoistPolymorphicLetBindings,
     toBinding hoistSubterms,
     toBinding isApplicationFunction,
     toBinding isEliminationUnion,
     toBinding isLambdaBody,
     toBinding isUnionElimination,
     toBinding normalizePathForHoisting,
     toBinding rewriteAndFoldTermWithTypeContext,
     toBinding rewriteAndFoldTermWithTypeContextAndPath,
     toBinding rewriteTermWithTypeContext,
     toBinding shouldHoistAll,
     toBinding shouldHoistCaseStatement,
     toBinding shouldHoistPolymorphic,
     toBinding updateHoistState]

-- | Check if a binding has a polymorphic type (non-empty list of type scheme variables)
bindingIsPolymorphic :: TBinding (Binding -> Bool)
bindingIsPolymorphic = define "bindingIsPolymorphic" $
  doc "Check if a binding has a polymorphic type (non-empty list of type scheme variables)" $
  "binding" ~>
  optCases (Core.bindingType $ var "binding")
    false  -- No type scheme means monomorphic (or untyped)
    ("ts" ~> Logic.not $ Lists.null $ Core.typeSchemeVariables $ var "ts")

-- | Check if a binding's type uses any type variables from the given TypeContext.
-- This checks if the free type variables in the binding's type intersect with
-- the type variables in scope (typeContextTypeVariables).
bindingUsesContextTypeVars :: TBinding (TypeContext -> Binding -> Bool)
bindingUsesContextTypeVars = define "bindingUsesContextTypeVars" $
  doc ("Check if a binding's type uses any type variables from the given TypeContext."
    <> " Returns True if the free type variables in the binding's type intersect with"
    <> " the type variables in scope (typeContextTypeVariables).") $
  "cx" ~> "binding" ~>
  optCases (Core.bindingType $ var "binding")
    false  -- No type scheme means no type variables used
    ("ts" ~>
      "freeInType" <~ Rewriting.freeVariablesInType @@ Core.typeSchemeType (var "ts") $
      "contextTypeVars" <~ Typing.typeContextTypeVariables (var "cx") $
      Logic.not $ Sets.null $ Sets.intersection (var "freeInType") (var "contextTypeVars"))

-- | Count the number of occurrences of a variable name in a term. Assumes no variable shadowing.
countVarOccurrences :: TBinding (Name -> Term -> Int)
countVarOccurrences = define "countVarOccurrences" $
  doc "Count the number of occurrences of a variable name in a term. Assumes no variable shadowing." $
  "name" ~> "term" ~>
  "childCount" <~ Lists.foldl
    ("acc" ~> "t" ~> Math.add (var "acc") (countVarOccurrences @@ var "name" @@ var "t"))
    (int32 0)
    (Rewriting.subterms @@ var "term") $
  cases _Term (var "term")
    (Just $ var "childCount") [
    _Term_variable>>: "v" ~>
      Logic.ifElse (Equality.equal (var "v") (var "name"))
        (Math.add (int32 1) (var "childCount"))
        (var "childCount")]

-- | Augment bindings with new free variables introduced by substitution, wrapping with lambdas after any type lambdas.
augmentBindingsWithNewFreeVars :: TBinding (TypeContext -> S.Set Name -> [Binding] -> ([Binding], TermSubst))
augmentBindingsWithNewFreeVars = define "augmentBindingsWithNewFreeVars" $
  doc "Augment bindings with new free variables introduced by substitution, wrapping with lambdas after any type lambdas." $
  "cx" ~> "boundVars" ~> "bindings" ~>
  "types" <~ Typing.typeContextTypes (var "cx") $
  "wrapAfterTypeLambdas" <~ ("vars" ~> "term" ~>
    cases _Term (var "term")
      -- Default: wrap with lambdas (for any non-type-lambda term)
      (Just $ Lists.foldl
        ("t" ~> "p" ~> Core.termFunction $ Core.functionLambda $
          Core.lambda (Pairs.first $ var "p") (Pairs.second $ var "p") (var "t"))
        (var "term") (Lists.reverse $ var "vars")) [
      -- Recurse through type lambdas
      _Term_typeLambda>>: "tl" ~>
        Core.termTypeLambda $ Core.typeLambda
          (Core.typeLambdaParameter $ var "tl")
          (var "wrapAfterTypeLambdas" @@ var "vars" @@ (Core.typeLambdaBody $ var "tl"))]) $
  "augment" <~ ("b" ~>
    "freeVars" <~ Sets.toList (Sets.intersection (var "boundVars") (Rewriting.freeVariablesInTerm @@ (Core.bindingTerm $ var "b"))) $
    "varTypePairs" <~ Lists.map ("v" ~> pair (var "v") (Maps.lookup (var "v") (var "types"))) (var "freeVars") $
    "varTypes" <~ Maybes.cat (Lists.map (unaryFunction Pairs.second) (var "varTypePairs")) $
    Logic.ifElse (Logic.or (Lists.null $ var "freeVars")
                           (Logic.not $ Equality.equal (Lists.length $ var "varTypes") (Lists.length $ var "varTypePairs")))
      (pair (var "b") nothing)
      (pair
        (Core.binding
          (Core.bindingName $ var "b")
          (var "wrapAfterTypeLambdas" @@ var "varTypePairs" @@ (Core.bindingTerm $ var "b"))
          (Maybes.map ("ts" ~> Core.typeScheme
            (Core.typeSchemeVariables $ var "ts")
            (Lists.foldl
              ("acc" ~> "t" ~> Core.typeFunction $ Core.functionType (var "t") (var "acc"))
              (Core.typeSchemeType $ var "ts")
              (Lists.reverse $ var "varTypes"))
            (Core.typeSchemeConstraints $ var "ts")) (Core.bindingType $ var "b")))
        (just $ pair
          (Core.bindingName $ var "b")
          (Lists.foldl
            ("t" ~> "v" ~> Core.termApplication $ Core.application (var "t") (Core.termVariable $ var "v"))
            (Core.termVariable $ Core.bindingName $ var "b")
            (var "freeVars"))))) $
  "results" <~ Lists.map (var "augment") (var "bindings") $
  pair
    (Lists.map (unaryFunction Pairs.first) (var "results"))
    (Typing.termSubst $ Maps.fromList $ Maybes.cat $ Lists.map (unaryFunction Pairs.second) (var "results"))

-- | Predicate for hoisting polymorphic bindings.
-- A binding should be hoisted if it is polymorphic or uses outer type variables.
shouldHoistPolymorphic :: TBinding (TypeContext -> Binding -> Bool)
shouldHoistPolymorphic = define "shouldHoistPolymorphic" $
  doc ("Predicate for hoisting polymorphic bindings."
    <> " Returns True if the binding is polymorphic (has type scheme variables)"
    <> " or if its type uses any type variables from the TypeContext.") $
  "cx" ~> "binding" ~>
  Logic.or (bindingIsPolymorphic @@ var "binding") (bindingUsesContextTypeVars @@ var "cx" @@ var "binding")

-- | Predicate for hoisting all bindings unconditionally.
shouldHoistAll :: TBinding (TypeContext -> Binding -> Bool)
shouldHoistAll = define "shouldHoistAll" $
  doc "Predicate that always returns True, for hoisting all bindings unconditionally." $
  constant (constant true)

hoistLetBindingsWithPredicate :: TBinding ((Binding -> Bool) -> (TypeContext -> Binding -> Bool) -> TypeContext -> Let -> Let)
hoistLetBindingsWithPredicate = define "hoistLetBindingsWithPredicate" $
  doc ("Transform a let-term by pulling let bindings to the top level."
    <> " The isParentBinding predicate applies to top-level bindings and determines whether their subterm bindings are"
    <> " eligible for hoisting."
    <> " The shouldHoistBinding predicate takes the TypeContext and a subterm binding,"
    <> " and returns True if the binding should be hoisted."
    <> " This is useful for targets like Java that cannot have polymorphic definitions in arbitrary positions."
    <> " The TypeContext provides information about type variables and lambda variables in scope."
    <> " If a hoisted binding captures let-bound or lambda-bound variables from an enclosing scope,"
    <> " the binding is wrapped in lambdas for those variables, and references are replaced"
    <> " with applications."
    <> " If a hoisted binding captures type variables from an enclosing type lambda scope,"
    <> " those type variables are added to the binding's type scheme, and references are replaced"
    <> " with type applications."
    <> " Note: we assume that there is no variable shadowing; use hydra.rewriting.unshadowVariables first.") $
  "isParentBinding" ~> "shouldHoistBinding" ~> "cx0" ~> "let0" ~>
  "hoistOne" <~ ("prefix" ~> "cx" ~> "pair" ~> "bindingWithCapturedVars" ~>
    "bindingAndReplacementPairs" <~ Pairs.first (var "pair") $
    "alreadyUsedNames" <~ Pairs.second (var "pair") $
    "b" <~ Pairs.first (var "bindingWithCapturedVars") $
    "capturedTermVars" <~ Pairs.second (var "bindingWithCapturedVars") $
    "types" <~ Typing.typeContextTypes (var "cx") $
    "capturedTermVarTypePairs" <~ Lists.map
      ("v" ~> pair (var "v") (Maps.lookup (var "v") (var "types")))
      (var "capturedTermVars") $
    "capturedTypeVars" <~ Sets.toList (Sets.intersection
      (Typing.typeContextTypeVariables $ var "cx")
      (optCases (Core.bindingType $ var "b")
        Sets.empty
        ("ts" ~> Rewriting.freeVariablesInType @@ (Core.typeSchemeType $ var "ts")))) $
    "globalBindingName" <~ Lexical.chooseUniqueName
      @@ var "alreadyUsedNames"
      @@ (Core.name (Strings.cat2 (var "prefix") (Core.unName $ Core.bindingName $ var "b"))) $
    "newUsedNames" <~ Sets.insert (var "globalBindingName") (var "alreadyUsedNames") $

    -- We can only construct a new type scheme if all of the captured term variables have types
    -- If there are any captured term variables, we create a function type
    "capturedTermVarTypes" <~ Lists.map ("typ" ~> Rewriting.deannotateTypeParameters @@ var "typ") (Maybes.cat (Lists.map (unaryFunction Pairs.second) (var "capturedTermVarTypePairs"))) $
    "newTypeScheme" <~ Logic.ifElse
      (Equality.equal (Lists.length $ var "capturedTermVarTypes") (Lists.length $ var "capturedTermVarTypePairs"))
      (Maybes.map
        ("ts" ~> Core.typeScheme
          (Lists.nub $ Lists.concat2 (var "capturedTypeVars") (Core.typeSchemeVariables $ var "ts"))
          (Lists.foldl
            ("t" ~> "a" ~> Core.typeFunction $ Core.functionType (var "a") (var "t"))
            (Core.typeSchemeType $ var "ts")
            (Lists.reverse $ var "capturedTermVarTypes"))
          (Core.typeSchemeConstraints $ var "ts"))
       (Core.bindingType $ var "b"))
      nothing $

    -- Strip all type lambdas from the original term. We will add new ones at the right level.
    "strippedTerm" <~ Rewriting.detypeTerm @@ (Core.bindingTerm $ var "b") $
    "termWithLambdas" <~ Lists.foldl
      ("t" ~> "p" ~> Core.termFunction $ Core.functionLambda $
        Core.lambda (Pairs.first $ var "p") (Maybes.map ("dom" ~> Rewriting.deannotateTypeParameters @@ var "dom") (Pairs.second $ var "p")) (var "t"))
      (var "strippedTerm")
      (Lists.reverse $ var "capturedTermVarTypePairs") $
    "termWithTypeLambdas" <~ Lists.foldl
      ("t" ~> "v" ~> Core.termTypeLambda $ Core.typeLambda (var "v") (var "t"))
      (var "termWithLambdas")
      (Lists.reverse $ Maybes.maybe (list ([] :: [TTerm Name])) (unaryFunction Core.typeSchemeVariables) $ var "newTypeScheme") $

    "replacement" <~ Lists.foldl
      ("t" ~> "v" ~> Core.termApplication $ Core.application (var "t") (Core.termVariable $ var "v"))
      (Core.termVariable $ var "globalBindingName")
      (var "capturedTermVars") $

    "newBindingAndReplacement" <~ pair
      (Core.binding (var "globalBindingName") (var "termWithTypeLambdas") (var "newTypeScheme"))
      (var "replacement") $

    -- Note: using cons for efficiency; will reverse later so that original bindings come first
    "newPairs" <~ Lists.cons (var "newBindingAndReplacement") (var "bindingAndReplacementPairs") $
    pair (var "newPairs") (var "newUsedNames")) $

  -- Rewriting function for subterms of existing bindings which also produces hoisted bindings
  "rewrite" <~ ("prefix" ~> "recurse" ~> "cx" ~> "bindingsAndNames" ~> "term" ~>
    -- Save previously finished bindings before we recurse; we don't want to apply the same substitutions to these
    "previouslyFinishedBindings" <~ Pairs.first (var "bindingsAndNames") $
    "emptyBindingsAndNames" <~ pair
      (list ([] :: [TBinding Binding]))
      (Pairs.second $ var "bindingsAndNames") $
    -- Recurse to hoist bindings from further subterms. After this, we only need to check for let at the current level.
    "result" <~ var "recurse" @@ var "emptyBindingsAndNames" @@ var "term" $
    "newBindingsAndNames" <~ Pairs.first (var "result") $
    "bindingsSoFar" <~ Pairs.first (var "newBindingsAndNames") $
    "alreadyUsedNames" <~ Pairs.second (var "newBindingsAndNames") $
    "newTerm" <~ Pairs.second (var "result") $
    cases _Term (var "newTerm")
      -- Not a let; we are done with this subterm
      (Just $ pair
        (pair (Lists.concat2 (var "previouslyFinishedBindings") (var "bindingsSoFar")) (var "alreadyUsedNames"))
        (var "newTerm")) [
      -- For a let term, we must process its bindings, hoist any or all of them as needed,
      -- and then we must replace any references to the hoisted bindings
      _Term_let>>: "l" ~>
        "body" <~ Core.letBody (var "l") $

        -- Separate bindings to be hoisted from those to be left in place
        "partitionPair" <~ Lists.partition (var "shouldHoistBinding" @@ var "cx") (Core.letBindings $ var "l") $
        "hoistUs" <~ Pairs.first (var "partitionPair") $
        "keepUs" <~ Pairs.second (var "partitionPair") $

        -- Find all of the term-level variables which are captured by each binding to be hoisted.
        -- Because of dependencies between sibling bindings, this must be done all at once, rather than
        -- individually per binding.
        "hoistedBindingNames" <~ Lists.map (unaryFunction $ Core.bindingName) (var "hoistUs") $

        -- Polymorphic let-bound variables, which are hoisted themselves.
        -- We need to include them for argument propagation, but exclude them from the final list of arguments
        -- for each hoisted binding.
        "polyLetVariables" <~ (Sets.fromList $ Lists.filter
          ("v" ~> optCases (Maps.lookup (var "v") (Typing.typeContextTypes $ var "cx"))
            false -- This function should not be applied to untyped terms, but we make a hopeful guess if it is
            Schemas.fTypeIsPolymorphic)
          (Sets.toList $ Typing.typeContextLetVariables $ var "cx")) $

        "boundTermVariables" <~ Sets.union
          (Typing.typeContextLambdaVariables $ var "cx")
          (Typing.typeContextLetVariables $ var "cx") $
        "freeVariablesInEachBinding" <~ Lists.map
          ("b" ~> Sets.toList $ Sets.intersection
            (var "boundTermVariables")
            (Rewriting.freeVariablesInTerm @@ (Core.bindingTerm $ var "b")))
          (var "hoistUs") $
        "bindingDependencies" <~ Lists.map
          ("vars" ~> Lists.partition
            ("v" ~> Sets.member (var "v") (Sets.fromList $ var "hoistedBindingNames"))
            (var "vars"))
          (var "freeVariablesInEachBinding") $
        "bindingEdges" <~ Lists.zip
          (var "hoistedBindingNames")
          (Lists.map (unaryFunction Pairs.first) (var "bindingDependencies")) $
        "bindingImmediateCapturedVars" <~ Lists.zip
          (var "hoistedBindingNames")
          (Lists.map (unaryFunction Pairs.second) (var "bindingDependencies")) $
        "capturedVarsMap" <~ Maps.fromList (Sorting.propagateTags @@ var "bindingEdges" @@ var "bindingImmediateCapturedVars") $
        "bindingsWithCapturedVars" <~ Lists.map
          ("b" ~> pair (var "b") $ optCases (Maps.lookup (Core.bindingName $ var "b") (var "capturedVarsMap"))
            (list ([] :: [TTerm Name]))
            ("vars" ~> Sets.toList $ Sets.difference (var "vars") (var "polyLetVariables")))
          (var "hoistUs") $

        -- Now hoist each binding, keeping track of the names used so far. We get back a list of
        -- top-level bindings along with replacement terms for each bound name.
        "hoistPairsAndNames" <~ Lists.foldl
          (var "hoistOne" @@ var "prefix" @@ var "cx")
          (pair (list ([] :: [TTerm (Binding, Term)])) (var "alreadyUsedNames"))
          (var "bindingsWithCapturedVars") $
        "hoistPairs" <~ Lists.reverse (Pairs.first $ var "hoistPairsAndNames") $
        "hoistedBindings" <~ Lists.map (unaryFunction Pairs.first) (var "hoistPairs") $
        "replacements" <~ Lists.map (unaryFunction Pairs.second) (var "hoistPairs") $
        "finalUsedNames" <~ Pairs.second (var "hoistPairsAndNames") $

        -- Pair each hoisted name with its replacement
        "hoistNameReplacementPairs" <~ Lists.zip
          (Lists.map (unaryFunction Core.bindingName) (var "hoistUs"))
          (var "replacements") $

        -- Map from binding name to original binding (for checking polymorphism)
        "hoistBindingMap" <~ Maps.fromList (Lists.map
          ("b" ~> pair (Core.bindingName $ var "b") (var "b"))
          (var "hoistUs")) $

        -- A binding is cacheable if: (1) referenced >1 time in body, AND (2) not polymorphic.
        -- Polymorphic bindings (with non-empty typeSchemeVariables) can't be local Java variables.
        -- Non-polymorphic bindings (hoisted only because they use context type vars) are safe to cache.
        "isCacheable" <~ ("name" ~>
          "multiRef" <~ Equality.gte (countVarOccurrences @@ var "name" @@ var "body") (int32 2) $
          "isPoly" <~ optCases (Maps.lookup (var "name") (var "hoistBindingMap"))
            false
            ("b" ~> bindingIsPolymorphic @@ var "b") $
          Logic.and (var "multiRef") (Logic.not $ var "isPoly")) $

        -- Split into single-ref/polymorphic (substitute directly) and cacheable (keep as local let)
        "singleRefPairs" <~ Lists.filter
          ("p" ~> Logic.not $ var "isCacheable" @@ Pairs.first (var "p"))
          (var "hoistNameReplacementPairs") $
        "multiRefPairs" <~ Lists.filter
          ("p" ~> var "isCacheable" @@ Pairs.first (var "p"))
          (var "hoistNameReplacementPairs") $

        -- Full substitution for all hoisted names (used for hoisted bindings, kept bindings, etc.)
        "fullSubst" <~ (Typing.termSubst $ Maps.fromList $ var "hoistNameReplacementPairs") $
        -- Partial substitution for single-ref names only (used for the body)
        "bodyOnlySubst" <~ (Typing.termSubst $ Maps.fromList $ var "singleRefPairs") $

        -- Substitute only single-ref names in the body; multi-ref names stay as variable refs
        "bodySubst" <~ Substitution.substituteInTerm @@ var "bodyOnlySubst" @@ var "body" $

        -- Create local let bindings for multi-ref hoisted names.
        -- These bind the original name to the replacement expression (which calls the hoisted function once).
        -- bindingType = Nothing so inference will infer the correct monomorphic type in context.
        "cacheBindings" <~ Lists.map
          ("p" ~> Core.binding (Pairs.first $ var "p") (Pairs.second $ var "p") nothing)
          (var "multiRefPairs") $

        -- Wrap the body in a let with the cache bindings if there are any
        "bodyWithCache" <~ Logic.ifElse (Lists.null $ var "cacheBindings")
          (var "bodySubst")
          (Core.termLet $ Core.let_ (var "cacheBindings") (var "bodySubst")) $

        -- Use full substitution for everything except the body
        "keepUsSubst" <~ Lists.map (Substitution.substituteInBinding @@ var "fullSubst") (var "keepUs") $
        "hoistedBindingsSubst" <~ Lists.map (Substitution.substituteInBinding @@ var "fullSubst") (var "hoistedBindings") $
        "bindingsSoFarSubst" <~ Lists.map (Substitution.substituteInBinding @@ var "fullSubst") (var "bindingsSoFar") $

        -- Augment bindings from inner lets with any new free variables introduced by substitution
        "augmentResult" <~ augmentBindingsWithNewFreeVars @@ var "cx" @@ (Sets.difference (var "boundTermVariables") (var "polyLetVariables")) @@ var "bindingsSoFarSubst" $
        "bindingsSoFarAugmented" <~ Pairs.first (var "augmentResult") $
        "augmentSubst" <~ Pairs.second (var "augmentResult") $

        -- Apply the augment substitution to update references in other bindings and body
        "hoistedBindingsFinal" <~ Lists.map (Substitution.substituteInBinding @@ var "augmentSubst") (var "hoistedBindingsSubst") $
        "bindingsSoFarFinal" <~ Lists.map (Substitution.substituteInBinding @@ var "augmentSubst") (var "bindingsSoFarAugmented") $
        "bodyFinal" <~ Substitution.substituteInTerm @@ var "augmentSubst" @@ var "bodyWithCache" $
        "keepUsFinal" <~ Lists.map (Substitution.substituteInBinding @@ var "augmentSubst") (var "keepUsSubst") $

        "finalTerm" <~ Logic.ifElse (Lists.null (var "keepUsFinal"))
          (var "bodyFinal")
          (Core.termLet $ Core.let_ (var "keepUsFinal") (var "bodyFinal")) $

        pair
          (pair
            (Lists.concat $ list [var "previouslyFinishedBindings", var "hoistedBindingsFinal", var "bindingsSoFarFinal"])
            (var "finalUsedNames"))
          (var "finalTerm")]) $
  "cx1" <~ Schemas.extendTypeContextForLet @@ ("c" ~> "b" ~> nothing) @@ var "cx0" @@ var "let0" $
  -- Each binding becomes a list of bindings: the original one with substitutions in its body,
  -- as well as hoisted bindings from any level. The hoisted bindings share the original binding's namespace.
  -- Since each top-level binding has exclusive access to its hoisted bindings, it can be processed individually.
  "forActiveBinding" <~ ("b" ~>
    -- Note: no possibility of name collisions between groups of hoisted bindings, because the names of the parent
    -- bindings are unique.
    "prefix" <~ (Strings.cat2 (Core.unName (Core.bindingName $ var "b")) (string "_")) $
    "init" <~ pair (list ([] :: [TTerm Binding])) (Sets.singleton $ Core.bindingName $ var "b") $
    "resultPair" <~ rewriteAndFoldTermWithTypeContext
      @@ (var "rewrite" @@ var "prefix") @@ var "cx1" @@ var "init" @@ (Core.bindingTerm $ var "b") $
    "resultBindings" <~ Pairs.first (Pairs.first (var "resultPair")) $
    "resultTerm" <~ Pairs.second (var "resultPair") $
    Lists.cons (Core.bindingWithTerm (var "b") (var "resultTerm")) (var "resultBindings")) $
  "forBinding" <~ ("b" ~> Logic.ifElse (var "isParentBinding" @@ var "b")
    (var "forActiveBinding" @@ var "b")
    (list [var "b"])) $
  Core.let_
    (Lists.concat $ Lists.map (var "forBinding") $ Core.letBindings $ var "let0")
    -- Original let body; hoisting more deeply nested bindings does not introduce additional variables into the body,
    -- and we do not hoist bindings from the body itself.
    -- This function is primarily intended for module-level transformations into targets like Java,
    -- in which we only care about the bindings of a whole-graph let term, while discarding the trivial let body.
    (Core.letBody $ var "let0")

-- | Transform a let-term by pulling all polymorphic let bindings to the top level
hoistPolymorphicLetBindings :: TBinding ((Binding -> Bool) -> Let -> Let)
hoistPolymorphicLetBindings = define "hoistPolymorphicLetBindings" $
  doc ("Transform a let-term by pulling all polymorphic let bindings to the top level."
    <> " This is useful to ensure that polymorphic bindings are not nested within other terms,"
    <> " which is unsupported by certain targets such as Java."
    <> " Polymorphic bindings are those with a non-empty list of type scheme variables."
    <> " If a hoisted binding captures lambda-bound variables from an enclosing scope,"
    <> " the binding is wrapped in lambdas for those variables, and references are replaced"
    <> " with applications."
    <> " Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first.") $
  "isParentBinding" ~> "let0" ~>
  -- Create an empty TypeContext
  "emptyIx" <~ Typing.inferenceContext (Maps.empty) (Maps.empty) (Maps.empty) (Maps.empty) false $
  "emptyCx" <~ Typing.typeContext Maps.empty Maps.empty Sets.empty Sets.empty Sets.empty (var "emptyIx") $
  hoistLetBindingsWithPredicate @@ var "isParentBinding" @@ shouldHoistPolymorphic @@ var "emptyCx" @@ var "let0"

hoistLetBindingsWithContext :: TBinding ((Binding -> Bool) -> TypeContext -> Let -> Let)
hoistLetBindingsWithContext = define "hoistLetBindingsWithContext" $
  doc ("Transform a let-term by pulling polymorphic let bindings to the top level, using TypeContext."
    <> " A binding is hoisted if:"
    <> " (1) It is polymorphic (has non-empty typeSchemeVariables), OR"
    <> " (2) Its type uses type variables from the TypeContext (i.e., from enclosing type lambdas)."
    <> " Bindings which are already at the top level are not hoisted."
    <> " If a hoisted binding captures lambda-bound or let-bound variables from an enclosing scope,"
    <> " the binding is wrapped in lambdas for those variables, and references are replaced"
    <> " with applications."
    <> " If a hoisted binding uses type variables from the context, those type variables are"
    <> " added to the binding's type scheme."
    <> " Note: we assume that there is no variable shadowing; use hydra.rewriting.unshadowVariables first.") $
  "isParentBinding" ~> "cx" ~> "let0" ~>
  hoistLetBindingsWithPredicate @@ var "isParentBinding" @@ shouldHoistPolymorphic @@ var "cx" @@ var "let0"

hoistAllLetBindings :: TBinding (Let -> Let)
hoistAllLetBindings = define "hoistAllLetBindings" $
  doc ("Transform a let-term by pulling ALL let bindings to the top level."
    <> " This is useful for targets like Java that don't support nested let expressions at all."
    <> " If a hoisted binding captures lambda-bound variables from an enclosing scope,"
    <> " the binding is wrapped in lambdas for those variables, and references are replaced"
    <> " with applications."
    <> " Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first.") $
  "let0" ~>
  -- Create an empty TypeContext
  "emptyIx" <~ Typing.inferenceContext (Maps.empty) (Maps.empty) (Maps.empty) (Maps.empty) false $
  "emptyCx" <~ Typing.typeContext Maps.empty Maps.empty Sets.empty Sets.empty Sets.empty (var "emptyIx") $
  hoistLetBindingsWithPredicate @@ constant true @@ shouldHoistAll @@ var "emptyCx" @@ var "let0"

hoistCaseStatements :: TBinding (TypeContext -> Term -> Term)
hoistCaseStatements = define "hoistCaseStatements" $
  doc ("Hoist case statements into local let bindings."
    <> " This is useful for targets such as Python which only support case statements (match) at the top level."
    <> " Case statements are hoisted only when they appear at non-top-level positions."
    <> " Top level = root, or reachable through annotations, let body/binding, lambda bodies, or ONE application LHS."
    <> " Once through an application LHS, lambda bodies no longer count as pass-through.") $
  hoistSubterms @@ shouldHoistCaseStatement

hoistCaseStatementsInGraph :: TBinding (Graph -> Flow Graph Graph)
hoistCaseStatementsInGraph = define "hoistCaseStatementsInGraph" $
  doc ("Hoist case statements into local let bindings for all elements in a graph."
    <> " This version operates prior to inference and uses an empty type context."
    <> " It hoists case statements and their applied arguments into let bindings.") $
  "graph" ~>
  -- Create an empty type context (no lambda variables to track since we're pre-inference)
  "emptyIx" <~ Typing.inferenceContext (Maps.empty) (Maps.empty) (Maps.empty) (Maps.empty) false $
  "emptyTx" <~ Typing.typeContext Maps.empty Maps.empty Sets.empty Sets.empty Sets.empty (var "emptyIx") $
  -- Convert graph to a term, apply hoisting, convert back
  "gterm0" <~ Schemas.graphAsTerm @@ var "graph" $
  "gterm1" <~ hoistCaseStatements @@ var "emptyTx" @@ var "gterm0" $
  "newElements" <~ Schemas.termAsGraph @@ var "gterm1" $
  produce $ Graph.graphWithElements (var "graph") (var "newElements")

-- | Check if a term is a union elimination (case statement)
isUnionElimination :: TBinding (Term -> Bool)
isUnionElimination = define "isUnionElimination" $
  doc "Check if a term is a union elimination (case statement)" $
  "term" ~> cases _Term (var "term")
    (Just false) [
    _Term_function>>: "f" ~> isEliminationUnion @@ var "f"]

-- | Check if a function is an elimination for union types
isEliminationUnion :: TBinding (Function -> Bool)
isEliminationUnion = define "isEliminationUnion" $
  doc "Check if a function is a union elimination" $
  "f" ~> cases _Function (var "f")
    (Just false) [
    _Function_elimination>>: "e" ~> cases _Elimination (var "e")
      (Just false) [
      _Elimination_union>>: constant true]]

-- | Update state when traversing an accessor in the path for hoisting logic.
-- State is (stillAtTopLevel, haveUsedAppLHS).
-- Returns updated state after processing one accessor.
updateHoistState :: TBinding (TermAccessor -> (Bool, Bool) -> (Bool, Bool))
updateHoistState = define "updateHoistState" $
  doc ("Update hoisting state when traversing an accessor."
    <> " State is (atTopLevel, usedAppLHS). Returns updated state.") $
  "accessor" ~> "state" ~>
  "atTop" <~ Pairs.first (var "state") $
  "usedApp" <~ Pairs.second (var "state") $
  -- If already not at top level, stay that way
  Logic.ifElse (Logic.not $ var "atTop")
    (pair false (var "usedApp"))
    -- Check this accessor
    (cases _TermAccessor (var "accessor")
      -- Default: any other accessor takes us out of top level
      (Just $ pair false (var "usedApp")) [
      -- Annotations are transparent
      _TermAccessor_annotatedBody>>: constant $ pair true (var "usedApp"),
      -- Let body and binding are pass-through
      _TermAccessor_letBody>>: constant $ pair true (var "usedApp"),
      _TermAccessor_letBinding>>: constant $ pair true (var "usedApp"),
      -- Lambda body: pass-through if we haven't used app LHS yet
      _TermAccessor_lambdaBody>>: constant $
        Logic.ifElse (var "usedApp")
          (pair false true)   -- After app LHS, lambda body is not pass-through
          (pair true false),  -- Before app LHS, lambda body is pass-through
      -- Case branches: same rules as lambda body
      _TermAccessor_unionCasesBranch>>: constant $
        Logic.ifElse (var "usedApp")
          (pair false true)
          (pair true false),
      _TermAccessor_unionCasesDefault>>: constant $
        Logic.ifElse (var "usedApp")
          (pair false true)
          (pair true false),
      -- Application function (LHS): mark usedApp=true
      _TermAccessor_applicationFunction>>: constant $
        Logic.ifElse (var "usedApp")
          (pair false true)  -- Already used app, not at top level
          (pair true true),  -- First app, still at top level but mark usedApp
      -- Application argument: takes us out of top level
      _TermAccessor_applicationArgument>>: constant $ pair false (var "usedApp")])

-- | Normalize a path by handling immediately-applied lambdas.
-- The pattern [applicationFunction, lambdaBody, ...] represents (\x -> ...) arg
-- which is semantically equivalent to let x = arg in ...
-- We replace applicationFunction followed by lambdaBody with just letBody,
-- which allows the case inside to remain at "top level".
normalizePathForHoisting :: TBinding ([TermAccessor] -> [TermAccessor])
normalizePathForHoisting = define "normalizePathForHoisting" $
  doc ("Normalize a path for hoisting by treating immediately-applied lambdas as let bindings."
    <> " Replaces [applicationFunction, lambdaBody, ...] with [letBody, ...].") $
  "path" ~>
  -- Helper: process pairs of adjacent accessors
  "go" <~ ("remaining" ~>
    -- If less than 2 elements, return as-is
    Logic.ifElse (Logic.or (Lists.null $ var "remaining")
                           (Lists.null $ Lists.tail $ var "remaining"))
      (var "remaining")
      -- Check if first two elements are applicationFunction followed by lambdaBody
      ("first" <~ Lists.head (var "remaining") $
       "second" <~ Lists.head (Lists.tail $ var "remaining") $
       "rest" <~ Lists.tail (Lists.tail $ var "remaining") $
       Logic.ifElse (Logic.and (isApplicationFunction @@ var "first")
                               (isLambdaBody @@ var "second"))
         -- Replace with letBody and continue
         (Lists.cons (inject _TermAccessor _TermAccessor_letBody unit)
                     (var "go" @@ var "rest"))
         -- Keep first element and continue
         (Lists.cons (var "first") (var "go" @@ Lists.tail (var "remaining"))))) $
  var "go" @@ var "path"

-- | Check if an accessor is applicationFunction
isApplicationFunction :: TBinding (TermAccessor -> Bool)
isApplicationFunction = define "isApplicationFunction" $
  "acc" ~> cases _TermAccessor (var "acc")
    (Just false) [
    _TermAccessor_applicationFunction>>: constant true]

-- | Check if an accessor is lambdaBody
isLambdaBody :: TBinding (TermAccessor -> Bool)
isLambdaBody = define "isLambdaBody" $
  "acc" ~> cases _TermAccessor (var "acc")
    (Just false) [
    _TermAccessor_lambdaBody>>: constant true]

-- | Predicate for hoisting case statements (union eliminations).
-- Returns True if the term is a case statement AND it is NOT at "top level".
--
-- Top level means: reachable from root through ONLY these accessor types:
--   - Annotations (transparent, always pass through)
--   - Let body or let binding (equivalent to lambda body for Python defs)
--   - Lambda body (more arguments to the def, as long as not after app LHS)
--   - ONE application function position (the single argument to match)
--
-- Once we've gone through an application function position, we can no longer
-- pass through lambda bodies (we've consumed the one allowed argument slot).
--
-- The path is traversed from the END (deepest/most recent accessor) toward
-- the beginning (root), tracking state:
--   - "atRoot": can pass through annotations, let body/binding, lambda body, or ONE app LHS
--   - "afterAppLHS": have used the one app LHS, can only pass through annotations
--   - Any other accessor: not at top level, should hoist if it's a case
shouldHoistCaseStatement :: TBinding (([TermAccessor], Term) -> Bool)
shouldHoistCaseStatement = define "shouldHoistCaseStatement" $
  doc ("Predicate for case statement hoisting."
    <> " Returns True if term is a case statement AND not at top level."
    <> " Top level = reachable through annotations, let body/binding, lambda bodies, or ONE app LHS."
    <> " Once through an app LHS, lambda bodies no longer pass through.") $
  "pathAndTerm" ~>
  "path" <~ Pairs.first (var "pathAndTerm") $
  "term" <~ Pairs.second (var "pathAndTerm") $
  -- If not a case statement, don't hoist
  Logic.ifElse (Logic.not $ isUnionElimination @@ var "term")
    false
    -- Walk the path from root to deepest, tracking whether we're still at top level
    -- State is (stillAtTopLevel, haveUsedAppLHS)
    -- Initial state: at top level, haven't used app LHS
    ("finalState" <~ Lists.foldl
      ("st" ~> "acc" ~> updateHoistState @@ var "acc" @@ var "st")
      (pair true false)
      (var "path") $
    -- If still at top level, don't hoist. If not at top level, hoist.
    Logic.not $ Pairs.first $ var "finalState")

hoistSubterms :: TBinding ((([TermAccessor], Term) -> Bool) -> TypeContext -> Term -> Term)
hoistSubterms = define "hoistSubterms" $
  doc ("Hoist subterms into local let bindings based on a path-aware predicate."
    <> " The predicate receives a pair of (path, term) where path is the list of TermAccessors"
    <> " from the root to the current term, and returns True if the term should be hoisted."
    <> " For each let term found, the immediate subterms (binding values and body) are processed:"
    <> " matching subterms within each immediate subterm are collected and hoisted into a local let"
    <> " that wraps that immediate subterm."
    <> " If a hoisted term contains free variables that are lambda-bound at an enclosing scope,"
    <> " the hoisted binding is wrapped in lambdas for those variables, and the reference"
    <> " is replaced with an application of those variables.") $
  "shouldHoist" ~> "cx0" ~> "term0" ~>

  -- Process a single immediate subterm: find all hoistable subterms, extract them, wrap in local let
  -- Returns (newCounter, transformedSubterm)
  -- Uses rewriteAndFoldTermWithTypeContextAndPath to track paths and type context
  -- The accumulator is (counter, [Binding])
  -- The namePrefix parameter is used to create stable hoisted binding names (e.g., the parent binding's name)
  "processImmediateSubterm" <~ ("cx" ~> "counter" ~> "namePrefix" ~> "subterm" ~>
    -- Lambda variables that exist at the level of the let (before processing this subterm)
    -- These don't need to be captured since they're in scope at the hoisting site
    "baselineLambdaVars" <~ Typing.typeContextLambdaVariables (var "cx") $
    -- Collect all hoistable subterms and their replacements using a fold
    -- The accumulator is (counter, [Binding])
    -- Important: We stop at let and type lambda boundaries - nested lets are handled by the outer rewrite loop,
    -- and type lambdas introduce type variables that can't be properly captured for hoisting
    --
    -- The user function receives:
    --   recurse :: a -> Term -> (a, Term) - framework handles subterm iteration
    --   path :: [TermAccessor]
    --   cx :: TypeContext
    --   acc :: (counter, [Binding])
    --   term :: Term
    "collectAndReplace" <~ ("recurse" ~> "path" ~> "cxInner" ~> "acc" ~> "term" ~>
      "currentCounter" <~ Pairs.first (var "acc") $
      "collectedBindings" <~ Pairs.second (var "acc") $
      -- Check if this is a let term or type lambda - if so, don't recurse into it
      cases _Term (var "term")
        (Just $
          -- Default case: let the framework recurse into subterms, then maybe hoist this term
          "result" <~ var "recurse" @@ var "acc" @@ var "term" $
          "newAcc" <~ Pairs.first (var "result") $
          "processedTerm" <~ Pairs.second (var "result") $
          "newCounter" <~ Pairs.first (var "newAcc") $
          "newBindings" <~ Pairs.second (var "newAcc") $
          -- Check if this term should be hoisted, passing the path
          Logic.ifElse (var "shouldHoist" @@ pair (var "path") (var "processedTerm"))
            -- Hoist: add to collected bindings, return reference
            -- Use the namePrefix to create stable names: _hoist_<prefix>_<counter>
            ("bindingName" <~ Core.name (Strings.cat (list [string "_hoist_", var "namePrefix", string "_", Literals.showInt32 (var "newCounter")])) $
             -- Find lambda-bound variables that need to be captured
             -- Only capture variables that were added INSIDE this subterm (not at the let level)
             "allLambdaVars" <~ Typing.typeContextLambdaVariables (var "cxInner") $
             -- Get names that are new lambda vars (in current scope but not baseline)
             "newLambdaVars" <~ Sets.difference (var "allLambdaVars") (var "baselineLambdaVars") $
             "freeVars" <~ Rewriting.freeVariablesInTerm @@ var "processedTerm" $
             "capturedVars" <~ Sets.toList (Sets.intersection (var "newLambdaVars") (var "freeVars")) $
             -- Wrap the term in lambdas for each captured variable
             "wrappedTerm" <~ Lists.foldl
               ("body" ~> "varName" ~>
                 Core.termFunction $ Core.functionLambda $ Core.lambda (var "varName") nothing (var "body"))
               (var "processedTerm")
               (Lists.reverse $ var "capturedVars") $
             -- Create the reference: apply the binding to all captured variables
             "reference" <~ Lists.foldl
               ("fn" ~> "varName" ~>
                 Core.termApplication $ Core.application (var "fn") (Core.termVariable $ var "varName"))
               (Core.termVariable $ var "bindingName")
               (var "capturedVars") $
             -- Add binding to collected list and return reference as the replacement
             "newBinding" <~ Core.binding (var "bindingName") (var "wrappedTerm") nothing $
             -- Return with updated state
             pair (pair (Math.add (var "newCounter") (int32 1))
                        (Lists.cons (var "newBinding") (var "newBindings")))
                  (var "reference"))
            -- Don't hoist: return (acc, processedTerm) unchanged
            (pair (var "newAcc") (var "processedTerm")))
        -- Don't recurse into these term types:
        -- TermLet: nested lets are handled by the outer rewrite loop
        -- TermTypeLambda: type lambdas introduce type variables that can't be properly captured
        [_Term_let>>: constant $ pair (var "acc") (var "term"),
         _Term_typeLambda>>: constant $ pair (var "acc") (var "term")]) $
    -- Run the collection/replacement pass using the path-aware rewriter
    -- Initial acc is (counter, []) - counter and empty list of bindings
    "result" <~ rewriteAndFoldTermWithTypeContextAndPath
      @@ var "collectAndReplace"
      @@ var "cx"
      @@ pair (var "counter") (list ([] :: [TTerm Binding]))
      @@ var "subterm" $
    -- result is (finalAcc, transformedSubterm)
    "finalAcc" <~ Pairs.first (var "result") $
    "transformedSubterm" <~ Pairs.second (var "result") $
    "finalCounter" <~ Pairs.first (var "finalAcc") $
    "bindings" <~ Pairs.second (var "finalAcc") $
    -- If any bindings were collected, wrap in a local let
    Logic.ifElse (Lists.null (var "bindings"))
      (pair (var "finalCounter") (var "transformedSubterm"))
      ("localLet" <~ Core.termLet (Core.let_ (Lists.reverse (var "bindings")) (var "transformedSubterm")) $
       pair (var "finalCounter") (var "localLet"))) $

  -- Process a let term: apply hoisting to each immediate subterm
  -- Each binding uses its own name as the prefix for hoisted bindings, providing stable naming.
  -- The prefix ensures uniqueness across siblings, so changes to one binding won't affect
  -- the hoisted names in other bindings.
  -- Each sibling uses the same starting counter (1), and the prefix prevents collisions.
  "processLetTerm" <~ ("cx" ~> "counter" ~> "lt" ~>
    "bindings" <~ Core.letBindings (var "lt") $
    "body" <~ Core.letBody (var "lt") $
    -- Process each binding value using its name as the prefix
    -- Each binding starts with counter 1 (reset for each sibling) for stable naming
    -- The prefix ensures uniqueness across siblings
    "processBinding" <~ ("acc" ~> "binding" ~>
      -- Use the binding name as the prefix for hoisted binding names
      -- Replace dots with underscores to avoid creating module-like names
      "namePrefix" <~ Strings.intercalate (string "_") (Strings.splitOn (string ".") (Core.unName (Core.bindingName (var "binding")))) $
      -- Each sibling starts fresh with counter 1 - prefix makes names unique
      "result" <~ var "processImmediateSubterm" @@ var "cx" @@ int32 1 @@ var "namePrefix" @@ (Core.bindingTerm (var "binding")) $
      "newValue" <~ Pairs.second (var "result") $
      "newBinding" <~ Core.binding (Core.bindingName (var "binding")) (var "newValue") (Core.bindingType (var "binding")) $
      Lists.cons (var "newBinding") (var "acc")) $
    -- Fold over bindings, starting with empty list
    "newBindingsReversed" <~ Lists.foldl (var "processBinding") (list ([] :: [TTerm Binding])) (var "bindings") $
    "newBindings" <~ Lists.reverse (var "newBindingsReversed") $
    -- Process the body with "_body" as the prefix, also starting with counter 1
    "bodyResult" <~ var "processImmediateSubterm" @@ var "cx" @@ int32 1 @@ string "_body" @@ var "body" $
    "newBody" <~ Pairs.second (var "bodyResult") $
    -- Return the original counter (siblings are independent, so counter doesn't propagate)
    pair (var "counter") (Core.termLet (Core.let_ (var "newBindings") (var "newBody")))) $

  -- Main rewrite: find let terms and process them
  "rewrite" <~ ("recurse" ~> "cx" ~> "counter" ~> "term" ~>
    cases _Term (var "term")
      (Just $ var "recurse" @@ var "counter" @@ var "term") [
      _Term_let>>: "lt" ~>
        -- First recurse into the let to process any nested lets
        "recursed" <~ var "recurse" @@ var "counter" @@ var "term" $
        "newCounter" <~ Pairs.first (var "recursed") $
        "recursedTerm" <~ Pairs.second (var "recursed") $
        -- Extract the let from the recursed term and process its immediate subterms
        cases _Term (var "recursedTerm")
          (Just $ pair (var "newCounter") (var "recursedTerm")) [
          _Term_let>>: "lt2" ~> var "processLetTerm" @@ var "cx" @@ var "newCounter" @@ var "lt2"]]) $

  Pairs.second $ rewriteAndFoldTermWithTypeContext @@ var "rewrite" @@ var "cx0" @@ int32 1 @@ var "term0"

rewriteAndFoldTermWithTypeContext :: TBinding (((a -> Term -> (a, Term)) -> TypeContext -> a -> Term -> (a, Term)) -> TypeContext -> a -> Term -> (a, Term))
rewriteAndFoldTermWithTypeContext = define "rewriteAndFoldTermWithTypeContext" $
  doc ("Rewrite a term while folding to produce a value, with TypeContext updated as we descend into subterms."
    <> " Combines the features of rewriteAndFoldTerm and rewriteTermWithTypeContext."
    <> " The user function f receives a recurse function that handles subterm traversal and TypeContext management.") $
  "f" ~> "cx0" ~> "val0" ~> "term0" ~>
  -- wrapper is the function we pass to rewriteAndFoldTerm
  -- Combined state is (val, cx). The low-level recurse handles term structure traversal.
  "wrapper" <~ ("lowLevelRecurse" ~> "valAndCx" ~> "term" ~>
    "val" <~ Pairs.first (var "valAndCx") $
    "cx" <~ Pairs.second (var "valAndCx") $
    -- Determine updated context based on the current term
    "cx1" <~ (cases _Term (var "term")
      (Just $ var "cx") [
      _Term_function>>: "fun" ~> cases _Function (var "fun")
        (Just $ var "cx") [
        _Function_lambda>>: "l" ~> Schemas.extendTypeContextForLambda @@ var "cx" @@ var "l"],
      _Term_let>>: "l" ~> Schemas.extendTypeContextForLet @@ constant (constant nothing) @@ var "cx" @@ var "l",
      _Term_typeLambda>>: "tl" ~> Schemas.extendTypeContextForTypeLambda @@ var "cx" @@ var "tl"]) $
    -- Create a recurse function for the user that unwraps/wraps the combined state
    "recurseForUser" <~ ("newVal" ~> "subterm" ~>
      -- Call low-level recurse with combined state (newVal, cx1)
      -- Note: cx1 is the context for subterms of the current term
      "result" <~ var "lowLevelRecurse" @@ pair (var "newVal") (var "cx1") @@ var "subterm" $
      -- Return just (val', term') to the user
      pair (Pairs.first $ Pairs.first $ var "result") (Pairs.second $ var "result")) $
    -- Call the user's function with the context-aware recurse
    "fResult" <~ var "f" @@ var "recurseForUser" @@ var "cx1" @@ var "val" @@ var "term" $
    -- Combine the result with cx (original context, not cx1) so sibling terms don't inherit each other's extensions
    pair (pair (Pairs.first $ var "fResult") (var "cx")) (Pairs.second $ var "fResult")) $
  -- Use rewriteAndFoldTerm to handle the actual traversal, with (val, cx) as combined state
  "result" <~ Rewriting.rewriteAndFoldTerm @@ var "wrapper" @@ pair (var "val0") (var "cx0") @@ var "term0" $
  -- Extract just the val part of the result
  pair (Pairs.first $ Pairs.first $ var "result") (Pairs.second $ var "result")

-- | The most general-purpose term rewriting function, combining:
--   - Folding to produce a value (like rewriteAndFoldTerm)
--   - TypeContext tracking (like rewriteTermWithTypeContext)
--   - Path tracking via TermAccessors (like rewriteAndFoldTermWithPath)
--
-- This function wraps rewriteAndFoldTermWithPath, automatically managing
-- TypeContext updates as the traversal descends into lambdas, lets, and type lambdas.
--
-- The user function receives:
--   - A recurse function: a -> Term -> (a, Term) - called by framework during traversal
--   - The current path (list of TermAccessors from root to current position)
--   - The current TypeContext (updated for the current position)
--   - The current accumulated value
--   - The current term
-- And returns (newVal, newTerm)
rewriteAndFoldTermWithTypeContextAndPath :: TBinding (
  ((a -> Term -> (a, Term)) -> [TermAccessor] -> TypeContext -> a -> Term -> (a, Term))
  -> TypeContext -> a -> Term -> (a, Term))
rewriteAndFoldTermWithTypeContextAndPath = define "rewriteAndFoldTermWithTypeContextAndPath" $
  doc ("Rewrite a term while folding to produce a value, with both TypeContext and accessor path tracked."
    <> " The path is a list of TermAccessors representing the position from the root to the current term."
    <> " Combines the features of rewriteAndFoldTermWithPath and TypeContext tracking."
    <> " The TypeContext is automatically updated when descending into lambdas, lets, and type lambdas.") $
  "f" ~> "cx0" ~> "val0" ~> "term0" ~>
  -- Combined state is (TypeContext, a). We wrap rewriteAndFoldTermWithPath.
  -- The wrapper function receives recurse, path, (cx, val), term and returns ((cx, val), term)
  "wrapper" <~ ("recurse" ~> "path" ~> "cxAndVal" ~> "term" ~>
    "cx" <~ Pairs.first (var "cxAndVal") $
    "val" <~ Pairs.second (var "cxAndVal") $
    -- Determine updated context based on the current term
    "cx1" <~ (cases _Term (var "term")
      (Just $ var "cx") [
      _Term_function>>: "fun" ~> cases _Function (var "fun")
        (Just $ var "cx") [
        _Function_lambda>>: "l" ~> Schemas.extendTypeContextForLambda @@ var "cx" @@ var "l"],
      _Term_let>>: "l" ~> Schemas.extendTypeContextForLet @@ constant (constant nothing) @@ var "cx" @@ var "l",
      _Term_typeLambda>>: "tl" ~> Schemas.extendTypeContextForTypeLambda @@ var "cx" @@ var "tl"]) $
    -- Create a recurse function for the user that uses the combined state
    -- Note: the user's recurse takes just (val, term) but the framework's recurse
    -- takes (path, val, term). We pass the current path through.
    "recurseForUser" <~ ("valIn" ~> "termIn" ~>
      -- Call the framework recurse with path and combined state (cx1, valIn)
      -- Note: cx1 is the context for subterms of the current term
      "result" <~ var "recurse" @@ var "path" @@ pair (var "cx1") (var "valIn") @@ var "termIn" $
      -- Return just (val', term') to the user - discard the context from result
      pair (Pairs.second $ Pairs.first $ var "result") (Pairs.second $ var "result")) $
    -- Call the user's function with the updated context and user-facing recurse
    "fResult" <~ var "f" @@ var "recurseForUser" @@ var "path" @@ var "cx1" @@ var "val" @@ var "term" $
    -- Return with combined state: ((cx, val'), term')
    -- Note: we return the original cx, not cx1, because cx1 is for subterms
    pair (pair (var "cx") (Pairs.first $ var "fResult")) (Pairs.second $ var "fResult")) $
  -- Use rewriteAndFoldTermWithPath with combined state (cx0, val0)
  "result" <~ Rewriting.rewriteAndFoldTermWithPath @@ var "wrapper" @@ pair (var "cx0") (var "val0") @@ var "term0" $
  -- Extract just the val part of the result
  pair (Pairs.second $ Pairs.first $ var "result") (Pairs.second $ var "result")

rewriteTermWithTypeContext :: TBinding (((Term -> Term) -> TypeContext -> Term -> Term) -> TypeContext -> Term -> Term)
rewriteTermWithTypeContext = define "rewriteTermWithTypeContext" $
  doc "Rewrite a term with the help of a type context which is updated as we descend into subterms" $
  "f" ~> "cx0" ~> "term0" ~>
  -- f2 wraps f to handle TypeContext updates for lambda/let/typeLambda
  "f2" <~ ("recurse" ~> "cx" ~> "term" ~>
    -- recurse1 is what the user sees: it takes just term and handles cx internally
    "recurse1" <~ ("term" ~> var "recurse" @@ var "cx" @@ var "term") $
    -- Determine updated context based on term type, then call user's f
    cases _Term (var "term") (Just $ var "f" @@ var "recurse1" @@ var "cx" @@ var "term") [
      _Term_function>>: "fun" ~> cases _Function (var "fun")
        (Just $ var "f" @@ var "recurse1" @@ var "cx" @@ var "term") [
        _Function_lambda>>: "l" ~>
          "cx1" <~ Schemas.extendTypeContextForLambda @@ var "cx" @@ var "l" $
          "recurse2" <~ ("term" ~> var "recurse" @@ var "cx1" @@ var "term") $
          var "f" @@ var "recurse2" @@ var "cx1" @@ var "term"],
      _Term_let>>: "l" ~>
        "cx1" <~ Schemas.extendTypeContextForLet @@ constant (constant nothing) @@ var "cx" @@ var "l" $
        "recurse2" <~ ("term" ~> var "recurse" @@ var "cx1" @@ var "term") $
        var "f" @@ var "recurse2" @@ var "cx1" @@ var "term",
      _Term_typeLambda>>: "tl" ~>
        "cx1" <~ Schemas.extendTypeContextForTypeLambda @@ var "cx" @@ var "tl" $
        "recurse2" <~ ("term" ~> var "recurse" @@ var "cx1" @@ var "term") $
        var "f" @@ var "recurse2" @@ var "cx1" @@ var "term"]) $
  -- Local fixpoint that threads context through
  "rewrite" <~ ("cx" ~> "term" ~> var "f2" @@ (var "rewrite") @@ var "cx" @@ var "term") $
  var "rewrite" @@ var "cx0" @@ var "term0"
