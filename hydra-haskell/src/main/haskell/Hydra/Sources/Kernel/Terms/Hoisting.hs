module Hydra.Sources.Kernel.Terms.Hoisting where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  bindingIsPolymorphic,
  hoistCaseStatements, hoistCaseStatementsInGraph, hoistLetBindings, hoistPolymorphicLetBindings, hoistSubterms,
  isApplicationFunction, isEliminationUnion, isLambdaBody, isUnionElimination,
  normalizePathForHoisting,
  rewriteAndFoldTermWithTypeContext, rewriteAndFoldTermWithTypeContextAndPath, rewriteTermWithTypeContext,
  shouldHoistCaseStatement, updateHoistState)
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

import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas


ns :: Namespace
ns = Namespace "hydra.hoisting"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Lexical.ns, Rewriting.ns, Schemas.ns]
    kernelTypesNamespaces $
    Just "Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms."
  where
   elements = [
     toBinding bindingIsPolymorphic,
     toBinding hoistCaseStatements,
     toBinding hoistCaseStatementsInGraph,
     toBinding hoistLetBindings,
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
     toBinding shouldHoistCaseStatement,
     toBinding updateHoistState]

-- | Check if a binding has a polymorphic type (non-empty list of type scheme variables)
bindingIsPolymorphic :: TBinding (Binding -> Bool)
bindingIsPolymorphic = define "bindingIsPolymorphic" $
  doc "Check if a binding has a polymorphic type (non-empty list of type scheme variables)" $
  "binding" ~>
  optCases (Core.bindingType $ var "binding")
    false  -- No type scheme means monomorphic (or untyped)
    ("ts" ~> Logic.not $ Lists.null $ Core.typeSchemeVariables $ var "ts")

-- | Transform a let-term by pulling let bindings to the top level.
-- The hoistAll parameter controls whether to hoist all bindings (True) or only polymorphic ones (False).
-- This is useful for targets like Java that cannot have let-expressions in arbitrary positions.
hoistLetBindings :: TBinding (Bool -> Let -> Let)
hoistLetBindings = define "hoistLetBindings" $
  doc ("Transform a let-term by pulling let bindings to the top level."
    <> " The hoistAll parameter controls whether to hoist all bindings (True) or only polymorphic ones (False)."
    <> " This is useful for targets like Java that cannot have let-expressions in arbitrary positions."
    <> " Polymorphic bindings are those with a non-empty list of type scheme variables,"
    <> " OR bindings inside a type lambda scope (which use outer type variables in their types)."
    <> " If a hoisted binding captures lambda-bound variables from an enclosing scope,"
    <> " the binding is wrapped in lambdas for those variables, and references are replaced"
    <> " with applications."
    <> " If a hoisted binding captures type variables from an enclosing type lambda scope,"
    <> " those type variables are added to the binding's type scheme, and references are replaced"
    <> " with type applications."
    <> " Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first.") $
  "hoistAll" ~> "let0" ~>
  -- Collect all binding names from the original let (to avoid name collisions)
  "topLevelNames" <~ Sets.fromList (Lists.map ("b" ~> Core.bindingName $ var "b") $ Core.letBindings $ var "let0") $

  -- Define mutually recursive helper functions together using lets
  lets [
    -- Process a term to find and hoist nested let bindings
    -- typeVars: list of Names for type variables in scope from enclosing type lambdas (outermost to innermost)
    -- lambdaVars: list of (Name, Maybe Type) for lambda-bound variables in scope (in order from outermost to innermost)
    -- reserved: set of reserved names
    -- Returns (hoistedBindings, reservedNames, processedTerm)
    -- Each hoisted binding is a tuple: (bindingName, wrappedTerm, capturedTermVars, capturedTypeVars, originalType)
    -- where capturedTermVars is the list of captured lambda variables (for replacing references)
    -- and capturedTypeVars is the list of captured type variables from enclosing type lambdas
    -- Hoisted binding info is a 5-tuple: (name, wrappedTerm, capturedTermVars, capturedTypeVars, wrappedType)
    -- Empty hoisted list type annotation
    "emptyHoisted">: (list ([] :: [TTerm (Name, Term, [Name], [Name], Y.Maybe TypeScheme)])),

    "processTermForHoisting">: ("typeVars" ~> "lambdaVars" ~> "reserved" ~> "term" ~>
      cases _Term (var "term")
        -- Default: return unchanged
        (Just $ pair (var "emptyHoisted") (pair (var "reserved") (var "term"))) [
        -- For let terms, process their bindings and body
        _Term_let>>: "lt" ~>
          -- Process each binding
          "bindingsResult" <~ Lists.foldl
            (var "processBinding" @@ var "typeVars" @@ var "lambdaVars")
            (pair (var "emptyHoisted") (pair (var "reserved") (list ([] :: [TTerm Binding]))))
            (Core.letBindings $ var "lt") $
          "hoistedFromBindings" <~ Pairs.first (var "bindingsResult") $
          "reservedAfterBindings" <~ Pairs.first (Pairs.second $ var "bindingsResult") $
          "keptBindingsRaw" <~ Lists.reverse (Pairs.second (Pairs.second $ var "bindingsResult")) $
          -- Process the body
          "bodyResult" <~ var "processTermForHoisting" @@ var "typeVars" @@ var "lambdaVars" @@ var "reservedAfterBindings" @@ (Core.letBody $ var "lt") $
          "hoistedFromBodyRaw" <~ Pairs.first (var "bodyResult") $
          "reservedAfterBody" <~ Pairs.first (Pairs.second $ var "bodyResult") $
          "processedBodyRaw" <~ Pairs.second (Pairs.second $ var "bodyResult") $

          -- Find kept bindings that are referenced by hoistedFromBody bindings.
          -- These must also be hoisted to maintain scope integrity.
          -- Collect names of all kept bindings
          "keptNames" <~ Sets.fromList (Lists.map ("b" ~> Core.bindingName (var "b")) (var "keptBindingsRaw")) $
          -- Collect free variables in all hoistedFromBody terms
          "hoistedBodyFreeVars" <~ Sets.fromList (Lists.concat (Lists.map
            ("info" ~> Sets.toList (Rewriting.freeVariablesInTerm @@ Pairs.first (Pairs.second $ var "info")))
            (var "hoistedFromBodyRaw"))) $
          -- Find kept bindings referenced by hoisted body bindings
          "forcedHoistNames" <~ Sets.intersection (var "keptNames") (var "hoistedBodyFreeVars") $

          -- Split keptBindingsRaw into bindings that must be force-hoisted and bindings that stay kept
          "forceHoistedBindings" <~ Lists.filter
            ("b" ~> Sets.member (Core.bindingName $ var "b") (var "forcedHoistNames"))
            (var "keptBindingsRaw") $
          "trulyKeptBindingsRaw" <~ Lists.filter
            ("b" ~> Logic.not (Sets.member (Core.bindingName $ var "b") (var "forcedHoistNames")))
            (var "keptBindingsRaw") $

          -- First, replace references to hoistedFromBindings in the terms of force-hoisted bindings.
          -- These bindings were kept bindings, so their terms need reference replacement.
          "forceHoistedBindingsWithReplacedRefs" <~ Lists.map
            ("b" ~>
              "updatedTerm" <~ var "replaceReferences" @@ var "hoistedFromBindings" @@ (Core.bindingTerm (var "b")) $
              Core.binding (Core.bindingName (var "b")) (var "updatedTerm") (Core.bindingType (var "b")))
            (var "forceHoistedBindings") $

          -- Convert force-hoisted bindings to hoisted info format
          -- Note: These bindings were originally monomorphic, so they have no captured type vars
          -- but we still need to handle captured term vars (lambda-bound variables they reference)
          "forceHoistedInfo" <~ Lists.map
            ("binding" ~>
              "bindingName" <~ Core.bindingName (var "binding") $
              "freeVars" <~ Rewriting.freeVariablesInTerm @@ Core.bindingTerm (var "binding") $
              "lambdaVarNames" <~ Sets.fromList (Lists.map ("lv" ~> Pairs.first (var "lv")) (var "lambdaVars")) $
              "capturedVarNames" <~ Sets.intersection (var "lambdaVarNames") (var "freeVars") $
              "capturedTermVars" <~ Lists.filter ("lv" ~> Sets.member (Pairs.first (var "lv")) (var "capturedVarNames")) (var "lambdaVars") $
              -- Wrap the term in lambdas for each captured term variable
              "wrappedTerm" <~ Lists.foldl
                ("body" ~> "lv" ~>
                  Core.termFunction $ Core.functionLambda $ Core.lambda (Pairs.first (var "lv")) (Pairs.second (var "lv")) (var "body"))
                (Core.bindingTerm $ var "binding")
                (Lists.reverse $ var "capturedTermVars") $
              -- Compute wrapped type with captured term variable types prepended
              "wrappedType" <~ optCases (Core.bindingType $ var "binding")
                Phantoms.nothing
                ("ts" ~>
                  "origType" <~ Core.typeSchemeType (var "ts") $
                  "newType" <~ Lists.foldl
                    ("innerType" ~> "lv" ~>
                      optCases (Pairs.second $ var "lv")
                        (var "innerType")
                        ("domainType" ~> Core.typeFunction $ Core.functionType (var "domainType") (var "innerType")))
                    (var "origType")
                    (Lists.reverse $ var "capturedTermVars") $
                  Phantoms.just $ Core.typeScheme (Core.typeSchemeVariables $ var "ts") (var "newType") (Core.typeSchemeConstraints $ var "ts")) $
              Phantoms.tuple5 (var "bindingName") (var "wrappedTerm") (Lists.map ("lv" ~> Pairs.first (var "lv")) (var "capturedTermVars")) (list ([] :: [TTerm Name])) (var "wrappedType"))
            (var "forceHoistedBindingsWithReplacedRefs") $

          -- Combine hoistedFromBindings with force-hoisted bindings for replacement purposes
          "allHoistedFromThisLevel" <~ Lists.concat2 (var "hoistedFromBindings") (var "forceHoistedInfo") $

          -- Replace references to allHoistedFromThisLevel (bindings hoisted at THIS level) in the
          -- terms of hoistedFromBody (bindings hoisted from nested lets).
          -- This handles the case where a nested let binding references a sibling from an outer let.
          "hoistedFromBody" <~ Lists.map
            ("info" ~>
              "infoName" <~ var "getInfoName" @@ var "info" $
              "infoTerm" <~ Pairs.first (Pairs.second $ var "info") $
              "infoCapTermVars" <~ var "getInfoCapturedTermVars" @@ var "info" $
              "infoCapTypeVars" <~ var "getInfoCapturedTypeVars" @@ var "info" $
              "infoType" <~ Pairs.second (Pairs.second (Pairs.second (Pairs.second $ var "info"))) $
              "updatedTerm" <~ var "replaceReferences" @@ var "allHoistedFromThisLevel" @@ var "infoTerm" $
              Phantoms.tuple5 (var "infoName") (var "updatedTerm") (var "infoCapTermVars") (var "infoCapTypeVars") (var "infoType"))
            (var "hoistedFromBodyRaw") $
          -- Combine all hoisted bindings (from body + from this level's bindings + force-hoisted)
          "allHoisted" <~ Lists.concat (list [var "hoistedFromBody", var "hoistedFromBindings", var "forceHoistedInfo"]) $
          -- Replace references to hoisted bindings in kept bindings and body
          "keptBindings" <~ Lists.map
            ("b" ~>
              "updatedTerm" <~ var "replaceReferences" @@ var "allHoistedFromThisLevel" @@ (Core.bindingTerm (var "b")) $
              Core.binding (Core.bindingName (var "b")) (var "updatedTerm") (Core.bindingType (var "b")))
            (var "trulyKeptBindingsRaw") $
          "processedBody" <~ var "replaceReferences" @@ var "allHoistedFromThisLevel" @@ var "processedBodyRaw" $
          -- If no bindings left, just return the body; otherwise rebuild the let
          Logic.ifElse (Lists.null $ var "keptBindings")
            (pair (var "allHoisted") (pair (var "reservedAfterBody") (var "processedBody")))
            (pair (var "allHoisted") (pair (var "reservedAfterBody")
              (Core.termLet $ Core.let_ (var "keptBindings") (var "processedBody")))),
        -- Lambda: add parameter to lambdaVars and recurse into body
        _Term_function>>: "f" ~>
          cases _Function (var "f")
            (Just $ pair (var "emptyHoisted") (pair (var "reserved") (var "term"))) [
            _Function_lambda>>: "lam" ~>
              "paramName" <~ Core.lambdaParameter (var "lam") $
              "paramDomain" <~ Core.lambdaDomain (var "lam") $
              -- Add this lambda's parameter to the list of lambda-bound variables
              "newLambdaVars" <~ Lists.concat2 (var "lambdaVars") (Lists.pure (pair (var "paramName") (var "paramDomain"))) $
              "bodyResult" <~ var "processTermForHoisting" @@ var "typeVars" @@ var "newLambdaVars" @@ var "reserved" @@ (Core.lambdaBody $ var "lam") $
              "hoisted" <~ Pairs.first (var "bodyResult") $
              "newReserved" <~ Pairs.first (Pairs.second $ var "bodyResult") $
              "newBody" <~ Pairs.second (Pairs.second $ var "bodyResult") $
              pair (var "hoisted") (pair (var "newReserved")
                (Core.termFunction $ Core.functionLambda $
                  Core.lambda (var "paramName") (var "paramDomain") (var "newBody")))],
        -- Application: recurse into function and argument
        _Term_application>>: "app" ~>
          "fnResult" <~ var "processTermForHoisting" @@ var "typeVars" @@ var "lambdaVars" @@ var "reserved" @@ (Core.applicationFunction $ var "app") $
          "fnHoisted" <~ Pairs.first (var "fnResult") $
          "reservedAfterFn" <~ Pairs.first (Pairs.second $ var "fnResult") $
          "newFn" <~ Pairs.second (Pairs.second $ var "fnResult") $
          "argResult" <~ var "processTermForHoisting" @@ var "typeVars" @@ var "lambdaVars" @@ var "reservedAfterFn" @@ (Core.applicationArgument $ var "app") $
          "argHoisted" <~ Pairs.first (var "argResult") $
          "reservedAfterArg" <~ Pairs.first (Pairs.second $ var "argResult") $
          "newArg" <~ Pairs.second (Pairs.second $ var "argResult") $
          pair (Lists.concat2 (var "argHoisted") (var "fnHoisted"))
            (pair (var "reservedAfterArg")
              (Core.termApplication $ Core.application (var "newFn") (var "newArg"))),
        -- Annotated: recurse into body
        _Term_annotated>>: "ann" ~>
          "bodyResult" <~ var "processTermForHoisting" @@ var "typeVars" @@ var "lambdaVars" @@ var "reserved" @@ (Core.annotatedTermBody $ var "ann") $
          "hoisted" <~ Pairs.first (var "bodyResult") $
          "newReserved" <~ Pairs.first (Pairs.second $ var "bodyResult") $
          "newBody" <~ Pairs.second (Pairs.second $ var "bodyResult") $
          pair (var "hoisted") (pair (var "newReserved")
            (Core.termAnnotated $ Core.annotatedTerm (var "newBody") (Core.annotatedTermAnnotation $ var "ann"))),
        -- Type lambda: recurse into body, tracking the type parameter
        -- Bindings inside type lambdas that use the type parameter will have it added to their type scheme
        _Term_typeLambda>>: "tl" ~>
          "typeParam" <~ Core.typeLambdaParameter (var "tl") $
          -- Add this type lambda's parameter to the list of type variables in scope
          "newTypeVars" <~ Lists.concat2 (var "typeVars") (Lists.pure (var "typeParam")) $
          "bodyResult" <~ var "processTermForHoisting" @@ var "newTypeVars" @@ var "lambdaVars" @@ var "reserved" @@ (Core.typeLambdaBody $ var "tl") $
          "hoisted" <~ Pairs.first (var "bodyResult") $
          "newReserved" <~ Pairs.first (Pairs.second $ var "bodyResult") $
          "newBody" <~ Pairs.second (Pairs.second $ var "bodyResult") $
          pair (var "hoisted") (pair (var "newReserved")
            (Core.termTypeLambda $ Core.typeLambda (var "typeParam") (var "newBody"))),
        -- Type application: recurse into body (type applications don't introduce new type variables)
        _Term_typeApplication>>: "ta" ~>
          "bodyResult" <~ var "processTermForHoisting" @@ var "typeVars" @@ var "lambdaVars" @@ var "reserved" @@ (Core.typeApplicationTermBody $ var "ta") $
          "hoisted" <~ Pairs.first (var "bodyResult") $
          "newReserved" <~ Pairs.first (Pairs.second $ var "bodyResult") $
          "newBody" <~ Pairs.second (Pairs.second $ var "bodyResult") $
          pair (var "hoisted") (pair (var "newReserved")
            (Core.termTypeApplication $ Core.typeApplicationTerm (var "newBody") (Core.typeApplicationTermType $ var "ta")))]),

    -- Process a single binding: if hoistAll is True, binding is polymorphic, OR we're inside a type lambda scope, add to hoisted list
    -- typeVars: list of Names for type variables in scope from enclosing type lambdas
    -- lambdaVars: list of (Name, Maybe Type) for lambda-bound variables in scope
    -- State is (hoistedBindings, reservedNames, keptBindings)
    "processBinding">: ("typeVars" ~> "lambdaVars" ~> "state" ~> "binding" ~>
      "hoisted" <~ Pairs.first (var "state") $
      "reserved" <~ Pairs.first (Pairs.second $ var "state") $
      "kept" <~ Pairs.second (Pairs.second $ var "state") $
      -- Process the binding's term to hoist nested bindings
      "processedTerm" <~ var "processTermForHoisting" @@ var "typeVars" @@ var "lambdaVars" @@ var "reserved" @@ (Core.bindingTerm $ var "binding") $
      "innerHoisted" <~ Pairs.first (var "processedTerm") $
      "newReserved" <~ Pairs.first (Pairs.second $ var "processedTerm") $
      "newTerm" <~ Pairs.second (Pairs.second $ var "processedTerm") $
      -- Check if this binding should be hoisted:
      -- 1. hoistAll is True (but NOT if inside type lambda - those need special handling), OR
      -- 2. binding is polymorphic (has type scheme variables), OR
      -- 3. binding's type uses type variables from enclosing type lambdas
      --
      -- For case 3: we need to check if the binding's type references any of the type variables in scope.
      -- If so, we must hoist it and add those type variables to its type scheme.
      "isInsideTypeLambda" <~ Logic.not (Lists.null $ var "typeVars") $
      -- Compute which type variables from scope are actually used by this binding's type
      "usedTypeVars" <~ optCases (Core.bindingType $ var "binding")
        (list ([] :: [TTerm Name]))  -- No type scheme, no type vars used
        ("ts" ~>
          -- Get free type variables in the type scheme's body
          "freeInType" <~ Rewriting.freeVariablesInType @@ Core.typeSchemeType (var "ts") $
          -- Filter to only those from enclosing type lambdas
          Lists.filter ("tv" ~> Sets.member (var "tv") (var "freeInType")) (var "typeVars")) $
      -- Binding uses outer type vars if usedTypeVars is non-empty
      "usesOuterTypeVars" <~ Logic.not (Lists.null $ var "usedTypeVars") $
      -- When hoistAll=true: hoist everything except bindings inside type lambdas
      -- When hoistAll=false: hoist polymorphic bindings, plus monomorphic ones that use outer type vars
      "shouldHoist" <~ Logic.ifElse (var "hoistAll")
        (Logic.not $ var "isInsideTypeLambda")  -- hoistAll: don't hoist inside type lambdas
        (Logic.or (bindingIsPolymorphic @@ var "binding") (var "usesOuterTypeVars")) $
      Logic.ifElse (var "shouldHoist")
        -- Hoist: compute captured term variables and type variables, add to hoisted list
        ("bindingName" <~ Core.bindingName (var "binding") $
         "freeVars" <~ Rewriting.freeVariablesInTerm @@ var "newTerm" $
         "lambdaVarNames" <~ Sets.fromList (Lists.map ("lv" ~> Pairs.first (var "lv")) (var "lambdaVars")) $
         -- Captured term vars are lambda-bound variables that appear free in the binding
         "capturedVarNames" <~ Sets.intersection (var "lambdaVarNames") (var "freeVars") $
         -- Filter lambdaVars to only those that are captured, preserving order
         "capturedTermVars" <~ Lists.filter ("lv" ~> Sets.member (Pairs.first (var "lv")) (var "capturedVarNames")) (var "lambdaVars") $
         -- Captured type vars: ALL free type variables in the binding's type that are not already quantified
         -- This includes both type variables from enclosing type lambdas (usedTypeVars)
         -- AND any other free type variables that may have been introduced during inference
         "capturedTypeVars" <~ optCases (Core.bindingType $ var "binding")
           (list ([] :: [TTerm Name]))  -- No type scheme means no captured type vars
           ("ts" ~>
             "freeInType" <~ Rewriting.freeVariablesInType @@ Core.typeSchemeType (var "ts") $
             "alreadyQuantified" <~ Sets.fromList (Core.typeSchemeVariables $ var "ts") $
             -- All free type vars not already in the type scheme's variables
             "allUnquantified" <~ Sets.toList (Sets.difference (var "freeInType") (var "alreadyQuantified")) $
             -- Prepend the outer type lambda vars to ensure they come first (maintaining order)
             Lists.concat2 (var "usedTypeVars") (Lists.filter ("tv" ~> Logic.not (Sets.member (var "tv") (Sets.fromList $ var "usedTypeVars"))) (var "allUnquantified"))) $
         -- Wrap the term in lambdas for each captured term variable (outermost to innermost)
         "wrappedTerm" <~ Lists.foldl
           ("body" ~> "lv" ~>
             Core.termFunction $ Core.functionLambda $ Core.lambda (Pairs.first (var "lv")) (Pairs.second (var "lv")) (var "body"))
           (var "newTerm")
           (Lists.reverse $ var "capturedTermVars") $
         -- Compute the wrapped type:
         -- 1. Add captured type variables to the type scheme's variables list
         -- 2. Prepend captured term variable types to the type scheme's body
         -- Original: forall vars. T -> becomes -> forall (capturedTypeVars ++ vars). T1 -> T2 -> ... -> T
         "wrappedType" <~ optCases (Core.bindingType $ var "binding")
           Phantoms.nothing  -- No type scheme, keep as nothing
           ("ts" ~>
             "origType" <~ Core.typeSchemeType (var "ts") $
             "origTypeVars" <~ Core.typeSchemeVariables (var "ts") $
             -- Add captured type vars to the front of the type scheme variables
             "newTypeVars" <~ Lists.concat2 (var "capturedTypeVars") (var "origTypeVars") $
             -- Prepend captured term variable types to the body type
             "newType" <~ Lists.foldl
               ("innerType" ~> "lv" ~>
                 optCases (Pairs.second $ var "lv")
                   -- No domain type, skip this variable in type wrapping
                   (var "innerType")
                   -- Has domain type, prepend it
                   ("domainType" ~> Core.typeFunction $ Core.functionType (var "domainType") (var "innerType")))
               (var "origType")
               (Lists.reverse $ var "capturedTermVars") $
             Phantoms.just $ Core.typeScheme (var "newTypeVars") (var "newType") (Core.typeSchemeConstraints $ var "ts")) $
         -- Record (name, wrappedTerm, capturedTermVarNames as list, capturedTypeVars, wrappedType)
         "hoistedInfo" <~ Phantoms.tuple5 (var "bindingName") (var "wrappedTerm") (Lists.map ("lv" ~> Pairs.first (var "lv")) (var "capturedTermVars")) (var "capturedTypeVars") (var "wrappedType") $
         pair
           (Lists.cons (var "hoistedInfo") (Lists.concat2 (var "innerHoisted") (var "hoisted")))
           (pair (var "newReserved") (var "kept")))
        -- Don't hoist: keep in place, but still add inner hoisted bindings
        ("processedBinding" <~ Core.binding (Core.bindingName $ var "binding") (var "newTerm") (Core.bindingType $ var "binding") $
         pair
           (Lists.concat2 (var "innerHoisted") (var "hoisted"))
           (pair (var "newReserved") (Lists.cons (var "processedBinding") (var "kept"))))),

    -- Replace references to hoisted bindings with type applications and term applications
    -- hoistedInfoList: list of (name, wrappedTerm, capturedTermVars, capturedTypeVars, wrappedType)
    "replaceReferences">: ("hoistedInfoList" ~> "term" ~>
      Rewriting.rewriteTerm
        @@ ("recurse" ~> "t" ~>
          cases _Term (var "t")
            (Just $ var "recurse" @@ var "t") [
            _Term_variable>>: "varName" ~>
              -- Check if this variable is one of our hoisted bindings
              "matchingInfo" <~ Lists.filter ("info" ~> Equality.equal (var "varName") (var "getInfoName" @@ var "info")) (var "hoistedInfoList") $
              Logic.ifElse (Lists.null (var "matchingInfo"))
                (var "t")  -- Not a hoisted binding, leave unchanged
                -- Replace with type applications and term applications to captured variables
                ("info" <~ Lists.head (var "matchingInfo") $
                 "capturedTermVars" <~ var "getInfoCapturedTermVars" @@ var "info" $
                 "capturedTypeVars" <~ var "getInfoCapturedTypeVars" @@ var "info" $
                 -- First apply type arguments for captured type variables
                 "withTypeApps" <~ Lists.foldl
                   ("fn" ~> "typeVarName" ~>
                     Core.termTypeApplication $ Core.typeApplicationTerm (var "fn") (Core.typeVariable $ var "typeVarName"))
                   (Core.termVariable $ var "varName")
                   (var "capturedTypeVars") $
                 -- Then apply term arguments for captured term variables
                 Lists.foldl
                   ("fn" ~> "capturedName" ~>
                     Core.termApplication $ Core.application (var "fn") (Core.termVariable $ var "capturedName"))
                   (var "withTypeApps")
                   (var "capturedTermVars"))])
        @@ var "term"),

    -- Helper to extract name from hoisted info tuple (5-tuple: name, term, capturedTermVars, capturedTypeVars, type)
    "getInfoName">: ("info" ~>
      Pairs.first (var "info")),

    -- Helper to extract captured term vars from hoisted info tuple
    "getInfoCapturedTermVars">: ("info" ~>
      Pairs.first (Pairs.second (Pairs.second (var "info")))),

    -- Helper to extract captured type vars from hoisted info tuple
    "getInfoCapturedTypeVars">: ("info" ~>
      Pairs.first (Pairs.second (Pairs.second (Pairs.second (var "info")))))] $

  -- Process all top-level bindings (no type variables or lambda variables in scope at top level)
  "emptyTypeVars" <~ list ([] :: [TTerm Name]) $
  "emptyLambdaVars" <~ list ([] :: [TTerm (Name, Y.Maybe Type)]) $
  "result" <~ Lists.foldl
    (var "processBinding" @@ var "emptyTypeVars" @@ var "emptyLambdaVars")
    (pair (var "emptyHoisted") (pair (var "topLevelNames") (list ([] :: [TTerm Binding]))))
    (Core.letBindings $ var "let0") $
  "hoistedInfo" <~ Lists.reverse (Pairs.first $ var "result") $
  "reservedNames" <~ Pairs.first (Pairs.second $ var "result") $
  "keptBindings" <~ Lists.reverse (Pairs.second (Pairs.second $ var "result")) $

  -- Process the body
  "bodyResult" <~ var "processTermForHoisting" @@ var "emptyTypeVars" @@ var "emptyLambdaVars" @@ var "reservedNames" @@ (Core.letBody $ var "let0") $
  "hoistedFromBody" <~ Lists.reverse (Pairs.first $ var "bodyResult") $
  "processedBody" <~ Pairs.second (Pairs.second $ var "bodyResult") $

  -- Combine all hoisted info
  "allHoistedInfo" <~ Lists.concat2 (var "hoistedInfo") (var "hoistedFromBody") $

  -- Convert hoisted info to bindings (name, wrappedTerm, type)
  -- The 5-tuple is: (name, wrappedTerm, capturedTermVars, capturedTypeVars, wrappedType)
  "hoistedBindings" <~ Lists.map
    ("info" ~>
      "name" <~ var "getInfoName" @@ var "info" $
      "wrappedTerm" <~ Pairs.first (Pairs.second (var "info")) $
      -- wrappedType is the 5th element: second.second.second.second
      "wrappedType" <~ Pairs.second (Pairs.second (Pairs.second (Pairs.second (var "info")))) $
      Core.binding (var "name") (var "wrappedTerm") (var "wrappedType"))
    (var "allHoistedInfo") $

  -- No need to replace references at the outer level because all references
  -- have already been replaced during the recursive processing

  -- Combine: hoisted bindings first, then kept bindings
  "allBindings" <~ Lists.concat2 (var "hoistedBindings") (var "keptBindings") $

  Core.let_ (var "allBindings") (var "processedBody")

-- | Transform a let-term by pulling all polymorphic let bindings to the top level
hoistPolymorphicLetBindings :: TBinding (Let -> Let)
hoistPolymorphicLetBindings = define "hoistPolymorphicLetBindings" $
  doc ("Transform a let-term by pulling all polymorphic let bindings to the top level."
    <> " This is useful to ensure that polymorphic bindings are not nested within other terms,"
    <> " which is unsupported by certain targets such as Java."
    <> " Polymorphic bindings are those with a non-empty list of type scheme variables."
    <> " If a hoisted binding captures lambda-bound variables from an enclosing scope,"
    <> " the binding is wrapped in lambdas for those variables, and references are replaced"
    <> " with applications."
    <> " Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first.") $
  hoistLetBindings @@ false

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
  "emptyIx" <~ Typing.inferenceContext (Phantoms.map M.empty) (Phantoms.map M.empty) (Phantoms.map M.empty) (Phantoms.map M.empty) false $
  "emptyTx" <~ Typing.typeContext Maps.empty Maps.empty Sets.empty Sets.empty (var "emptyIx") $
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
