module Hydra.Sources.Kernel.Terms.Reduction where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  alphaConvert, betaReduceType, contractTerm, countPrimitiveInvocations, etaReduceTerm, etaExpandTerm, etaExpandTermNew, etaExpansionArity, etaExpandTypedTerm,
  reduceTerm, termIsClosed, termIsValue)
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

import qualified Hydra.Sources.Kernel.Terms.Arity as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking as Checking
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Hoisting as Hoisting
import qualified Hydra.Sources.Kernel.Terms.Inference as Inference
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations


ns :: Namespace
ns = Namespace "hydra.reduction"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Arity.ns, Checking.ns, ExtractCore.ns, Hoisting.ns, Inference.ns, Lexical.ns,
      Rewriting.ns,
      Schemas.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just "Functions for reducing terms and types, i.e. performing computations."
  where
   elements = [
     toBinding alphaConvert,
     toBinding betaReduceType,
     toBinding contractTerm,
     toBinding countPrimitiveInvocations,
     toBinding etaReduceTerm,
     toBinding etaExpandTerm,
     toBinding etaExpandTermNew,
     toBinding etaExpansionArity,
     toBinding etaExpandTypedTerm,
     toBinding reduceTerm,
     toBinding termIsClosed,
     toBinding termIsValue]

alphaConvert :: TBinding (Name -> Name -> Term -> Term)
alphaConvert = define "alphaConvert" $
  doc "Alpha convert a variable in a term" $
  "vold" ~> "vnew" ~> "term" ~>
    Rewriting.replaceFreeTermVariable @@ var "vold" @@ (Core.termVariable $ var "vnew") @@ var "term"

-- Note: this is eager beta reduction, in that we always descend into subtypes,
--       and always reduce the right-hand side of an application prior to substitution
betaReduceType :: TBinding (Type -> Flow Graph Type)
betaReduceType = define "betaReduceType" $
  doc "Eagerly beta-reduce a type by substituting type arguments into type lambdas" $
  "typ" ~>
  "reduceApp" <~ ("app" ~>
    "lhs" <~ Core.applicationTypeFunction (var "app") $
    "rhs" <~ Core.applicationTypeArgument (var "app") $
    cases _Type (var "lhs") Nothing [
      _Type_annotated>>: "at" ~>
        "a" <<~ var "reduceApp" @@ (Core.applicationType
          (Core.annotatedTypeBody $ var "at")
          (var "rhs")) $
        Flows.pure $ Core.typeAnnotated $ Core.annotatedType (var "a") (Core.annotatedTypeAnnotation $ var "at"),
      _Type_forall>>: "ft" ~>
        betaReduceType @@ (Rewriting.replaceFreeTypeVariable
          @@ (Core.forallTypeParameter $ var "ft")
          @@ var "rhs"
          @@ (Core.forallTypeBody $ var "ft")),
      _Type_variable>>: "name" ~>
        "t'" <<~ Schemas.requireType @@ var "name" $
        betaReduceType @@ (Core.typeApplication $ Core.applicationType (var "t'") (var "rhs"))]) $
  "mapExpr" <~ ("recurse" ~> "t" ~>
    "findApp" <~ ("r" ~> cases _Type (var "r")
      (Just $ Flows.pure $ var "r") [
      _Type_application>>: "a" ~> var "reduceApp" @@ var "a"]) $
    "r" <<~ var "recurse" @@ var "t" $
    var "findApp" @@ var "r") $
  Rewriting.rewriteTypeM @@ var "mapExpr" @@ var "typ"

contractTerm :: TBinding (Term -> Term)
contractTerm = define "contractTerm" $
  doc ("Apply the special rules:\n"
    <> "    ((\\x.e1) e2) == e1, where x does not appear free in e1\n"
    <> "  and\n"
    <> "     ((\\x.e1) e2) = e1[x/e2]\n"
    <> "These are both limited forms of beta reduction which help to \"clean up\" a term without fully evaluating it.") $
  "term" ~>
  "rewrite" <~ ("recurse" ~> "t" ~>
    "rec" <~ var "recurse" @@ var "t" $
    cases _Term (var "rec")
      (Just $ var "rec") [
      _Term_application>>: "app" ~>
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        cases _Term (Rewriting.deannotateTerm @@ var "lhs")
          (Just $ var "rec") [
          _Term_function>>: "f" ~> cases _Function (var "f")
            (Just $ var "rec") [
            _Function_lambda>>: "l" ~>
              "v" <~ Core.lambdaParameter (var "l") $
              "body" <~ Core.lambdaBody (var "l") $
              Logic.ifElse (Rewriting.isFreeVariableInTerm @@ var "v" @@ var "body")
                (var "body")
                (Rewriting.replaceFreeTermVariable @@ var "v" @@ var "rhs" @@ var "body")]]]) $
  Rewriting.rewriteTerm @@ var "rewrite" @@ var "term"

-- For demo purposes. This should be generalized to enable additional side effects of interest.
countPrimitiveInvocations :: TBinding Bool
countPrimitiveInvocations = define "countPrimitiveInvocations" true

-- TODO: see notes on etaExpansionArity
etaExpandTerm :: TBinding (Graph -> Term -> Term)
etaExpandTerm = define "etaExpandTerm" $
  doc ("Recursively transform arbitrary terms like 'add 42' into terms like '\\x.add 42 x', in which the implicit"
    <> " parameters of primitive functions and eliminations are made into explicit lambda parameters."
    <> " Variable references are not expanded."
    <> " This is useful for targets like Python with weaker support for currying than Hydra or Haskell."
    <> " Note: this is a \"trusty\" function which assumes the graph is well-formed, i.e. no dangling references.") $
  "graph" ~> "term" ~>
  "expand" <~ ("args" ~> "arity" ~> "t" ~>
    "apps" <~ Lists.foldl
      ("lhs" ~> "arg" ~> Core.termApplication $ Core.application (var "lhs") (var "arg"))
      (var "t")
      (var "args") $
    "is" <~ Logic.ifElse (Equality.lte (var "arity") (Lists.length $ var "args"))
      (list ([] :: [TTerm Int]))
      (Math.range (int32 1) (Math.sub (var "arity") (Lists.length $ var "args"))) $
    "pad" <~ ("indices" ~> "t" ~>
      Logic.ifElse (Lists.null $ var "indices")
        (var "t")
        (Core.termFunction $ Core.functionLambda $
          Core.lambda (Core.name $ Strings.cat2 (string "v") (Literals.showInt32 $ Lists.head $ var "indices")) nothing $
            var "pad" @@ Lists.tail (var "indices") @@
              (Core.termApplication $ Core.application (var "t") $ Core.termVariable $
                Core.name $ Strings.cat2 (string "v") (Literals.showInt32 $ Lists.head $ var "indices")))) $
    var "pad" @@ var "is" @@ var "apps") $
  "rewrite" <~ ("args" ~> "recurse" ~> "t" ~>
    "afterRecursion" <~ ("term" ~>
      var "expand" @@ var "args" @@ (etaExpansionArity @@ var "graph" @@ var "term") @@ var "term") $
    "t2" <~ Rewriting.detypeTerm @@ var "t" $
    cases _Term (var "t2")
      (Just $ var "afterRecursion" @@ (var "recurse" @@ var "t2")) [
      _Term_application>>: "app" ~>
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        "erhs" <~ var "rewrite" @@ (list ([] :: [TTerm Term])) @@ var "recurse" @@ var "rhs" $
        var "rewrite" @@ (Lists.cons (var "erhs") (var "args")) @@ var "recurse" @@ var "lhs"]) $
  contractTerm @@ (Rewriting.rewriteTerm @@ (var "rewrite" @@ list ([] :: [TTerm Term])) @@ var "term")

-- | Eta-expand a term using TypeContext for type lookups. This is a pure function that does not
-- require type inference, instead relying on the TypeContext being properly populated with types
-- for all in-scope variables.
--
-- The key differences from etaExpandTypedTerm:
-- 1. Pure (no Flow monad needed) because we look up types directly from the context
-- 2. Manually tracks TypeContext when entering lambdas, lets, and type lambdas
-- 3. Preserves existing type annotations where possible
etaExpandTermNew :: TBinding (TypeContext -> Term -> Term)
etaExpandTermNew = define "etaExpandTermNew" $
  doc ("Recursively transform terms to eliminate partial application, e.g. 'add 42' becomes '\\x.add 42 x'."
    <> " Uses the TypeContext to look up types for arity calculation."
    <> " Bare primitives and variables are NOT expanded; eliminations and partial applications are."
    <> " This version properly tracks the TypeContext through nested scopes.") $
  "tx0" ~> "term0" ~>

  -- termArityWithContext: compute arity of a term using TypeContext for lookups
  "termArityWithContext" <~ ("tx" ~> "term" ~>
    cases _Term (var "term")
      (Just $ int32 0) [
      _Term_annotated>>: "at" ~>
        var "termArityWithContext" @@ var "tx" @@ Core.annotatedTermBody (var "at"),
      _Term_application>>: "app" ~>
        Math.sub (var "termArityWithContext" @@ var "tx" @@ Core.applicationFunction (var "app")) (int32 1),
      _Term_function>>: "f" ~> cases _Function (var "f") Nothing [
        _Function_elimination>>: constant $ int32 1,
        _Function_lambda>>: constant $ int32 0,
        _Function_primitive>>: "name" ~>
          optCases (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ Typing.typeContextInferenceContext $ var "tx"))
            (int32 0) Arity.typeSchemeArity],
      _Term_let>>: "l" ~>
        var "termArityWithContext"
          @@ (Schemas.extendTypeContextForLet @@ constant (constant nothing) @@ var "tx" @@ var "l")
          @@ Core.letBody (var "l"),
      _Term_typeLambda>>: "tl" ~>
        var "termArityWithContext"
          @@ (Schemas.extendTypeContextForTypeLambda @@ var "tx" @@ var "tl")
          @@ Core.typeLambdaBody (var "tl"),
      _Term_typeApplication>>: "tat" ~>
        var "termArityWithContext" @@ var "tx" @@ Core.typeApplicationTermBody (var "tat"),
      _Term_variable>>: "name" ~>
        optCases (Maps.lookup (var "name") (Typing.typeContextTypes $ var "tx"))
          (int32 0) Arity.typeArity]) $

  -- expand: apply args to head and pad with lambdas if needed
  -- alwaysPad: if true, pad even when args is empty (for eliminations)
  "expand" <~ ("alwaysPad" ~> "args" ~> "arity" ~> "head" ~>
    "applied" <~ Lists.foldl
      ("lhs" ~> "arg" ~> Core.termApplication (Core.application (var "lhs") (var "arg")))
      (var "head") (var "args") $
    "numArgs" <~ Lists.length (var "args") $
    "needed" <~ Math.sub (var "arity") (var "numArgs") $
    -- Pad if: (needed > 0) AND (alwaysPad OR numArgs > 0)
    Logic.ifElse (Logic.and (Equality.gt (var "needed") (int32 0))
                            (Logic.or (var "alwaysPad") (Equality.gt (var "numArgs") (int32 0))))
      -- Pad with lambdas: first build fully applied term, then wrap with lambdas
      ("indices" <~ Math.range (int32 1) (var "needed") $
       -- Step 1: Build fully applied term: applied v1 v2 ... vn
       "fullyApplied" <~ Lists.foldl
         ("body" ~> "i" ~>
           "vn" <~ Core.name (Strings.cat2 (string "v") (Literals.showInt32 $ var "i")) $
           Core.termApplication $ Core.application (var "body") (Core.termVariable $ var "vn"))
         (var "applied") (var "indices") $
       -- Step 2: Wrap with lambdas from inside out by reversing indices: \v1 -> \v2 -> ... -> fullyApplied
       -- Using foldl with reversed indices gives us: for [2,1], wrap v2 first (innermost), then v1 (outermost)
       Lists.foldl
         ("body" ~> "i" ~>
           "vn" <~ Core.name (Strings.cat2 (string "v") (Literals.showInt32 $ var "i")) $
           Core.termFunction $ Core.functionLambda $ Core.lambda (var "vn") nothing (var "body"))
         (var "fullyApplied") (Lists.reverse (var "indices")))
      (var "applied")) $

  -- rewriteWithArgs: tracks accumulated args as we descend into application spines
  -- This is a recursive let binding that calls itself directly
  "rewriteWithArgs" <~ ("args" ~> "tx" ~> "term" ~>
    -- recurse is shorthand for calling rewriteWithArgs with empty args
    "recurse" <~ ("tx1" ~> "term1" ~> var "rewriteWithArgs" @@ list ([] :: [TTerm Term]) @@ var "tx1" @@ var "term1") $

    -- afterRecursion: apply expansion logic after subterms have been processed
    "afterRecursion" <~ ("trm" ~>
      "arity" <~ var "termArityWithContext" @@ var "tx" @@ var "trm" $
      var "expand" @@ false @@ var "args" @@ var "arity" @@ var "trm") $

    -- Helper for processing fields (used in records, unions, but NOT case statement branches)
    "forField" <~ ("f" ~> Core.fieldWithTerm (var "recurse" @@ var "tx" @@ Core.fieldTerm (var "f")) (var "f")) $

    -- Helper for case statement branches - forces expansion of the branch body
    -- This is needed because case branches represent partial function values that need full expansion
    "forCaseBranch" <~ ("f" ~>
      "branchBody" <~ var "recurse" @@ var "tx" @@ Core.fieldTerm (var "f") $
      "arty" <~ var "termArityWithContext" @@ var "tx" @@ var "branchBody" $
      Core.fieldWithTerm (var "expand" @@ true @@ list ([] :: [TTerm Term]) @@ var "arty" @@ var "branchBody") (var "f")) $

    -- Helper for eliminations
    "forElimination" <~ ("elm" ~> cases _Elimination (var "elm") Nothing [
      _Elimination_record>>: "p" ~> Core.eliminationRecord (var "p"),
      _Elimination_union>>: "cs" ~> Core.eliminationUnion $ Core.caseStatement
        (Core.caseStatementTypeName $ var "cs")
        (Maybes.map ("t1" ~> var "recurse" @@ var "tx" @@ var "t1") (Core.caseStatementDefault $ var "cs"))
        (Lists.map (var "forCaseBranch") (Core.caseStatementCases $ var "cs")),
      _Elimination_wrap>>: "nm" ~> Core.eliminationWrap $ var "nm"]) $

    -- Helper for maps
    "forMap" <~ ("mp" ~>
      "forPair" <~ ("pr" ~> pair (var "recurse" @@ var "tx" @@ Pairs.first (var "pr"))
                                 (var "recurse" @@ var "tx" @@ Pairs.second (var "pr"))) $
      Maps.fromList $ Lists.map (var "forPair") $ Maps.toList $ var "mp") $

    cases _Term (var "term") Nothing [
      -- Annotated: recurse into body, preserve annotation
      _Term_annotated>>: "at" ~> var "afterRecursion" @@ Core.termAnnotated (Core.annotatedTerm
        (var "recurse" @@ var "tx" @@ Core.annotatedTermBody (var "at"))
        (Core.annotatedTermAnnotation $ var "at")),

      -- Application: process RHS with empty args, then descend into LHS with RHS added to args
      _Term_application>>: "app" ~>
        "rhs" <~ var "rewriteWithArgs" @@ list ([] :: [TTerm Term]) @@ var "tx" @@ Core.applicationArgument (var "app") $
        var "rewriteWithArgs" @@ Lists.cons (var "rhs") (var "args") @@ var "tx" @@ Core.applicationFunction (var "app"),

      -- Either: recurse into left or right
      _Term_either>>: "e" ~> var "afterRecursion" @@ Core.termEither (Eithers.either_
        ("l" ~> left $ var "recurse" @@ var "tx" @@ var "l")
        ("r" ~> right $ var "recurse" @@ var "tx" @@ var "r")
        (var "e")),

      -- Function: special handling
      _Term_function>>: "fn" ~> cases _Function (var "fn")
        Nothing [
        _Function_elimination>>: "elm" ~>
          -- Recurse into the elimination, then expand
          -- Only pad union eliminations (case statements); record/wrap eliminations are handled by the language coder
          "padElim" <~ cases _Elimination (var "elm") Nothing [
            _Elimination_record>>: "_" ~> false,
            _Elimination_union>>: "_" ~> true,
            _Elimination_wrap>>: "_" ~> false] $
          var "expand" @@ var "padElim" @@ var "args" @@ (int32 1) @@
            (Core.termFunction $ Core.functionElimination $ var "forElimination" @@ var "elm"),
        _Function_lambda>>: "lm" ~>
          "tx1" <~ Schemas.extendTypeContextForLambda @@ var "tx" @@ var "lm" $
          "body" <~ var "rewriteWithArgs" @@ list ([] :: [TTerm Term]) @@ var "tx1" @@ Core.lambdaBody (var "lm") $
          "result" <~ Core.termFunction (Core.functionLambda $
            Core.lambda (Core.lambdaParameter $ var "lm") (Core.lambdaDomain $ var "lm") (var "body")) $
          "arty" <~ var "termArityWithContext" @@ var "tx" @@ var "result" $
          var "expand" @@ false @@ var "args" @@ var "arty" @@ var "result",
        _Function_primitive>>: "pn" ~>
          -- Don't expand if bare
          "arty" <~ var "termArityWithContext" @@ var "tx" @@ var "term" $
          var "expand" @@ false @@ var "args" @@ var "arty" @@ var "term"],

      -- Let: extend context for bindings and body
      _Term_let>>: "lt" ~>
        "tx1" <~ Schemas.extendTypeContextForLet @@ constant (constant nothing) @@ var "tx" @@ var "lt" $
        "mapBinding" <~ ("b" ~> Core.binding
          (Core.bindingName $ var "b")
          (var "rewriteWithArgs" @@ list ([] :: [TTerm Term]) @@ var "tx1" @@ Core.bindingTerm (var "b"))
          (Core.bindingType $ var "b")) $
        "result" <~ Core.termLet (Core.let_
          (Lists.map (var "mapBinding") (Core.letBindings $ var "lt"))
          (var "rewriteWithArgs" @@ list ([] :: [TTerm Term]) @@ var "tx1" @@ Core.letBody (var "lt"))) $
        var "afterRecursion" @@ var "result",

      -- List: recurse into elements
      _Term_list>>: "els" ~> var "afterRecursion" @@
        (Core.termList $ Lists.map ("el" ~> var "recurse" @@ var "tx" @@ var "el") (var "els")),

      -- Literal: no recursion needed
      _Term_literal>>: "v" ~> Core.termLiteral $ var "v",

      -- Map: recurse into keys and values
      _Term_map>>: "mp" ~> var "afterRecursion" @@ (Core.termMap $ var "forMap" @@ var "mp"),

      -- Maybe: recurse into value if present
      _Term_maybe>>: "mb" ~> var "afterRecursion" @@
        (Core.termMaybe $ Maybes.map ("v" ~> var "recurse" @@ var "tx" @@ var "v") (var "mb")),

      -- Pair: recurse into both elements
      _Term_pair>>: "pr" ~> var "afterRecursion" @@ (Core.termPair $ pair
        (var "recurse" @@ var "tx" @@ Pairs.first (var "pr"))
        (var "recurse" @@ var "tx" @@ Pairs.second (var "pr"))),

      -- Record: recurse into fields
      _Term_record>>: "rc" ~> var "afterRecursion" @@ (Core.termRecord $ Core.record
        (Core.recordTypeName $ var "rc")
        (Lists.map (var "forField") (Core.recordFields $ var "rc"))),

      -- Set: recurse into elements
      _Term_set>>: "st" ~> var "afterRecursion" @@
        (Core.termSet $ Sets.fromList $ Lists.map ("el" ~> var "recurse" @@ var "tx" @@ var "el") $ Sets.toList (var "st")),

      -- TypeApplication: recurse into body
      _Term_typeApplication>>: "tt" ~> var "afterRecursion" @@ (Core.termTypeApplication $ Core.typeApplicationTerm
        (var "recurse" @@ var "tx" @@ Core.typeApplicationTermBody (var "tt"))
        (Core.typeApplicationTermType $ var "tt")),

      -- TypeLambda: extend context for body
      _Term_typeLambda>>: "tl" ~>
        "tx1" <~ Schemas.extendTypeContextForTypeLambda @@ var "tx" @@ var "tl" $
        "result" <~ Core.termTypeLambda (Core.typeLambda
          (Core.typeLambdaParameter $ var "tl")
          (var "rewriteWithArgs" @@ list ([] :: [TTerm Term]) @@ var "tx1" @@ Core.typeLambdaBody (var "tl"))) $
        var "afterRecursion" @@ var "result",

      -- Union: recurse into injection field
      _Term_union>>: "inj" ~> var "afterRecursion" @@ (Core.termUnion $ Core.injection
        (Core.injectionTypeName $ var "inj")
        (var "forField" @@ Core.injectionField (var "inj"))),

      -- Unit: no recursion needed
      _Term_unit>>: constant Core.termUnit,

      -- Variable: don't expand if bare
      _Term_variable>>: "vn" ~>
        "arty" <~ var "termArityWithContext" @@ var "tx" @@ var "term" $
        var "expand" @@ false @@ var "args" @@ var "arty" @@ var "term",

      -- Wrap: recurse into body
      _Term_wrap>>: "wt" ~> var "afterRecursion" @@ (Core.termWrap $ Core.wrappedTerm
        (Core.wrappedTermTypeName $ var "wt")
        (var "recurse" @@ var "tx" @@ Core.wrappedTermBody (var "wt")))]) $

  contractTerm @@ (var "rewriteWithArgs" @@ list ([] :: [TTerm Term]) @@ var "tx0" @@ var "term0")

-- TODO: this function probably needs to be replaced with a function which takes not only a Graph, but a TypeContext.
--       etaExpansionArity won't give the correct answer unless it has access to the full lexical environment
--       of each subterm in which it is applied, including lambda-bound variables as well as nested let-bound variables.
--       The new function need not be monadic, because we don't need to call typeOf; it just needs accurate type lookups.
etaExpansionArity :: TBinding (Graph -> Term -> Int)
etaExpansionArity = define "etaExpansionArity" $
  doc ("Calculate the arity for eta expansion"
    <> " Note: this is a \"trusty\" function which assumes the graph is well-formed, i.e. no dangling references.") $
  "graph" ~> "term" ~> cases _Term (var "term")
    (Just $ int32 0) [
    _Term_annotated>>: "at" ~>
      etaExpansionArity @@ var "graph" @@ Core.annotatedTermBody (var "at"),
    _Term_application>>: "app" ~> Math.sub
      (etaExpansionArity @@ var "graph" @@ Core.applicationFunction (var "app"))
      (int32 1),
    _Term_function>>: "f" ~> cases _Function (var "f")
      Nothing [
      _Function_elimination>>: constant $ int32 1,
      _Function_lambda>>: constant $ int32 0,
      _Function_primitive>>: "name" ~> Arity.primitiveArity
        @@ (Maybes.fromJust (Lexical.lookupPrimitive @@ var "graph" @@ var "name"))],
    _Term_typeLambda>>: "ta" ~> etaExpansionArity @@ var "graph" @@ Core.typeLambdaBody (var "ta"),
    _Term_typeApplication>>: "tt" ~> etaExpansionArity @@ var "graph" @@ Core.typeApplicationTermBody (var "tt"),
    _Term_variable>>: "name" ~>
      -- Note: we assume that the graph is fully typed.
      Maybes.maybe (int32 0)
        ("ts" ~> Arity.typeArity @@ (Core.typeSchemeType $ var "ts"))
        (Maybes.bind
          (Lexical.lookupElement @@ var "graph" @@ var "name")
          ("b" ~> Core.bindingType $ var "b"))]

-- TODO: add lambda domains as part of the rewriting process, so inference does not need to be performed again.
etaExpandTypedTerm :: TBinding (TypeContext -> Term -> Flow s Term)
etaExpandTypedTerm = define "etaExpandTypedTerm" $
  doc ("Recursively transform arbitrary terms like 'add 42' into terms like '\\x.add 42 x',"
    <> " eliminating partial application. Variable references are not expanded."
    <> " This is useful for targets like Python with weaker support for currying than Hydra or Haskell."
    <> " Note: this is a \"trusty\" function which assumes the graph is well-formed, i.e. no dangling references."
    <> " It also assumes that type inference has already been performed."
    <> " After eta expansion, type inference needs to be performed again, as new, untyped lambdas may have been added."
    ) $
  "tx0" ~> "term0" ~>
  "rewrite" <~ ("topLevel" ~> "forced" ~> "typeArgs" ~> "recurse" ~> "tx" ~> "term" ~>
    "rewriteSpine" <~ ("term" ~> cases _Term (var "term")
      (Just $ var "rewrite" @@ false @@ false @@ list ([] :: [TTerm Type]) @@ var "recurse" @@ var "tx" @@ var "term") [
      _Term_annotated>>: "at" ~>
        "body" <<~ var "rewriteSpine" @@ Core.annotatedTermBody (var "at") $
        "ann" <~ Core.annotatedTermAnnotation (var "at") $
        produce (Core.termAnnotated $ Core.annotatedTerm (var "body") (var "ann")),
      _Term_application>>: "a" ~>
        "l" <~ Logic.ifElse false (list [Core.typeLiteral Core.literalTypeString]) (list ([] :: [TTerm Type])) $ -- TODO: hack for type checking
        "lhs" <<~ var "rewriteSpine" @@ Core.applicationFunction (var "a") $
        "rhs" <<~ var "rewrite" @@ true @@ false @@ var "l" @@ var "recurse" @@ var "tx" @@ Core.applicationArgument (var "a") $
        produce (Core.termApplication $ Core.application (var "lhs") (var "rhs")),
      _Term_typeApplication>>: "tat" ~>
        "body" <<~ var "rewriteSpine" @@ Core.typeApplicationTermBody (var "tat") $
        "typ" <~ Core.typeApplicationTermType (var "tat") $
        produce (Core.termTypeApplication $ Core.typeApplicationTerm (var "body") (var "typ"))]) $

    -- Arity as provided by type checking, but with exceptions which give us the desired behavior for
    -- targets including Python. Remove the special cases and see which regression tests fail.
    -- TODO: this function is moving toward "syntactic" arity which does not require type checking; only
    --       type lookups.
    "arityOf" <~ ("tx" ~> "term" ~>
      "dflt" <~ Flows.map Arity.typeArity (Checking.typeOf @@ var "tx" @@ list ([] :: [TTerm Type]) @@ var "term") $
      "forFunction" <~ ("tx" ~> "f" ~> cases _Function (var "f")
        Nothing [
        _Function_elimination>>: constant $ produce $ int32 1,
        _Function_lambda>>: "l" ~>
          "txl" <~ Schemas.extendTypeContextForLambda @@ var "tx" @@ var "l" $
          var "arityOf" @@ var "txl" @@ Core.lambdaBody (var "l"),
        _Function_primitive>>: "name" ~> Flows.map
          (Arity.typeSchemeArity)
          (Lexical.requirePrimitiveType @@ var "tx" @@ var "name")]) $
--      trace ("arityOf(" ++ (ShowCore.term @@ var "term") ++ ")") $
      cases _Term (var "term")
        (Just $ var "dflt") [
        _Term_annotated>>: "at" ~> var "arityOf" @@ var "tx" @@ Core.annotatedTermBody (var "at"),
        -- Note: No _Term_application case - the dflt fallback using typeOf is correct.
        -- We can't use arityOf(f) - 1 because that doesn't account for higher-order functions
        -- like identity where (id x) has the same arity as x.
        _Term_function>>: "f" ~> var "forFunction" @@ var "tx" @@ var "f",
        _Term_let>>: "l" ~>
          "txl" <~ Schemas.extendTypeContextForLet @@ constant (constant nothing) @@ var "tx" @@ var "l" $
          var "arityOf" @@ var "txl" @@ Core.letBody (var "l"),
        _Term_typeApplication>>: "tat" ~> var "arityOf" @@ var "tx" @@ Core.typeApplicationTermBody (var "tat"),
        _Term_typeLambda>>: "tl" ~>
          "txt" <~ Schemas.extendTypeContextForTypeLambda @@ var "tx" @@ var "tl" $
          var "arityOf" @@ var "txt" @@ Core.typeLambdaBody (var "tl"),
        _Term_variable>>: "name" ~> optCases (Maps.lookup (var "name") (Typing.typeContextTypes $ var "tx"))
          -- Variable not in typeContextTypes; use typeOf with CURRENT context and variable term as fallback
          -- This can happen with local let bindings that aren't yet in scope during eta expansion
          (Flows.map Arity.typeArity (Checking.typeOf @@ var "tx" @@ list ([] :: [TTerm Type]) @@ Core.termVariable (var "name")))
          ("t" ~> produce $ Arity.typeArity @@ var "t")]) $

--    "arityOf" <~ ("term" ~> Flows.map
--      (Arity.typeArity)
--      (Checking.typeOf @@ var "tx" @@ list ([] :: [TTerm Type]) @@ var "term")) $

    "extraVariables" <~ ("n" ~> Lists.map ("i" ~> Core.name $ Strings.cat2 (string "v") (Literals.showInt32 $ var "i")) $
      Math.range (int32 1) (var "n")) $
    "pad" <~ ("vars" ~> "body" ~>
      Logic.ifElse (Lists.null $ var "vars")
        (var "body")
        (Core.termFunction $ Core.functionLambda $ Core.lambda (Lists.head $ var "vars") nothing $ var "pad"
          @@ Lists.tail (var "vars")
          @@ (Core.termApplication $ Core.application (var "body") $ Core.termVariable $ Lists.head $ var "vars"))) $
    "padn" <~ ("n" ~> "body" ~> var "pad" @@ (var "extraVariables" @@ var "n") @@ var "body") $

    "unwind" <~ ("term" ~> Lists.foldl
      ("e" ~> "t" ~> Core.termTypeApplication (Core.typeApplicationTerm (var "e") (var "t")))
      (var "term") (var "typeArgs")) $

    "forceExpansion" <~ ("t" ~>
      "typ" <<~ Checking.typeOf @@ var "tx" @@ list ([] :: [TTerm Type]) @@ var "t" $
      "arity" <~ Arity.typeArity @@ var "typ" $
      produce $ var "padn" @@ var "arity" @@ (var "unwind" @@ var "t")) $

    "recurseOrForce" <~ ("term" ~> Logic.ifElse (var "forced")
      (var "forceExpansion" @@ var "term")
      (var "recurse" @@ var "tx" @@ (var "unwind" @@ var "term"))) $

    "forCase" <~ ("f" ~>
      "r" <<~ var "rewrite" @@ false @@ true @@ list ([] :: [TTerm Type]) @@ var "recurse" @@ var "tx" @@ Core.fieldTerm (var "f") $
      produce $ Core.fieldWithTerm (var "r") (var "f")) $

    -- Forcing case statement branches is intended for Python, where we cannot accept a branch which is simply
    -- a variable or a primitive reference; we need to expand these to lambdas.
    -- TODO: make this behavior configurable
    "forCaseStatement" <~ ("cs" ~>
      "tname" <~ Core.caseStatementTypeName (var "cs") $
      "dflt" <~ Core.caseStatementDefault (var "cs") $
      "cases" <~ Core.caseStatementCases (var "cs") $
      "rdflt" <<~ Flows.mapMaybe (var "rewrite" @@ false @@ false @@ list ([] :: [TTerm Type]) @@ var "recurse" @@ var "tx") (var "dflt") $
      "rcases" <<~ Flows.mapList (var "forCase") (var "cases") $
      produce $ Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
        Core.caseStatement (var "tname") (var "rdflt") (var "rcases")) $

    "forElimination" <~ ("elm" ~>
      "checkBase" <~ ("elm" ~> cases _Elimination (var "elm")
        (Just $ var "recurse" @@ var "tx" @@ var "term") [
        _Elimination_union>>: "cs" ~> var "forCaseStatement" @@ var "cs"]) $
      "base" <<~ Flows.map (var "unwind") (var "checkBase" @@ var "elm") $
      produce $ Logic.ifElse (Logic.or (var "topLevel") (var "forced"))
        (var "padn" @@ int32 1 @@ var "base")
        (var "base")) $

    cases _Term (var "term")
      (Just $ var "recurseOrForce" @@ var "term") [
      _Term_application>>: "a" ~>
        "lhs" <~ Core.applicationFunction (var "a") $
        "rhs" <~ Core.applicationArgument (var "a") $
        "rhs2" <<~ var "rewrite" @@ true @@ false @@ list ([] :: [TTerm Type]) @@ var "recurse" @@ var "tx" @@ var "rhs" $
        "lhsarity" <<~ var "arityOf" @@ var "tx" @@ var "lhs" $
        "lhs2" <<~ var "rewriteSpine" @@ var "lhs" $
        "a2" <~ Core.termApplication (Core.application (var "lhs2") (var "rhs2")) $
        produce $ Logic.ifElse (Equality.gt (var "lhsarity") (int32 1))
          (var "padn" @@ (Math.sub (var "lhsarity") (int32 1)) @@ var "a2")
          (var "a2"),
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "recurseOrForce" @@ var "term") [
        _Function_elimination>>: "elm" ~> var "forElimination" @@ var "elm",
        _Function_lambda>>: "l" ~>
          "txl" <~ Schemas.extendTypeContextForLambda @@ var "tx" @@ var "l" $
           Flows.map (var "unwind") (var "recurse" @@ var "txl" @@ var "term")],
      _Term_let>>: "l" ~>
        "txlt" <~ Schemas.extendTypeContextForLet @@ constant (constant nothing) @@ var "tx" @@ var "l" $
        var "recurse" @@ var "txlt" @@ var "term",
      _Term_typeApplication>>: "tat" ~> var "rewrite" @@ var "topLevel" @@ var "forced"
        @@ (Lists.cons (Core.typeApplicationTermType $ var "tat") (var "typeArgs"))
        @@ var "recurse" @@ var "tx"
        @@ Core.typeApplicationTermBody (var "tat"),
      _Term_typeLambda>>: "tl" ~>
        "txt" <~ Schemas.extendTypeContextForTypeLambda @@ var "tx" @@ var "tl" $
        var "recurse" @@ var "txt" @@ var "term"]) $
--  trace ("term0: " ++ (ShowCore.term @@ var "term0")) $
  Rewriting.rewriteTermWithContextM @@ (var "rewrite" @@ true @@ false @@ list ([] :: [TTerm Type])) @@ var "tx0" @@ var "term0"

etaReduceTerm :: TBinding (Term -> Term)
etaReduceTerm = define "etaReduceTerm" $
  doc "Eta-reduce a term by removing redundant lambda abstractions" $
  "term" ~>
  "noChange" <~ var "term" $
  "reduceLambda" <~ ("l" ~>
    "v" <~ Core.lambdaParameter (var "l") $
    "d" <~ Core.lambdaDomain (var "l") $
    "body" <~ Core.lambdaBody (var "l") $
    cases _Term (etaReduceTerm @@ var "body")
      (Just $ var "noChange") [
      _Term_annotated>>: "at" ~>
        var "reduceLambda" @@ (Core.lambda (var "v") (var "d") (Core.annotatedTermBody $ var "at")),
      _Term_application>>: "app" ~>
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        cases _Term (etaReduceTerm @@ var "rhs")
          (Just $ var "noChange") [
          _Term_annotated>>: "at" ~>
            var "reduceLambda" @@ (Core.lambda (var "v") (var "d") $
              Core.termApplication $ Core.application (var "lhs") (Core.annotatedTermBody $ var "at")),
          _Term_variable>>: "v1" ~>
            Logic.ifElse
              (Logic.and
                (Equality.equal (Core.unName $ var "v") (Core.unName $ var "v1"))
                (Logic.not $ Rewriting.isFreeVariableInTerm @@ var "v" @@ var "lhs"))
              (etaReduceTerm @@ var "lhs")
              (var "noChange")]]) $
  cases _Term (var "term")
    (Just $ var "noChange") [
    _Term_annotated>>: "at" ~>
      Core.termAnnotated $ Core.annotatedTerm
        (etaReduceTerm @@ (Core.annotatedTermBody $ var "at"))
        (Core.annotatedTermAnnotation $ var "at"),
    _Term_function>>: "f" ~>
      cases _Function (var "f")
        (Just $ var "noChange") [
        _Function_lambda>>: "l" ~> var "reduceLambda" @@ var "l"]]

reduceTerm :: TBinding (Bool -> Term -> Flow Graph Term)
reduceTerm = define "reduceTerm" $
  doc "A term evaluation function which is alternatively lazy or eager" $
  "eager" ~> "term" ~>
  "reduce" <~ ("eager" ~> reduceTerm @@ var "eager") $
  "doRecurse" <~ ("eager" ~> "term" ~>
    "isNonLambda" <~ ("f" ~> cases _Function (var "f")
      (Just true) [
      _Function_lambda>>: constant false]) $
    "isNonLambdaTerm" <~ cases _Term (var "term")
      (Just true) [
      _Term_function>>: "f" ~> var "isNonLambda" @@ var "f",
      -- Don't recurse into let; handle in applyIfNullary
      _Term_let>>: constant false] $
    Logic.and (var "eager") (var "isNonLambdaTerm")) $
  "reduceArg" <~ ("eager" ~> "arg" ~>
    Logic.ifElse (var "eager")
      (Flows.pure $ var "arg")
      (var "reduce" @@ false @@ var "arg")) $
  "applyToArguments" <~ ("fun" ~> "args" ~>
    Logic.ifElse (Lists.null $ var "args")
      (var "fun")
      (var "applyToArguments" @@
        (Core.termApplication $ Core.application (var "fun") (Lists.head $ var "args")) @@
        (Lists.tail $ var "args"))) $
  "applyElimination" <~ ("elm" ~> "reducedArg" ~>
    cases _Elimination (var "elm") Nothing [
      _Elimination_record>>: "proj" ~>
        "fields" <<~ ExtractCore.record @@ (Core.projectionTypeName $ var "proj") @@ (Rewriting.deannotateTerm @@ var "reducedArg") $
        "matchingFields" <~ Lists.filter
          ("f" ~> Equality.equal (Core.fieldName $ var "f") (Core.projectionField $ var "proj"))
          (var "fields") $
        Logic.ifElse
          (Lists.null $ var "matchingFields")
          (Flows.fail $ Strings.cat $ list [
            string "no such field: ",
            unwrap _Name @@ (Core.projectionField $ var "proj"),
            string " in ",
            unwrap _Name @@ (Core.projectionTypeName $ var "proj"),
            string " record"])
          (Flows.pure $ Core.fieldTerm $ Lists.head $ var "matchingFields"),
      _Elimination_union>>: "cs" ~>
        "field" <<~ ExtractCore.injection @@ (Core.caseStatementTypeName $ var "cs") @@ var "reducedArg" $
        "matchingFields" <~ Lists.filter
          ("f" ~> Equality.equal (Core.fieldName $ var "f") (Core.fieldName $ var "field"))
          (Core.caseStatementCases $ var "cs") $
        Logic.ifElse (Lists.null $ var "matchingFields")
          (Maybes.maybe
            (Flows.fail $ Strings.cat $ list [
              string "no such field ",
              unwrap _Name @@ (Core.fieldName $ var "field"),
              string " in ",
              unwrap _Name @@ (Core.caseStatementTypeName $ var "cs"),
              string " case statement"])
            (unaryFunction Flows.pure)
            (Core.caseStatementDefault $ var "cs"))
          (Flows.pure $ Core.termApplication $ Core.application
            (Core.fieldTerm $ Lists.head $ var "matchingFields")
            (Core.fieldTerm $ var "field")),
      _Elimination_wrap>>: "name" ~> ExtractCore.wrap @@ var "name" @@ var "reducedArg"]) $
  "applyIfNullary" <~ ("eager" ~> "original" ~> "args" ~>
    "stripped" <~ Rewriting.deannotateTerm @@ var "original" $
    "forElimination" <~ ("elm" ~> "args" ~>
      "arg" <~ Lists.head (var "args") $
      "remainingArgs" <~ Lists.tail (var "args") $
      "reducedArg" <<~ var "reduceArg" @@ var "eager" @@ (Rewriting.deannotateTerm @@ var "arg") $
      "reducedResult" <<~ Flows.bind (var "applyElimination" @@ var "elm" @@ var "reducedArg") (var "reduce" @@ var "eager") $
      var "applyIfNullary" @@ var "eager" @@ var "reducedResult" @@ var "remainingArgs") $
    "forLambda" <~ ("l" ~> "args" ~>
      "param" <~ Core.lambdaParameter (var "l") $
      "body" <~ Core.lambdaBody (var "l") $
      "arg" <~ Lists.head (var "args") $
      "remainingArgs" <~ Lists.tail (var "args") $
      "reducedArg" <<~ var "reduce" @@ var "eager" @@ (Rewriting.deannotateTerm @@ var "arg") $
      "reducedResult" <<~ var "reduce" @@ var "eager"
        @@ (Rewriting.replaceFreeTermVariable @@ var "param" @@ var "reducedArg" @@ var "body") $
      var "applyIfNullary" @@ var "eager" @@ var "reducedResult" @@ var "remainingArgs") $
    "forPrimitive" <~ ("prim" ~> "arity" ~> "args" ~>
      "argList" <~ Lists.take (var "arity") (var "args") $
      "remainingArgs" <~ Lists.drop (var "arity") (var "args") $
      "reducedArgs" <<~ Flows.mapList (var "reduceArg" @@ var "eager") (var "argList") $
      -- Strip annotations from reduced args so primitives can extract values properly
      "strippedArgs" <~ Lists.map Rewriting.deannotateTerm (var "reducedArgs") $
      "reducedResult" <<~ Flows.bind
        (Graph.primitiveImplementation (var "prim") @@ var "strippedArgs")
        (var "reduce" @@ var "eager") $
      var "applyIfNullary" @@ var "eager" @@ var "reducedResult" @@ var "remainingArgs") $
    cases _Term (var "stripped")
      (Just $ Flows.pure $ var "applyToArguments" @@ var "original" @@ var "args") [
      _Term_application>>: "app" ~> var "applyIfNullary" @@ var "eager" @@
        (Core.applicationFunction $ var "app") @@
        (Lists.cons (Core.applicationArgument $ var "app") (var "args")),
      _Term_function>>: match _Function Nothing [
          _Function_elimination>>: "elm" ~>
            Logic.ifElse (Lists.null $ var "args")
              (Flows.pure $ var "original")
              (var "forElimination" @@ var "elm" @@ var "args"),
          _Function_lambda>>: "l" ~>
            Logic.ifElse (Lists.null $ var "args")
              (Flows.pure $ var "original")
              (var "forLambda" @@ var "l" @@ var "args"),
          _Function_primitive>>: "name" ~>
            "prim" <<~ Lexical.requirePrimitive @@ var "name" $
            "arity" <~ Arity.primitiveArity @@ var "prim" $
            Logic.ifElse (Equality.gt (var "arity") (Lists.length $ var "args"))
              (Flows.pure $ var "applyToArguments" @@ var "original" @@ var "args")
              (var "forPrimitive" @@ var "prim" @@ var "arity" @@ var "args")],
      _Term_variable>>: "v" ~>
        -- Look up the variable in the graph; if found, reduce its definition
        "mBinding" <<~ Lexical.dereferenceElement @@ var "v" $
        Maybes.maybe
          -- Not found: lambda-bound variable, return with args applied
          (Flows.pure $ var "applyToArguments" @@ var "original" @@ var "args")
          -- Found: reduce the element's term with the accumulated args
          ("binding" ~> var "applyIfNullary" @@ var "eager" @@ (Core.bindingTerm $ var "binding") @@ var "args")
          (var "mBinding"),
      _Term_let>>: "lt" ~>
        -- For recursive let bindings, wrap self-references with `let f = <value> in f`
        -- This allows recursive functions to be properly evaluated
        "bindings" <~ Core.letBindings (var "lt") $
        "body" <~ Core.letBody (var "lt") $
        -- Create a let expression that wraps a binding: let b = <value> in b
        "letExpr" <~ ("b" ~>
          Core.termLet $ Core.let_
            (list [var "b"])
            (Core.termVariable (Core.bindingName $ var "b"))) $
        -- Expand a binding by replacing self-references with the let wrapper
        "expandBinding" <~ ("b" ~>
          Core.binding
            (Core.bindingName $ var "b")
            (Rewriting.replaceFreeTermVariable
              @@ (Core.bindingName $ var "b")
              @@ (var "letExpr" @@ var "b")
              @@ (Core.bindingTerm $ var "b"))
            (Core.bindingType $ var "b")) $
        "expandedBindings" <~ Lists.map (var "expandBinding") (var "bindings") $
        -- Substitute each binding into the term (foldl takes acc -> elem -> acc)
        "substituteBinding" <~ ("term" ~> "b" ~>
          Rewriting.replaceFreeTermVariable
            @@ (Core.bindingName $ var "b")
            @@ (Core.bindingTerm $ var "b")
            @@ var "term") $
        "substituteAll" <~ ("bs" ~> "term" ~>
          Lists.foldl (var "substituteBinding") (var "term") (var "bs")) $
        "expandedBody" <~ var "substituteAll" @@ var "expandedBindings" @@ var "body" $
        "reducedBody" <<~ var "reduce" @@ var "eager" @@ var "expandedBody" $
        var "applyIfNullary" @@ var "eager" @@ var "reducedBody" @@ var "args"]) $
  "mapping" <~ ("recurse" ~> "mid" ~>
    "inner" <<~ Logic.ifElse (var "doRecurse" @@ var "eager" @@ var "mid")
      (var "recurse" @@ var "mid")
      (Flows.pure $ var "mid") $
    var "applyIfNullary" @@ var "eager" @@ var "inner" @@ (list ([] :: [TTerm Term]))) $
  Rewriting.rewriteTermM @@ var "mapping" @@ var "term"

termIsClosed :: TBinding (Term -> Bool)
termIsClosed = define "termIsClosed" $
  doc "Whether a term is closed, i.e. represents a complete program" $
  "term" ~> Sets.null $ Rewriting.freeVariablesInTerm @@ var "term"

termIsValue :: TBinding (Graph -> Term -> Bool)
termIsValue = define "termIsValue" $
  doc "Whether a term has been fully reduced to a value" $
  "g" ~> "term" ~>
  "forList" <~ ("els" ~> Lists.foldl ("b" ~> "t" ~> Logic.and (var "b") (termIsValue @@ var "g" @@ var "t")) true (var "els")) $
  "checkField" <~ ("f" ~> termIsValue @@ var "g" @@ Core.fieldTerm (var "f")) $
  "checkFields" <~ ("fields" ~> Lists.foldl ("b" ~> "f" ~> Logic.and (var "b") (var "checkField" @@ var "f")) true (var "fields")) $
  "functionIsValue" <~ ("f" ~> cases _Function (var "f") Nothing [
    _Function_elimination>>: "e" ~>
      cases _Elimination (var "e") Nothing [
        _Elimination_wrap>>: constant true,
        _Elimination_record>>: constant true,
        _Elimination_union>>: "cs" ~>
          Logic.and (var "checkFields" @@ Core.caseStatementCases (var "cs"))
            (Maybes.maybe true (termIsValue @@ var "g") (Core.caseStatementDefault $ var "cs"))],
    _Function_lambda>>: "l" ~> termIsValue @@ var "g" @@ Core.lambdaBody (var "l"),
    _Function_primitive>>: constant true]) $
  cases _Term (Rewriting.deannotateTerm @@ var "term")
    (Just false) [
    _Term_application>>: constant false,
    _Term_either>>: "e" ~>
      Eithers.either_
        ("l" ~> termIsValue @@ var "g" @@ var "l")
        ("r" ~> termIsValue @@ var "g" @@ var "r")
        (var "e"),
    _Term_literal>>: constant true,
    _Term_function>>: "f" ~> var "functionIsValue" @@ var "f",
    _Term_list>>: "els" ~> var "forList" @@ var "els",
    _Term_map>>: "m" ~>
      Lists.foldl ("b" ~> "kv" ~>
        Logic.and (var "b") $ Logic.and
          (termIsValue @@ var "g" @@ Pairs.first (var "kv"))
          (termIsValue @@ var "g" @@ Pairs.second (var "kv")))
        true $ Maps.toList (var "m"),
    _Term_maybe>>: "m" ~>
      Maybes.maybe true (termIsValue @@ var "g") (var "m"),
    _Term_record>>: "r" ~> var "checkFields" @@ Core.recordFields (var "r"),
    _Term_set>>: "s" ~> var "forList" @@ Sets.toList (var "s"),
    _Term_union>>: "i" ~> var "checkField" @@ Core.injectionField (var "i"),
    _Term_unit>>: constant true,
    _Term_variable>>: constant false]
