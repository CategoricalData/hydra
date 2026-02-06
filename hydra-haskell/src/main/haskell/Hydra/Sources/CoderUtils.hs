-- | Common utilities for language coders, providing shared patterns for term decomposition,
-- environment management, and other cross-language concerns.
--
-- This module is expressed in the Hydra DSL and generates code for all target implementations.

module Hydra.Sources.CoderUtils where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  commentsFromElement,
  commentsFromFieldType,
  gatherApplications,
  gatherArgs,
  gatherArgsWithTypeApps,
  isComplexBinding,
  isComplexTerm,
  isComplexVariable,
  isSimpleAssignment,
  normalizeComment)
import Hydra.Sources.Libraries
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

import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking as Checking
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas


ns :: Namespace
ns = Namespace "hydra.coderUtils"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Arity.ns, Checking.ns, Lexical.ns, Monads.ns, Rewriting.ns, Schemas.ns]
    kernelTypesNamespaces $
    Just "Common utilities for language coders, providing shared patterns for term decomposition and analysis."
  where
   elements = [
     -- Simple pure functions first
     toBinding normalizeComment,
     toBinding gatherApplications,
     toBinding gatherArgs,
     toBinding gatherArgsWithTypeApps,
     -- Predicates
     toBinding isSimpleAssignment,
     toBinding isComplexTerm,
     toBinding isComplexVariable,
     toBinding isComplexBinding,
     -- Flow-based utilities
     toBinding commentsFromElement,
     toBinding commentsFromFieldType,
     -- Function analysis helpers
     toBinding tryTypeOf,
     toBinding bindingMetadata,
     toBinding analyzeFunctionTermWith,
     toBinding analyzeFunctionTermNoInferWith,
     toBinding analyzeFunctionTerm,
     toBinding analyzeFunctionTermInline,
     toBinding analyzeFunctionTermNoInfer,
     -- State management helpers for coders
     toBinding updateCoderMetadata,
     toBinding withUpdatedCoderGraph,
     toBinding withGraphBindings,
     toBinding inCoderGraphContext]


--------------------------------------------------------------------------------
-- Simple pure functions
--------------------------------------------------------------------------------

-- | Normalize a comment string for consistent output across coders.
-- Strips leading/trailing whitespace and ensures the comment ends with a period.
-- Returns empty string for whitespace-only input.
normalizeComment :: TBinding (String -> String)
normalizeComment = define "normalizeComment" $
  doc "Normalize a comment string for consistent output across coders" $
  "s" ~>
  "stripped" <~ Formatting.stripLeadingAndTrailingWhitespace @@ var "s" $
  Logic.ifElse
    (Strings.null (var "stripped"))
    (string "")
    -- Get the last character by using charAt with (length - 1)
    -- Code point 46 is '.'
    ("lastIdx" <~ Math.sub (Strings.length (var "stripped")) (int32 1) $
     "lastChar" <~ Strings.charAt (var "lastIdx") (var "stripped") $
     Logic.ifElse
       (Equality.equal (var "lastChar") (int32 46))
       (var "stripped")
       (Strings.cat2 (var "stripped") (string ".")))

-- | Recursively gather applications from a term, returning the list of arguments
-- and the base term. Applications are traversed left-to-right, with arguments
-- collected in the order they appear (leftmost first).
--
-- For example, given a term representing @f a b c@, this returns @([a, b, c], f)@.
gatherApplications :: TBinding (Term -> ([Term], Term))
gatherApplications = define "gatherApplications" $
  doc "Gather applications from a term, returning (args, baseTerm)" $
  "term" ~>
  -- Use a local recursive helper with an accumulator
  "go" <~ ("args" ~> "t" ~>
    cases _Term (Rewriting.deannotateTerm @@ var "t")
      (Just $ pair (var "args") (var "t")) [
      _Term_application>>: "app" ~>
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        var "go" @@ (Lists.cons (var "rhs") (var "args")) @@ var "lhs"]) $
  var "go" @@ (list ([] :: [TTerm Term])) @@ var "term"

-- | Recursively gather applications, type lambdas, and type applications from a term.
-- Returns a pair of (base term, argument list) where the base term has all applications,
-- type lambdas, and type applications removed.
gatherArgs :: TBinding (Term -> [Term] -> (Term, [Term]))
gatherArgs = define "gatherArgs" $
  doc "Gather term arguments, stripping type-level constructs" $
  "term" ~> "args" ~>
  cases _Term (Rewriting.deannotateTerm @@ var "term")
    (Just $ pair (var "term") (var "args")) [
    _Term_application>>: "app" ~>
      "lhs" <~ Core.applicationFunction (var "app") $
      "rhs" <~ Core.applicationArgument (var "app") $
      gatherArgs @@ var "lhs" @@ (Lists.cons (var "rhs") (var "args")),
    _Term_typeLambda>>: "tl" ~>
      "body" <~ Core.typeLambdaBody (var "tl") $
      gatherArgs @@ var "body" @@ var "args",
    _Term_typeApplication>>: "ta" ~>
      "body" <~ Core.typeApplicationTermBody (var "ta") $
      gatherArgs @@ var "body" @@ var "args"]

-- | Like gatherArgs but also collects type arguments from TermTypeApplication nodes.
-- Returns (fun, args, typeArgs) where typeArgs are in application order.
gatherArgsWithTypeApps :: TBinding (Term -> [Term] -> [Type] -> (Term, [Term], [Type]))
gatherArgsWithTypeApps = define "gatherArgsWithTypeApps" $
  doc "Gather term and type arguments from a term" $
  "term" ~> "args" ~> "tyArgs" ~>
  cases _Term (Rewriting.deannotateTerm @@ var "term")
    (Just $ triple (var "term") (var "args") (var "tyArgs")) [
    _Term_application>>: "app" ~>
      "lhs" <~ Core.applicationFunction (var "app") $
      "rhs" <~ Core.applicationArgument (var "app") $
      gatherArgsWithTypeApps @@ var "lhs" @@ (Lists.cons (var "rhs") (var "args")) @@ var "tyArgs",
    _Term_typeLambda>>: "tl" ~>
      "body" <~ Core.typeLambdaBody (var "tl") $
      gatherArgsWithTypeApps @@ var "body" @@ var "args" @@ var "tyArgs",
    _Term_typeApplication>>: "ta" ~>
      "body" <~ Core.typeApplicationTermBody (var "ta") $
      "typ" <~ Core.typeApplicationTermType (var "ta") $
      gatherArgsWithTypeApps @@ var "body" @@ var "args" @@ (Lists.cons (var "typ") (var "tyArgs"))]


--------------------------------------------------------------------------------
-- Predicate functions
--------------------------------------------------------------------------------

-- | Determines whether a term can be encoded as a simple assignment (without type annotation).
-- A term is considered a simple assignment if:
-- - It's not a lambda, let, or type lambda
-- - It's not a type application (which introduces polymorphism requiring type signatures)
-- - When peeled of applications, it's not a case statement
isSimpleAssignment :: TBinding (Term -> Bool)
isSimpleAssignment = define "isSimpleAssignment" $
  doc "Check if a term can be encoded as a simple assignment" $
  "term" ~>
  cases _Term (var "term")
    (Just $
      -- Check if the base term (after gathering args) is a union elimination
      "baseTerm" <~ Pairs.first (gatherArgs @@ var "term" @@ list ([] :: [TTerm Term])) $
      cases _Term (var "baseTerm")
        (Just $ boolean True) [
        _Term_function>>: "f" ~>
          cases _Function (var "f")
            (Just $ boolean True) [
            _Function_elimination>>: "elim" ~>
              cases _Elimination (var "elim")
                (Just $ boolean True) [
                _Elimination_union>>: constant (boolean False)]]]) [
    _Term_annotated>>: "at" ~>
      isSimpleAssignment @@ (Core.annotatedTermBody $ var "at"),
    _Term_function>>: "f" ~>
      cases _Function (var "f")
        (Just $ boolean True) [
        _Function_lambda>>: constant (boolean False)],
    _Term_let>>: constant (boolean False),
    _Term_typeLambda>>: constant (boolean False),
    _Term_typeApplication>>: "ta" ~>
      isSimpleAssignment @@ (Core.typeApplicationTermBody $ var "ta")]

-- | Determine whether a given term needs to be treated as a (possibly nullary) function,
-- rather than a simple value. The term might be an actual function, or it may have type parameters
-- or internal let bindings, or it may reference complex variables.
isComplexTerm :: TBinding (TypeContext -> Term -> Bool)
isComplexTerm = define "isComplexTerm" $
  doc "Check if a term needs to be treated as a function rather than a simple value" $
  "tc" ~> "t" ~>
  cases _Term (var "t")
    (Just $
      -- Default: check if any subterm is complex
      Lists.foldl
        ("b" ~> "sub" ~> Logic.or (var "b") (isComplexTerm @@ var "tc" @@ var "sub"))
        (boolean False)
        (Rewriting.subterms @@ var "t")) [
    _Term_let>>: constant (boolean True),
    _Term_typeApplication>>: constant (boolean True),
    _Term_typeLambda>>: constant (boolean True),
    _Term_variable>>: "name" ~> isComplexVariable @@ var "tc" @@ var "name"]

-- | Look up a variable to see if it is bound to a complex term
isComplexVariable :: TBinding (TypeContext -> Name -> Bool)
isComplexVariable = define "isComplexVariable" $
  doc "Check if a variable is bound to a complex term" $
  "tc" ~> "name" ~>
  -- Check if there's metadata for this variable (indicates complexity)
  "metaLookup" <~ Maps.lookup (var "name") (Typing.typeContextMetadata $ var "tc") $
  Logic.ifElse
    (Maybes.isJust (var "metaLookup"))
    (boolean True)
    -- Lambda-bound variables are complex because they might be thunked
    (Logic.ifElse
      (Sets.member (var "name") (Typing.typeContextLambdaVariables $ var "tc"))
      (boolean True)
      -- If not in type context, assume mutual recursion (complex)
      ("typeLookup" <~ Maps.lookup (var "name") (Typing.typeContextTypes $ var "tc") $
       Logic.not (Maybes.isJust (var "typeLookup"))))

-- | Check if a binding is complex and needs to be treated as a function
isComplexBinding :: TBinding (TypeContext -> Binding -> Bool)
isComplexBinding = define "isComplexBinding" $
  doc "Check if a binding needs to be treated as a function" $
  "tc" ~> "b" ~>
  "term" <~ Core.bindingTerm (var "b") $
  "mts" <~ Core.bindingType (var "b") $
  -- Bindings without type schemes are complex (e.g., lifted case expressions)
  Maybes.cases (var "mts")
    (isComplexTerm @@ var "tc" @@ var "term") $
    "ts" ~>
      -- Check if polymorphic
      "isPolymorphic" <~ Logic.not (Lists.null (Core.typeSchemeVariables $ var "ts")) $
      -- Check if non-nullary
      "isNonNullary" <~ Equality.gt (Arity.typeArity @@ (Core.typeSchemeType $ var "ts")) (int32 0) $
      -- Check if complex term
      "isComplex" <~ isComplexTerm @@ var "tc" @@ var "term" $
      Logic.or (Logic.or (var "isPolymorphic") (var "isNonNullary")) (var "isComplex")


--------------------------------------------------------------------------------
-- Flow-based utilities
--------------------------------------------------------------------------------

-- | Extract comments/description from a Binding (element definition).
-- This is a common pattern for coders that need to preserve documentation.
commentsFromElement :: TBinding (Binding -> Flow Graph (Maybe String))
commentsFromElement = define "commentsFromElement" $
  doc "Extract comments/description from a Binding" $
  "b" ~> Annotations.getTermDescription @@ (Core.bindingTerm $ var "b")

-- | Extract comments/description from a FieldType.
-- This is a common pattern for coders that need to preserve field documentation.
commentsFromFieldType :: TBinding (FieldType -> Flow Graph (Maybe String))
commentsFromFieldType = define "commentsFromFieldType" $
  doc "Extract comments/description from a FieldType" $
  "ft" ~> Annotations.getTypeDescription @@ (Core.fieldTypeType $ var "ft")


--------------------------------------------------------------------------------
-- Function analysis utilities
--------------------------------------------------------------------------------

-- | Infer the type of a term using the given TypeContext.
-- This is a helper that adds tracing for debugging purposes.
tryTypeOf :: TBinding (String -> TypeContext -> Term -> Flow s Type)
tryTypeOf = define "tryTypeOf" $
  doc "Infer the type of a term with tracing" $
  "msg" ~> "tc" ~> "term" ~>
  Monads.withTrace @@ var "msg" @@ (Checking.typeOf @@ var "tc" @@ list ([] :: [TTerm Type]) @@ var "term")

-- | Produces a simple 'true' value if the binding is complex (needs to be treated as a function)
bindingMetadata :: TBinding (TypeContext -> Binding -> Maybe Term)
bindingMetadata = define "bindingMetadata" $
  doc "Produces metadata for a binding if it is complex" $
  "tc" ~> "b" ~>
  Logic.ifElse
    (isComplexBinding @@ var "tc" @@ var "b")
    (just MetaTerms.true)
    nothing

-- | Internal helper: analyze a function term with a configurable binding metadata function.
-- This is the core implementation used by all analyzeFunctionTerm variants.
--
-- The function peels off lambdas, type lambdas, lets, and type applications,
-- collecting their components into a FunctionStructure. It uses a recursive
-- gather/finish pattern with an argMode flag to track whether we're still
-- collecting lambda parameters (vs. having seen a let which stops parameter collection).
analyzeFunctionTermWith :: TBinding (
  (TypeContext -> Binding -> Maybe Term) ->
  (env -> TypeContext) ->
  (TypeContext -> env -> env) ->
  env ->
  Term ->
  Flow s (FunctionStructure env))
analyzeFunctionTermWith = define "analyzeFunctionTermWith" $
  doc "Analyze a function term with configurable binding metadata" $
  "forBinding" ~> "getTC" ~> "setTC" ~> "env" ~> "term" ~>
  -- Use mutually recursive helpers: gather and finish
  lets [
    -- Finish helper: reapply type applications and infer return type
    "finish">: (
      "fEnv" ~> "tparams" ~> "args" ~> "bindings" ~> "doms" ~> "tapps" ~> "body" ~>
      -- Reapply type applications to the body
      "bodyWithTapps" <~ Lists.foldl
        ("trm" ~> "typ" ~> Core.termTypeApplication (Core.typeApplicationTerm (var "trm") (var "typ")))
        (var "body")
        (var "tapps") $
      -- Infer the return type
      "typ" <<~ tryTypeOf @@ string "analyzeFunctionTermWith" @@ (var "getTC" @@ var "fEnv") @@ var "bodyWithTapps" $
      Flows.pure $ record _FunctionStructure [
        _FunctionStructure_typeParams>>: Lists.reverse (var "tparams"),
        _FunctionStructure_params>>: Lists.reverse (var "args"),
        _FunctionStructure_bindings>>: var "bindings",
        _FunctionStructure_body>>: var "bodyWithTapps",
        _FunctionStructure_domains>>: Lists.reverse (var "doms"),
        _FunctionStructure_codomain>>: just (var "typ"),
        _FunctionStructure_environment>>: var "fEnv"]),
    -- Gather helper: recursively collect components
    "gather">: (
      "argMode" ~> "gEnv" ~> "tparams" ~> "args" ~> "bindings" ~> "doms" ~> "tapps" ~> "t" ~>
      cases _Term (Rewriting.deannotateTerm @@ var "t")
        (Just $ var "finish" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t") [
        -- Lambda: collect parameter and domain if in argMode
        _Term_function>>: "f" ~>
          cases _Function (var "f")
            (Just $ var "finish" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t") [
            _Function_lambda>>: "lam" ~>
              Logic.ifElse (var "argMode")
                -- Collect parameter and domain, extend environment
                ("v" <~ Core.lambdaParameter (var "lam") $
                 "dom" <~ Maybes.maybe (Core.typeVariable (Core.name (string "_"))) identity (Core.lambdaDomain (var "lam")) $
                 "body" <~ Core.lambdaBody (var "lam") $
                 "newEnv" <~ (var "setTC" @@ (Schemas.extendTypeContextForLambda @@ (var "getTC" @@ var "gEnv") @@ var "lam") @@ var "gEnv") $
                 var "gather" @@ var "argMode" @@ var "newEnv"
                   @@ var "tparams"
                   @@ (Lists.cons (var "v") (var "args"))
                   @@ var "bindings"
                   @@ (Lists.cons (var "dom") (var "doms"))
                   @@ var "tapps"
                   @@ var "body")
                -- Stop collecting, finish
                (var "finish" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t")],
        -- Let: accumulate bindings, extend environment, stop argMode
        _Term_let>>: "lt" ~>
          "newBindings" <~ Core.letBindings (var "lt") $
          "body" <~ Core.letBody (var "lt") $
          "newEnv" <~ (var "setTC" @@ (Schemas.extendTypeContextForLet @@ var "forBinding" @@ (var "getTC" @@ var "gEnv") @@ var "lt") @@ var "gEnv") $
          var "gather" @@ boolean False @@ var "newEnv"
            @@ var "tparams"
            @@ var "args"
            @@ (Lists.concat2 (var "bindings") (var "newBindings"))
            @@ var "doms"
            @@ var "tapps"
            @@ var "body",
        -- Type application: accumulate type arguments
        _Term_typeApplication>>: "ta" ~>
          "taBody" <~ Core.typeApplicationTermBody (var "ta") $
          "typ" <~ Core.typeApplicationTermType (var "ta") $
          var "gather" @@ var "argMode" @@ var "gEnv"
            @@ var "tparams"
            @@ var "args"
            @@ var "bindings"
            @@ var "doms"
            @@ (Lists.cons (var "typ") (var "tapps"))
            @@ var "taBody",
        -- Type lambda: collect type parameter, extend environment
        _Term_typeLambda>>: "tl" ~>
          "tvar" <~ Core.typeLambdaParameter (var "tl") $
          "tlBody" <~ Core.typeLambdaBody (var "tl") $
          "newEnv" <~ (var "setTC" @@ (Schemas.extendTypeContextForTypeLambda @@ (var "getTC" @@ var "gEnv") @@ var "tl") @@ var "gEnv") $
          var "gather" @@ var "argMode" @@ var "newEnv"
            @@ (Lists.cons (var "tvar") (var "tparams"))
            @@ var "args"
            @@ var "bindings"
            @@ var "doms"
            @@ var "tapps"
            @@ var "tlBody"])] $
  var "gather" @@ boolean True @@ var "env"
    @@ list ([] :: [TTerm Name])
    @@ list ([] :: [TTerm Name])
    @@ list ([] :: [TTerm Binding])
    @@ list ([] :: [TTerm Type])
    @@ list ([] :: [TTerm Type])
    @@ var "term"

-- | Internal helper: analyze a function term without type inference, with configurable binding metadata.
analyzeFunctionTermNoInferWith :: TBinding (
  (TypeContext -> Binding -> Maybe Term) ->
  (env -> TypeContext) ->
  (TypeContext -> env -> env) ->
  env ->
  Term ->
  Flow s (FunctionStructure env))
analyzeFunctionTermNoInferWith = define "analyzeFunctionTermNoInferWith" $
  doc "Analyze a function term without type inference, with configurable binding metadata" $
  "forBinding" ~> "getTC" ~> "setTC" ~> "env" ~> "term" ~>
  -- Use mutually recursive helpers: gather and finish
  lets [
    -- Finish helper: reapply type applications, skip type inference
    "finish">: (
      "fEnv" ~> "tparams" ~> "args" ~> "bindings" ~> "doms" ~> "tapps" ~> "body" ~>
      "bodyWithTapps" <~ Lists.foldl
        ("trm" ~> "typ" ~> Core.termTypeApplication (Core.typeApplicationTerm (var "trm") (var "typ")))
        (var "body")
        (var "tapps") $
      Flows.pure $ record _FunctionStructure [
        _FunctionStructure_typeParams>>: Lists.reverse (var "tparams"),
        _FunctionStructure_params>>: Lists.reverse (var "args"),
        _FunctionStructure_bindings>>: var "bindings",
        _FunctionStructure_body>>: var "bodyWithTapps",
        _FunctionStructure_domains>>: Lists.reverse (var "doms"),
        _FunctionStructure_codomain>>: nothing,
        _FunctionStructure_environment>>: var "fEnv"]),
    -- Gather helper: recursively collect components
    "gather">: (
      "argMode" ~> "gEnv" ~> "tparams" ~> "args" ~> "bindings" ~> "doms" ~> "tapps" ~> "t" ~>
      cases _Term (Rewriting.deannotateTerm @@ var "t")
        (Just $ var "finish" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t") [
        _Term_function>>: "f" ~>
          cases _Function (var "f")
            (Just $ var "finish" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t") [
            _Function_lambda>>: "lam" ~>
              Logic.ifElse (var "argMode")
                ("v" <~ Core.lambdaParameter (var "lam") $
                 "dom" <~ Maybes.maybe (Core.typeVariable (Core.name (string "_"))) identity (Core.lambdaDomain (var "lam")) $
                 "body" <~ Core.lambdaBody (var "lam") $
                 "newEnv" <~ (var "setTC" @@ (Schemas.extendTypeContextForLambda @@ (var "getTC" @@ var "gEnv") @@ var "lam") @@ var "gEnv") $
                 var "gather" @@ var "argMode" @@ var "newEnv"
                   @@ var "tparams"
                   @@ (Lists.cons (var "v") (var "args"))
                   @@ var "bindings"
                   @@ (Lists.cons (var "dom") (var "doms"))
                   @@ var "tapps"
                   @@ var "body")
                (var "finish" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t")],
        _Term_let>>: "lt" ~>
          "newBindings" <~ Core.letBindings (var "lt") $
          "body" <~ Core.letBody (var "lt") $
          "newEnv" <~ (var "setTC" @@ (Schemas.extendTypeContextForLet @@ var "forBinding" @@ (var "getTC" @@ var "gEnv") @@ var "lt") @@ var "gEnv") $
          var "gather" @@ boolean False @@ var "newEnv"
            @@ var "tparams"
            @@ var "args"
            @@ (Lists.concat2 (var "bindings") (var "newBindings"))
            @@ var "doms"
            @@ var "tapps"
            @@ var "body",
        _Term_typeApplication>>: "ta" ~>
          "taBody" <~ Core.typeApplicationTermBody (var "ta") $
          "typ" <~ Core.typeApplicationTermType (var "ta") $
          var "gather" @@ var "argMode" @@ var "gEnv"
            @@ var "tparams"
            @@ var "args"
            @@ var "bindings"
            @@ var "doms"
            @@ (Lists.cons (var "typ") (var "tapps"))
            @@ var "taBody",
        _Term_typeLambda>>: "tl" ~>
          "tvar" <~ Core.typeLambdaParameter (var "tl") $
          "tlBody" <~ Core.typeLambdaBody (var "tl") $
          "newEnv" <~ (var "setTC" @@ (Schemas.extendTypeContextForTypeLambda @@ (var "getTC" @@ var "gEnv") @@ var "tl") @@ var "gEnv") $
          var "gather" @@ var "argMode" @@ var "newEnv"
            @@ (Lists.cons (var "tvar") (var "tparams"))
            @@ var "args"
            @@ var "bindings"
            @@ var "doms"
            @@ var "tapps"
            @@ var "tlBody"])] $
  var "gather" @@ boolean True @@ var "env"
    @@ list ([] :: [TTerm Name])
    @@ list ([] :: [TTerm Name])
    @@ list ([] :: [TTerm Binding])
    @@ list ([] :: [TTerm Type])
    @@ list ([] :: [TTerm Type])
    @@ var "term"

-- | Analyze a function term by recursively peeling off lambdas, type lambdas, lets, and type applications.
-- This is a common pattern across all language coders: we need to understand the structure of a function
-- to properly encode it in the target language.
analyzeFunctionTerm :: TBinding (
  (env -> TypeContext) ->
  (TypeContext -> env -> env) ->
  env ->
  Term ->
  Flow s (FunctionStructure env))
analyzeFunctionTerm = define "analyzeFunctionTerm" $
  doc "Analyze a function term, collecting lambdas, type lambdas, lets, and type applications" $
  "getTC" ~> "setTC" ~> "env" ~> "term" ~>
  analyzeFunctionTermWith @@ bindingMetadata @@ var "getTC" @@ var "setTC" @@ var "env" @@ var "term"

-- | Like analyzeFunctionTerm, but without recording binding metadata. This is used for inline
-- lambda expressions where let bindings are encoded as walrus operators (which evaluate
-- immediately and share values, so don't need thunking or function call syntax).
analyzeFunctionTermInline :: TBinding (
  (env -> TypeContext) ->
  (TypeContext -> env -> env) ->
  env ->
  Term ->
  Flow s (FunctionStructure env))
analyzeFunctionTermInline = define "analyzeFunctionTermInline" $
  doc "Analyze a function term without recording binding metadata" $
  "getTC" ~> "setTC" ~> "env" ~> "term" ~>
  analyzeFunctionTermWith @@ (constant (constant nothing)) @@ var "getTC" @@ var "setTC" @@ var "env" @@ var "term"

-- | Analyze a function term without inferring the return type.
-- This is a performance optimization for dynamically-typed target languages (like Python)
-- where the codomain type is not needed and type inference is expensive.
analyzeFunctionTermNoInfer :: TBinding (
  (env -> TypeContext) ->
  (TypeContext -> env -> env) ->
  env ->
  Term ->
  Flow s (FunctionStructure env))
analyzeFunctionTermNoInfer = define "analyzeFunctionTermNoInfer" $
  doc "Analyze a function term without type inference (performance optimization)" $
  "getTC" ~> "setTC" ~> "env" ~> "term" ~>
  analyzeFunctionTermNoInferWith @@ bindingMetadata @@ var "getTC" @@ var "setTC" @@ var "env" @@ var "term"


--------------------------------------------------------------------------------
-- State management helpers for coders
--------------------------------------------------------------------------------

-- | Update the metadata portion of a coder state.
-- This is useful for tracking language-specific metadata during code generation.
--
-- Parameters:
-- - getMeta: Extract metadata from state
-- - makeCoder: Construct state from Graph and metadata
-- - getGraph: Extract Graph from state
-- - f: Transformation to apply to metadata
updateCoderMetadata :: TBinding (
  (state -> metadata) ->
  (Graph -> metadata -> state) ->
  (state -> Graph) ->
  (metadata -> metadata) ->
  Flow state ())
updateCoderMetadata = define "updateCoderMetadata" $
  doc "Update the metadata portion of a coder state" $
  "getMeta" ~> "makeCoder" ~> "getGraph" ~> "f" ~>
  "st" <<~ Monads.getState $
  Monads.putState @@ (var "makeCoder" @@ (var "getGraph" @@ var "st") @@ (var "f" @@ (var "getMeta" @@ var "st")))

-- | Temporarily update the graph for a computation, then restore it.
-- The metadata remains mutable throughout the flow (any changes are preserved).
--
-- This pattern is useful when you need to:
-- - Try encoding with an extended graph but not commit the extensions
-- - Test something in a modified graph context
-- - Keep metadata changes but discard graph changes
withUpdatedCoderGraph :: TBinding (
  (state -> Graph) ->
  (state -> metadata) ->
  (Graph -> metadata -> state) ->
  (Graph -> Graph) ->
  Flow state a ->
  Flow state a)
withUpdatedCoderGraph = define "withUpdatedCoderGraph" $
  doc "Temporarily update the graph for a computation, then restore it" $
  "getGraph" ~> "getMeta" ~> "makeCoder" ~> "f" ~> "flow" ~>
  "st" <<~ Monads.getState $
  exec (Monads.putState @@ (var "makeCoder" @@ (var "f" @@ (var "getGraph" @@ var "st")) @@ (var "getMeta" @@ var "st"))) $
  "r" <<~ var "flow" $
  "st2" <<~ Monads.getState $
  exec (Monads.putState @@ (var "makeCoder" @@ (var "getGraph" @@ var "st") @@ (var "getMeta" @@ var "st2"))) $
  Flows.pure (var "r")

-- | Temporarily extend the graph with additional bindings for a computation.
-- The bindings are only visible within the provided flow action.
--
-- This is commonly used when encoding terms that introduce local bindings
-- (like let expressions), where we need those bindings available in the graph
-- for type checking and term resolution.
withGraphBindings :: TBinding (
  (state -> Graph) ->
  (Graph -> metadata -> state) ->
  (state -> metadata) ->
  [Binding] ->
  Flow state a ->
  Flow state a)
withGraphBindings = define "withGraphBindings" $
  doc "Temporarily extend the graph with additional bindings for a computation" $
  "getGraph" ~> "makeCoder" ~> "getMeta" ~> "bindings" ~> "flow" ~>
  withUpdatedCoderGraph @@ var "getGraph" @@ var "getMeta" @@ var "makeCoder"
    @@ (Lexical.extendGraphWithBindings @@ var "bindings")
    @@ var "flow"

-- | Run a Flow Graph computation within a Flow state computation.
-- This allows you to execute graph-level operations (like type inference,
-- term lookups, etc.) while maintaining a richer coder state with metadata.
--
-- The graph changes from the inner computation are preserved, but run in
-- the context of the coder's graph.
inCoderGraphContext :: TBinding (
  (state -> Graph) ->
  (state -> metadata) ->
  (Graph -> metadata -> state) ->
  Flow Graph a ->
  Flow state a)
inCoderGraphContext = define "inCoderGraphContext" $
  doc "Run a Flow Graph computation within a Flow state computation" $
  "getGraph" ~> "getMeta" ~> "makeCoder" ~> "graphFlow" ~>
  "st" <<~ Monads.getState $
  "result" <<~ Monads.withState @@ (var "getGraph" @@ var "st") @@ (
    "ret" <<~ var "graphFlow" $
    "g2" <<~ Monads.getState $
    Flows.pure (pair (var "ret") (var "g2"))) $
  exec (Monads.putState @@ (var "makeCoder" @@ (Pairs.second (var "result")) @@ (var "getMeta" @@ var "st"))) $
  Flows.pure (Pairs.first (var "result"))
