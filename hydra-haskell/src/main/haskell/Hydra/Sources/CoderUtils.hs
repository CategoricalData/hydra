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
  isTrivialTerm,
  normalizeComment)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Error         as Error
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
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
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas


ns :: Namespace
ns = Namespace "hydra.coderUtils"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Arity.ns, Checking.ns, Lexical.ns, Rewriting.ns, Schemas.ns]
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
     toBinding isTrivialTerm,
     -- Tail-call optimization detection
     toBinding isSelfTailRecursive,
     toBinding isTailRecursiveInTailPosition,
     -- Context/graph utilities
     toBinding commentsFromElement,
     toBinding commentsFromFieldType,
     toBinding typeOfTerm,
     -- Function analysis helpers
     toBinding bindingMetadata,
     toBinding analyzeFunctionTerm,
     toBinding analyzeFunctionTermWith,
     toBinding analyzeFunctionTermWith_finish,
     toBinding analyzeFunctionTermWith_gather]

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
isComplexTerm :: TBinding (Graph -> Term -> Bool)
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
isComplexVariable :: TBinding (Graph -> Name -> Bool)
isComplexVariable = define "isComplexVariable" $
  doc "Check if a variable is bound to a complex term" $
  "tc" ~> "name" ~>
  -- Check if there's metadata for this variable (indicates complexity)
  "metaLookup" <~ Maps.lookup (var "name") (Graph.graphMetadata $ var "tc") $
  Logic.ifElse
    (Maybes.isJust (var "metaLookup"))
    (boolean True)
    -- Lambda-bound variables are complex because they might be thunked
    (Logic.ifElse
      (Sets.member (var "name") (Graph.graphLambdaVariables $ var "tc"))
      (boolean True)
      -- Check if the variable is in the graph's bound types
      ("typeLookup" <~ Maps.lookup (var "name") (Graph.graphBoundTypes $ var "tc") $
       Maybes.maybe
         -- If not in graph at all, assume mutual recursion (complex)
         (boolean True)
         -- If in graph, check if the binding itself is non-nullary (a function).
         -- Non-nullary bindings are always complex (they take parameters).
         -- Nullary bindings are assumed non-complex from this check;
         -- their actual complexity will be determined by isComplexBinding
         -- at the reference site.
         ("ts" ~> Equality.gt (Arity.typeSchemeArity @@ var "ts") (int32 0))
         (var "typeLookup")))

-- | Check if a binding is complex and needs to be treated as a function
isComplexBinding :: TBinding (Graph -> Binding -> Bool)
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

-- | Check if a term is trivially cheap to evaluate, meaning it needs no thunking.
-- Trivial terms include: literals, plain variable references, field projections
-- on trivial terms (e.g. app.function), unit values, maybe wrappers around trivial terms,
-- and type wrappers around trivial terms.
-- Field projections cause minor regressions in inference/hoisting (~200ms) but yield
-- large gains in checking/annotations/strings (~1500ms), for a net 4.2% improvement.
-- This is a conservative predicate: anything not explicitly recognized is non-trivial.
isTrivialTerm :: TBinding (Term -> Bool)
isTrivialTerm = define "isTrivialTerm" $
  doc "Check if a term is trivially cheap (no thunking needed)" $
  "t" ~>
  cases _Term (Rewriting.deannotateTerm @@ var "t")
    (Just $ boolean False) [
    -- Literals are always trivial
    _Term_literal>>: constant (boolean True),
    -- Plain variables are trivial (the variable itself is cheap to reference)
    _Term_variable>>: constant (boolean True),
    -- Unit is trivial
    _Term_unit>>: constant (boolean True),
    -- Field projection on a trivial subterm is trivial (e.g. app.function)
    _Term_application>>: "app" ~>
      "fun" <~ Core.applicationFunction (var "app") $
      "arg" <~ Core.applicationArgument (var "app") $
      cases _Term (var "fun") (Just $ boolean False) [
        _Term_function>>: "f" ~>
          cases _Function (var "f") (Just $ boolean False) [
            _Function_elimination>>: "e" ~>
              cases _Elimination (var "e") (Just $ boolean False) [
                -- record projection: trivial if the subject is trivial
                _Elimination_record>>: constant (isTrivialTerm @@ var "arg"),
                -- newtype unwrap: trivial if the subject is trivial
                _Elimination_wrap>>: constant (isTrivialTerm @@ var "arg")]]],
    -- Maybe term (just x) where x is trivial; nothing is also trivial
    _Term_maybe>>: "opt" ~>
      Maybes.maybe (boolean True) ("inner" ~> isTrivialTerm @@ var "inner") (var "opt"),
    -- Record construction is trivial if all field terms are trivial
    _Term_record>>: "rec" ~>
      Lists.foldl ("acc" ~> "fld" ~> Logic.and (var "acc") (isTrivialTerm @@ (Core.fieldTerm $ var "fld")))
        (boolean True) (Core.recordFields $ var "rec"),
    -- Wrap (newtype construction) is trivial if the inner term is trivial
    _Term_wrap>>: "wt" ~> isTrivialTerm @@ (Core.wrappedTermBody $ var "wt"),
    -- Type applications/lambdas: check the inner term
    _Term_typeApplication>>: "ta" ~> isTrivialTerm @@ (Core.typeApplicationTermBody $ var "ta"),
    _Term_typeLambda>>: "tl" ~> isTrivialTerm @@ (Core.typeLambdaBody $ var "tl")]


--------------------------------------------------------------------------------
-- Tail-call optimization detection
--------------------------------------------------------------------------------

-- | Check if a term body is self-tail-recursive with respect to a function name.
--   Returns True if the function references itself AND all self-references are in tail position.
--   Note: isFreeVariableInTerm returns True when the variable is NOT present (confusing API).
isSelfTailRecursive :: TBinding (Name -> Term -> Bool)
isSelfTailRecursive = define "isSelfTailRecursive" $
  doc "Check if a term body is self-tail-recursive with respect to a function name" $
  "funcName" ~> "body" ~>
    -- isFreeVariableInTerm returns True when v is NOT free (not present).
    -- So Logic.not means: the name IS present as a free variable.
    "callsSelf" <~ Logic.not (Rewriting.isFreeVariableInTerm @@ var "funcName" @@ var "body") $
    Logic.ifElse (var "callsSelf")
      (isTailRecursiveInTailPosition @@ var "funcName" @@ var "body")
      false

-- | Check that all occurrences of funcName in a term are in tail position.
--   Called after confirming funcName IS present in the term.
--   Returns True if the term is safe for TCO transformation.
isTailRecursiveInTailPosition :: TBinding (Name -> Term -> Bool)
isTailRecursiveInTailPosition = define "isTailRecursiveInTailPosition" $
  doc "Check that all self-references are in tail position" $
  "funcName" ~> "term" ~>
    "stripped" <~ (Rewriting.deannotateAndDetypeTerm @@ var "term") $
    cases _Term (var "stripped") (Just $
      -- Default: funcName must NOT appear free in this term (not a recognized tail position)
      Rewriting.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
      -- Application: check if it's a self-tail-call or a case statement application
      _Term_application>>: "app" ~>
        "gathered" <~ (gatherApplications @@ var "stripped") $
        "gatherArgs" <~ (Pairs.first $ var "gathered") $
        "gatherFun" <~ (Pairs.second $ var "gathered") $
        "strippedFun" <~ (Rewriting.deannotateAndDetypeTerm @@ var "gatherFun") $
        cases _Term (var "strippedFun") (Just $
          -- Unknown function form: funcName must not appear anywhere
          Rewriting.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
          -- Variable: check if self-call
          _Term_variable>>: "vname" ~>
            Logic.ifElse (Equality.equal (var "vname") (var "funcName"))
              -- Self-call in tail position: args must not contain funcName
              -- and must not contain lambdas (closures over parameters break TCO
              -- because Python closures capture by reference, not by value)
              ("argsNoFunc" <~ (Lists.foldl
                ("ok" ~> "arg" ~>
                  Logic.and (var "ok")
                    (Rewriting.isFreeVariableInTerm @@ var "funcName" @@ var "arg"))
                true
                (var "gatherArgs")) $
               "argsNoLambda" <~ (Lists.foldl
                ("ok" ~> "arg" ~>
                  Logic.and (var "ok")
                    (Logic.not $ Rewriting.foldOverTerm @@ Coders.traversalOrderPre
                      @@ ("found" ~> "t" ~>
                        Logic.or (var "found")
                          (cases _Term (var "t") (Just false) [
                            _Term_function>>: "f2" ~>
                              cases _Function (var "f2") (Just false) [
                                _Function_lambda>>: "lam" ~>
                                  -- Any lambda in an argument disqualifies from TCO
                                  "ignore" <~ (Core.lambdaBody $ var "lam") $
                                  true]]))
                      @@ false
                      @@ var "arg"))
                true
                (var "gatherArgs")) $
               Logic.and (var "argsNoFunc") (var "argsNoLambda"))
              -- Not a self-call: funcName must not appear anywhere in the term
              (Rewriting.isFreeVariableInTerm @@ var "funcName" @@ var "term"),
          -- Function: check for case statement (union elimination)
          _Term_function>>: "f" ~>
            cases _Function (var "f") (Just $
              Rewriting.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
              _Function_elimination>>: "e" ~>
                cases _Elimination (var "e") (Just $
                  Rewriting.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
                  _Elimination_union>>: "cs" ~>
                    "cases_" <~ (Core.caseStatementCases $ var "cs") $
                    "dflt" <~ (Core.caseStatementDefault $ var "cs") $
                    -- All case branches must have funcName only in tail position
                    "branchesOk" <~ (Lists.foldl
                      ("ok" ~> "field" ~>
                        Logic.and (var "ok")
                          (isTailRecursiveInTailPosition @@ var "funcName" @@ Core.fieldTerm (var "field")))
                      true
                      (var "cases_")) $
                    -- Default branch (if present) must also be tail-recursive
                    "dfltOk" <~ (Maybes.maybe true
                      ("d" ~> isTailRecursiveInTailPosition @@ var "funcName" @@ var "d")
                      (var "dflt")) $
                    -- Arguments to the case statement must NOT contain funcName
                    "argsOk" <~ (Lists.foldl
                      ("ok" ~> "arg" ~>
                        Logic.and (var "ok")
                          (Rewriting.isFreeVariableInTerm @@ var "funcName" @@ var "arg"))
                      true
                      (var "gatherArgs")) $
                    Logic.and (Logic.and (var "branchesOk") (var "dfltOk")) (var "argsOk")]]],
      -- Lambda: tail position is the body
      _Term_function>>: "f" ~>
        cases _Function (var "f") (Just $
          Rewriting.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
          _Function_lambda>>: "lam" ~>
            isTailRecursiveInTailPosition @@ var "funcName" @@ (Core.lambdaBody $ var "lam")],
      -- Let: tail position is the body; bindings must not contain funcName
      _Term_let>>: "lt" ~>
        "bindingsOk" <~ (Lists.foldl
          ("ok" ~> "b" ~>
            Logic.and (var "ok")
              (Rewriting.isFreeVariableInTerm @@ var "funcName" @@ Core.bindingTerm (var "b")))
          true
          (Core.letBindings $ var "lt")) $
        Logic.and (var "bindingsOk")
          (isTailRecursiveInTailPosition @@ var "funcName" @@ (Core.letBody $ var "lt"))]


--------------------------------------------------------------------------------
-- Context/graph utilities
--------------------------------------------------------------------------------

-- | Extract comments/description from a Binding (element definition).
-- This is a common pattern for coders that need to preserve documentation.
commentsFromElement :: TBinding (Context -> Graph -> Binding -> Either (InContext OtherError) (Maybe String))
commentsFromElement = define "commentsFromElement" $
  doc "Extract comments/description from a Binding" $
  "cx" ~> "g" ~> "b" ~>
  Annotations.getTermDescription @@ var "cx" @@ var "g" @@ (Core.bindingTerm $ var "b")

-- | Extract comments/description from a FieldType.
-- This is a common pattern for coders that need to preserve field documentation.
commentsFromFieldType :: TBinding (Context -> Graph -> FieldType -> Either (InContext OtherError) (Maybe String))
commentsFromFieldType = define "commentsFromFieldType" $
  doc "Extract comments/description from a FieldType" $
  "cx" ~> "g" ~> "ft" ~>
  Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ (Core.fieldTypeType $ var "ft")

-- | Check/reconstruct the type of a term, discarding the updated Context.
-- Wraps Checking.typeOf and returns just the Type.
typeOfTerm :: TBinding (Context -> Graph -> Term -> Either (InContext OtherError) Type)
typeOfTerm = define "typeOfTerm" $
  doc "Check the type of a term" $
  "cx" ~> "g" ~> "term" ~>
  Eithers.map (primitive _pairs_first)
    (Checking.typeOf @@ var "cx" @@ var "g" @@ list ([] :: [TTerm Type]) @@ var "term")


--------------------------------------------------------------------------------
-- Function analysis utilities
--------------------------------------------------------------------------------

-- | Produces a simple 'true' value if the binding is complex (needs to be treated as a function)
bindingMetadata :: TBinding (Graph -> Binding -> Maybe Term)
bindingMetadata = define "bindingMetadata" $
  doc "Produces metadata for a binding if it is complex" $
  "tc" ~> "b" ~>
  Logic.ifElse
    (isComplexBinding @@ var "tc" @@ var "b")
    (just MetaTerms.true)
    nothing

-- | Analyze a function term by recursively peeling off lambdas, type lambdas, lets, and type applications.
-- This is a common pattern across all language coders: we need to understand the structure of a function
-- to properly encode it in the target language.
analyzeFunctionTerm :: TBinding (
  Context ->
  (env -> Graph) ->
  (Graph -> env -> env) ->
  env ->
  Term ->
  Either (InContext OtherError) (FunctionStructure env))
analyzeFunctionTerm = define "analyzeFunctionTerm" $
  doc "Analyze a function term, collecting lambdas, type lambdas, lets, and type applications" $
  "cx" ~> "getTC" ~> "setTC" ~> "env" ~> "term" ~>
  analyzeFunctionTermWith @@ var "cx" @@ bindingMetadata @@ var "getTC" @@ var "setTC" @@ var "env" @@ var "term"

analyzeFunctionTermWith :: TBinding (
  Context ->
  (Graph -> Binding -> Maybe Term) ->
  (env -> Graph) ->
  (Graph -> env -> env) ->
  env ->
  Term ->
  Either (InContext OtherError) (FunctionStructure env))
analyzeFunctionTermWith = define "analyzeFunctionTermWith" $
  doc "Analyze a function term with configurable binding metadata" $
  "cx" ~> "forBinding" ~> "getTC" ~> "setTC" ~> "env" ~> "term" ~>
  analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
    @@ boolean True @@ var "env"
    @@ list ([] :: [TTerm Name])
    @@ list ([] :: [TTerm Name])
    @@ list ([] :: [TTerm Binding])
    @@ list ([] :: [TTerm Type])
    @@ list ([] :: [TTerm Type])
    @@ var "term"

-- | Internal helper: analyze a function term with a configurable binding metadata function.
-- This is the core implementation used by all analyzeFunctionTerm variants.
--
-- The function peels off lambdas, type lambdas, lets, and type applications,
-- collecting their components into a FunctionStructure. It uses a recursive
-- gather/finish pattern with an argMode flag to track whether we're still
-- collecting lambda parameters (vs. having seen a let which stops parameter collection).
-- | Finish helper for analyzeFunctionTermWith: reapply type applications and infer return type
analyzeFunctionTermWith_finish :: TBinding (
  Context ->
  (env -> Graph) ->
  env -> [Name] -> [Name] -> [Binding] -> [Type] -> [Type] -> Term ->
  Either (InContext OtherError) (FunctionStructure env))
analyzeFunctionTermWith_finish = define "analyzeFunctionTermWith_finish" $
  "cx" ~> "getTC" ~> "fEnv" ~> "tparams" ~> "args" ~> "bindings" ~> "doms" ~> "tapps" ~> "body" ~>
  "bodyWithTapps" <~ Lists.foldl
    ("trm" ~> "typ" ~> Core.termTypeApplication (Core.typeApplicationTerm (var "trm") (var "typ")))
    (var "body")
    (var "tapps") $
  -- Use typeOfTerm but fall back to Nothing if type inference fails (e.g. for untyped hoisted bindings)
  "mcod" <~ Eithers.either_ (constant nothing) ("c" ~> just (var "c"))
    (typeOfTerm @@ var "cx" @@ (var "getTC" @@ var "fEnv") @@ var "bodyWithTapps") $
  right $ record _FunctionStructure [
    _FunctionStructure_typeParams>>: Lists.reverse (var "tparams"),
    _FunctionStructure_params>>: Lists.reverse (var "args"),
    _FunctionStructure_bindings>>: var "bindings",
    _FunctionStructure_body>>: var "bodyWithTapps",
    _FunctionStructure_domains>>: Lists.reverse (var "doms"),
    _FunctionStructure_codomain>>: var "mcod",
    _FunctionStructure_environment>>: var "fEnv"]

-- | Gather helper for analyzeFunctionTermWith: recursively collect function components
analyzeFunctionTermWith_gather :: TBinding (
  Context ->
  (Graph -> Binding -> Maybe Term) ->
  (env -> Graph) ->
  (Graph -> env -> env) ->
  Bool -> env -> [Name] -> [Name] -> [Binding] -> [Type] -> [Type] -> Term ->
  Either (InContext OtherError) (FunctionStructure env))
analyzeFunctionTermWith_gather = define "analyzeFunctionTermWith_gather" $
  "cx" ~> "forBinding" ~> "getTC" ~> "setTC" ~>
  "argMode" ~> "gEnv" ~> "tparams" ~> "args" ~> "bindings" ~> "doms" ~> "tapps" ~> "t" ~>
  cases _Term (Rewriting.deannotateTerm @@ var "t")
    (Just $ analyzeFunctionTermWith_finish @@ var "cx" @@ var "getTC" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t") [
    _Term_function>>: "f" ~>
      cases _Function (var "f")
        (Just $ analyzeFunctionTermWith_finish @@ var "cx" @@ var "getTC" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t") [
        _Function_lambda>>: "lam" ~>
          Logic.ifElse (var "argMode")
            ("v" <~ Core.lambdaParameter (var "lam") $
             "dom" <~ Maybes.maybe (Core.typeVariable (Core.name (string "_"))) identity (Core.lambdaDomain (var "lam")) $
             "body" <~ Core.lambdaBody (var "lam") $
             "newEnv" <~ (var "setTC" @@ (Schemas.extendGraphForLambda @@ (var "getTC" @@ var "gEnv") @@ var "lam") @@ var "gEnv") $
             analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
               @@ var "argMode" @@ var "newEnv"
               @@ var "tparams"
               @@ (Lists.cons (var "v") (var "args"))
               @@ var "bindings"
               @@ (Lists.cons (var "dom") (var "doms"))
               @@ var "tapps"
               @@ var "body")
            (analyzeFunctionTermWith_finish @@ var "cx" @@ var "getTC" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t")],
    _Term_let>>: "lt" ~>
      "newBindings" <~ Core.letBindings (var "lt") $
      "body" <~ Core.letBody (var "lt") $
      "newEnv" <~ (var "setTC" @@ (Schemas.extendGraphForLet @@ var "forBinding" @@ (var "getTC" @@ var "gEnv") @@ var "lt") @@ var "gEnv") $
      analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
        @@ boolean False @@ var "newEnv"
        @@ var "tparams"
        @@ var "args"
        @@ (Lists.concat2 (var "bindings") (var "newBindings"))
        @@ var "doms"
        @@ var "tapps"
        @@ var "body",
    _Term_typeApplication>>: "ta" ~>
      "taBody" <~ Core.typeApplicationTermBody (var "ta") $
      "typ" <~ Core.typeApplicationTermType (var "ta") $
      analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
        @@ var "argMode" @@ var "gEnv"
        @@ var "tparams"
        @@ var "args"
        @@ var "bindings"
        @@ var "doms"
        @@ (Lists.cons (var "typ") (var "tapps"))
        @@ var "taBody",
    _Term_typeLambda>>: "tl" ~>
      "tvar" <~ Core.typeLambdaParameter (var "tl") $
      "tlBody" <~ Core.typeLambdaBody (var "tl") $
      "newEnv" <~ (var "setTC" @@ (Schemas.extendGraphForTypeLambda @@ (var "getTC" @@ var "gEnv") @@ var "tl") @@ var "gEnv") $
      analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
        @@ var "argMode" @@ var "newEnv"
        @@ (Lists.cons (var "tvar") (var "tparams"))
        @@ var "args"
        @@ var "bindings"
        @@ var "doms"
        @@ var "tapps"
        @@ var "tlBody"]
