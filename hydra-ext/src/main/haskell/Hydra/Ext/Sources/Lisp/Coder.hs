-- | Lisp code generator in Hydra DSL.
-- This module provides DSL versions of Lisp code generation functions.
-- Type definitions are mapped to record/struct definitions and tagged unions;
-- term definitions are mapped to function definitions and variable bindings.
-- The coder produces a dialect-neutral Lisp AST; per-dialect serializers render
-- the concrete syntax for Clojure, Emacs Lisp, Common Lisp, or Scheme.

module Hydra.Ext.Sources.Lisp.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Coders                          as Coders
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                           as Error
import qualified Hydra.Dsl.Module                          as Module
import qualified Hydra.Dsl.Util                            as Util
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports for Lisp AST
import qualified Hydra.Ext.Lisp.Syntax as L
import qualified Hydra.Ext.Sources.Lisp.Syntax as LispSyntax
import qualified Hydra.Ext.Sources.Lisp.Language as LispLanguageSource
import qualified Hydra.Sources.CoderUtils as CoderUtils


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.lisp.coder"

module_ :: Module
module_ = Module ns elements
    [moduleNamespace LispLanguageSource.module_,
      Formatting.ns, Names.ns, Rewriting.ns, Schemas.ns, Sorting.ns, Lexical.ns, CoderUtils.ns]
    (LispSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Lisp code generator: converts Hydra type and term modules to Lisp AST"
  where
    elements = [
      toTermDefinition dialectCadr,
      toTermDefinition dialectCar,
      toTermDefinition dialectConstructorPrefix,
      toTermDefinition dialectEqual,
      toTermDefinition encodeApplication,
      toTermDefinition encodeElimination,
      toTermDefinition encodeFieldDef,
      toTermDefinition encodeFunction,
      toTermDefinition encodeLetAsLambdaApp,
      toTermDefinition encodeLetAsNative,
      toTermDefinition encodeLiteral,
      toTermDefinition encodeTerm,
      toTermDefinition encodeTermDefinition,
      toTermDefinition encodeType,
      toTermDefinition encodeTypeBody,
      toTermDefinition encodeTypeDefinition,
      toTermDefinition isCasesPrimitive,
      toTermDefinition isLazy2ArgPrimitive,
      toTermDefinition isLazy3ArgPrimitive,
      toTermDefinition isPrimitiveRef,
      toTermDefinition lispApp,
      toTermDefinition lispKeyword,
      toTermDefinition lispLambdaExpr,
      toTermDefinition lispListExpr,
      toTermDefinition lispLitExpr,
      toTermDefinition lispNamedLambdaExpr,
      toTermDefinition lispNilExpr,
      toTermDefinition lispSymbol,
      toTermDefinition lispTopForm,
      toTermDefinition lispTopFormWithComments,
      toTermDefinition lispVar,
      toTermDefinition moduleExports,
      toTermDefinition moduleImports,
      toTermDefinition moduleToLisp,
      toTermDefinition qualifiedSnakeName,
      toTermDefinition qualifiedTypeName,
      toTermDefinition CoderUtils.reorderDefs,
      toTermDefinition wrapInThunk]


-- | Dialect-aware name for "cadr" (second element of a list)
-- Clojure: "second", others: "cadr"
dialectCadr :: TBinding (L.Dialect -> String)
dialectCadr = def "dialectCadr" $
  lambda "d" $ cases L._Dialect (var "d") (Just $ string "cadr") [
    L._Dialect_clojure>>: constant $ string "second"]

-- | Dialect-aware name for "car" (first element of a list)
-- Clojure: "first", others: "car"
dialectCar :: TBinding (L.Dialect -> String)
dialectCar = def "dialectCar" $
  lambda "d" $ cases L._Dialect (var "d") (Just $ string "car") [
    L._Dialect_clojure>>: constant $ string "first"]

-- | Dialect-aware constructor prefix for record types
-- Clojure: "->", others: "make-"
dialectConstructorPrefix :: TBinding (L.Dialect -> String)
dialectConstructorPrefix = def "dialectConstructorPrefix" $
  lambda "d" $ cases L._Dialect (var "d") (Just $ string "make-") [
    L._Dialect_clojure>>: constant $ string "->"]

-- | Dialect-aware name for "equal?" (equality test)
-- Clojure: "=", Common Lisp/Emacs Lisp: "equal", Scheme: "equal?"
dialectEqual :: TBinding (L.Dialect -> String)
dialectEqual = def "dialectEqual" $
  lambda "d" $ cases L._Dialect (var "d") (Just $ string "equal?") [
    L._Dialect_clojure>>: constant $ string "=",
    L._Dialect_commonLisp>>: constant $ string "equal",
    L._Dialect_emacsLisp>>: constant $ string "equal"]

-- | Encode a function application, detecting ifElse and other lazy primitives.
-- Transforms (((hydra.lib.logic.ifElse C) T) E) into native (if C T E).
-- For other lazy primitives, wraps the appropriate argument in a thunk.
encodeApplication :: TBinding (L.Dialect -> Context -> Graph -> Term -> Term -> Either (InContext Error) L.Expression)
encodeApplication = def "encodeApplication" $
  "dialect" ~> "cx" ~> "g" ~> lambda "rawFun" $ lambda "rawArg" $
    "dFun" <~ (Rewriting.deannotateTerm @@ var "rawFun") $
    -- Helper: encode a normal (non-special) application
    "normal" <~
      ("fun" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "rawFun") $
      "arg" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "rawArg") $
        right (lispApp @@ var "fun" @@ list [var "arg"])) $
    -- Helper: encode a term
    "enc" <~ (lambda "t" $ encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "t") $
    cases _Term (var "dFun") (Just $ var "normal")
    [_Term_application>>: lambda "app2" $
       "midFun" <~ Core.applicationFunction (var "app2") $
       "midArg" <~ Core.applicationArgument (var "app2") $
       "dMidFun" <~ (Rewriting.deannotateTerm @@ var "midFun") $
       -- 2-deep: dFun = App(midFun, midArg), applied to rawArg
       -- Check if midFun is a 2-arg lazy primitive
       "isLazy2" <~ Logic.or (isPrimitiveRef @@ string "hydra.lib.eithers.fromLeft" @@ var "dMidFun")
                      (Logic.or (isPrimitiveRef @@ string "hydra.lib.eithers.fromRight" @@ var "dMidFun")
                                (isPrimitiveRef @@ string "hydra.lib.maybes.fromMaybe" @@ var "dMidFun")) $
       Logic.ifElse (var "isLazy2")
         -- 2-arg lazy primitive: ((prim defVal) arg2) — wrap defVal in thunk
         ("ePrim" <<~ (var "enc" @@ var "midFun") $
         "eDef" <<~ (var "enc" @@ var "midArg") $
         "eArg" <<~ (var "enc" @@ var "rawArg") $
           right (lispApp @@ (lispApp @@ var "ePrim" @@ list [wrapInThunk @@ var "eDef"])
                          @@ list [var "eArg"]))
         -- Not a 2-arg lazy primitive — check for 3-deep patterns
         (cases _Term (var "dMidFun") (Just $ var "normal")
         [_Term_application>>: lambda "app3" $
            "innerFun" <~ Core.applicationFunction (var "app3") $
            "innerArg" <~ Core.applicationArgument (var "app3") $
            "dInnerFun" <~ (Rewriting.deannotateTerm @@ var "innerFun") $
            -- 3-deep: ifElse, maybe, or cases
            Logic.ifElse (isPrimitiveRef @@ string "hydra.lib.logic.ifElse" @@ var "dInnerFun")
              -- ifElse: (((ifElse C) T) E) -> native (if C T E)
              ("eC" <<~ (var "enc" @@ var "innerArg") $
              "eT" <<~ (var "enc" @@ var "midArg") $
              "eE" <<~ (var "enc" @@ var "rawArg") $
                right (inject L._Expression L._Expression_if $
                  record L._IfExpression [
                    L._IfExpression_condition>>: var "eC",
                    L._IfExpression_then>>: var "eT",
                    L._IfExpression_else>>: just (var "eE")]))
              (Logic.ifElse (isPrimitiveRef @@ string "hydra.lib.maybes.maybe" @@ var "dInnerFun")
                -- maybe: (((maybe defVal) f) m) — wrap defVal in thunk
                ("eP" <<~ (var "enc" @@ var "innerFun") $
                "eDef" <<~ (var "enc" @@ var "innerArg") $
                "eF" <<~ (var "enc" @@ var "midArg") $
                "eM" <<~ (var "enc" @@ var "rawArg") $
                  right (lispApp @@ (lispApp @@ (lispApp @@ var "eP" @@ list [wrapInThunk @@ var "eDef"])
                                             @@ list [var "eF"])
                                 @@ list [var "eM"]))
                (Logic.ifElse (isPrimitiveRef @@ string "hydra.lib.maybes.cases" @@ var "dInnerFun")
                  -- cases: (((cases m) nothingVal) justFn) — wrap nothingVal in thunk
                  ("eP" <<~ (var "enc" @@ var "innerFun") $
                  "eM" <<~ (var "enc" @@ var "innerArg") $
                  "eN" <<~ (var "enc" @@ var "midArg") $
                  "eJ" <<~ (var "enc" @@ var "rawArg") $
                    right (lispApp @@ (lispApp @@ (lispApp @@ var "eP" @@ list [var "eM"])
                                               @@ list [wrapInThunk @@ var "eN"])
                                   @@ list [var "eJ"]))
                  -- Not a special primitive — encode normally
                  (var "normal")))])]

-- | Encode a Hydra elimination as a Lisp expression.
-- Takes an optional argument for applied eliminations.
encodeElimination :: TBinding (L.Dialect -> Context -> Graph -> Elimination -> Maybe Term -> Either (InContext Error) L.Expression)
encodeElimination = def "encodeElimination" $
  "dialect" ~> "cx" ~> "g" ~> lambda "elim" $ lambda "marg" $
    cases _Elimination (var "elim") Nothing [
      -- Record projection: (:field record) or (record-type-field record)
      _Elimination_record>>: lambda "proj" $
        "fname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.projectionField (var "proj"))) $
        "tname" <~ (qualifiedSnakeName @@ Core.projectionTypeName (var "proj")) $
        Maybes.cases (var "marg")
          -- Unapplied: (lambda (v) (record-type-field v))
          (right (lispLambdaExpr @@ list [string "v"] @@
            (inject L._Expression L._Expression_fieldAccess $
              record L._FieldAccess [
                L._FieldAccess_recordType>>: wrap L._Symbol (var "tname"),
                L._FieldAccess_field>>: wrap L._Symbol (var "fname"),
                L._FieldAccess_target>>: lispVar @@ string "v"])))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "arg") $
              right (inject L._Expression L._Expression_fieldAccess $
                record L._FieldAccess [
                  L._FieldAccess_recordType>>: wrap L._Symbol (var "tname"),
                  L._FieldAccess_field>>: wrap L._Symbol (var "fname"),
                  L._FieldAccess_target>>: var "sarg"])),

      -- Union elimination: cond dispatch on tagged values
      _Elimination_union>>: lambda "cs" $
        "tname" <~ (Names.localNameOf @@ Core.caseStatementTypeName (var "cs")) $
        "caseFields" <~ Core.caseStatementCases (var "cs") $
        "defCase" <~ Core.caseStatementDefault (var "cs") $
        -- Build cond clauses from each case field
        "clauses" <<~ (Eithers.mapList
          (lambda "cf" $
            "cfname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.fieldName (var "cf"))) $
            "cfterm" <~ Core.fieldTerm (var "cf") $
            -- Each case applies the handler to the value: ((handler) v)
            -- Condition: (equal? (car __m) :variantName) or (= (first __m) :variantName)
            "condExpr" <~ (lispApp @@ (lispVar @@ (dialectEqual @@ var "dialect")) @@ list [
              lispApp @@ (lispVar @@ (dialectCar @@ var "dialect")) @@ list [lispVar @@ string "match_target"],
              lispKeyword @@ var "cfname"]) $
            -- Body: apply handler to (cadr __m)
            "bodyExpr" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ (Core.termApplication (Core.application (var "cfterm") (Core.termVariable (wrap _Name (string "match_value")))))) $
              right (record L._CondClause [
                L._CondClause_condition>>: var "condExpr",
                L._CondClause_body>>: var "bodyExpr"]))
          (var "caseFields")) $
        -- Default clause
        -- Default is a direct result value, NOT a handler function.
        -- The reducer returns the default as-is without applying it to the payload.
        "defExpr" <<~ (Maybes.cases (var "defCase")
          (right nothing)
          (lambda "dt" $
            "defBody" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "dt") $
              right (just (var "defBody"))))  $
        -- Build the cond expression wrapped in a lambda taking "v"
        "condExpr" <~ (inject L._Expression L._Expression_cond $
          record L._CondExpression [
            L._CondExpression_clauses>>: var "clauses",
            L._CondExpression_default>>: var "defExpr"]) $
        -- Wrap in ((lambda (__mv) (cond ...)) (cadr __m)) or (second __m) for Clojure
        "innerExpr" <~ (lispApp @@
          (lispLambdaExpr @@ list [string "match_value"] @@ var "condExpr") @@
          list [lispApp @@ (lispVar @@ (dialectCadr @@ var "dialect")) @@ list [lispVar @@ string "match_target"]]) $
        Maybes.cases (var "marg")
          -- Unapplied: (lambda (__m) ((lambda (__mv) (cond ...)) (second __m)))
          (right (lispLambdaExpr @@ list [string "match_target"] @@ var "innerExpr"))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "arg") $
              -- Applied: ((lambda (__m) ((lambda (__mv) (cond ...)) (second __m))) sarg)
              right (lispApp @@ (lispLambdaExpr @@ list [string "match_target"] @@ var "innerExpr") @@ list [var "sarg"])),

      -- Wrap elimination: transparent unwrap
      _Elimination_wrap>>: lambda "name" $
        Maybes.cases (var "marg")
          -- Unapplied: identity function
          (right (lispLambdaExpr @@ list [string "v"] @@ (lispVar @@ string "v")))
          (lambda "arg" $
            encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "arg")]

-- | Encode a Hydra field type as a Lisp field definition
encodeFieldDef :: TBinding (FieldType -> L.FieldDefinition)
encodeFieldDef = def "encodeFieldDef" $
  lambda "ft" $
    "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
      record L._FieldDefinition [
        L._FieldDefinition_name>>: wrap L._Symbol (Formatting.convertCaseCamelToLowerSnake @@ var "fname"),
        L._FieldDefinition_defaultValue>>: nothing]

-- | Encode a Hydra function as a Lisp expression
encodeFunction :: TBinding (L.Dialect -> Context -> Graph -> Function -> Either (InContext Error) L.Expression)
encodeFunction = def "encodeFunction" $
  "dialect" ~> "cx" ~> "g" ~> lambda "fun" $
    cases _Function (var "fun") Nothing [
      _Function_lambda>>: lambda "lam" $
        "param" <~ (Formatting.convertCaseCamelOrUnderscoreToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ LispLanguageSource.lispReservedWords @@ Core.unName (Core.lambdaParameter (var "lam")))) $
        "body" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Core.lambdaBody (var "lam")) $
          right (lispLambdaExpr @@ list [var "param"] @@ var "body"),
      _Function_primitive>>: lambda "name" $
        right (lispVar @@ (Formatting.convertCaseCamelOrUnderscoreToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ LispLanguageSource.lispReservedWords @@ Core.unName (var "name")))),
      _Function_elimination>>: lambda "elim" $
        encodeElimination @@ var "dialect" @@ var "cx" @@ var "g" @@ var "elim" @@ nothing]

-- | Encode let bindings as nested ((lambda (x) body) init) applications.
-- Used for self-referential non-lambda bindings (Y-combinator fixpoint pattern)
-- so that the loader's fix-letrec can transform them into proper letrec with thunking.
encodeLetAsLambdaApp :: TBinding (L.Dialect -> Context -> Graph -> [Binding] -> Term -> Either (InContext Error) L.Expression)
encodeLetAsLambdaApp = def "encodeLetAsLambdaApp" $
  "dialect" ~> "cx" ~> "g" ~> lambda "bindings" $ lambda "body" $
    "bodyExpr" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "body") $
    Eithers.foldl
      (lambda "acc" $ lambda "b" $
        "bname" <~ (Formatting.convertCaseCamelOrUnderscoreToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ LispLanguageSource.lispReservedWords @@ Core.unName (Core.bindingName (var "b")))) $
        "bval" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Core.bindingTerm (var "b")) $
          right (lispApp @@ (lispLambdaExpr @@ list [var "bname"] @@ var "acc") @@ list [var "bval"]))
      (var "bodyExpr")
      (Lists.reverse (var "bindings"))

-- | Encode let bindings as native let/let*/letrec expressions.
-- Self-referential bindings -> letrec (with eta-expansion for non-lambda self-refs)
-- Single non-self-ref binding -> let
-- Multiple non-self-ref bindings -> let* (sequential)
encodeLetAsNative :: TBinding (L.Dialect -> Context -> Graph -> [Binding] -> Term -> Either (InContext Error) L.Expression)
encodeLetAsNative = def "encodeLetAsNative" $
  "dialect" ~> "cx" ~> "g" ~> lambda "bindings" $ lambda "body" $
    "isClojureTop" <~ (cases L._Dialect (var "dialect") (Just $ boolean False)
      [L._Dialect_clojure>>: constant $ boolean True]) $
    "bodyExpr" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "body") $
    -- For Clojure: topologically sort bindings so dependencies come before dependents.
    -- Clojure's let is sequential, so referenced bindings must be defined first.
    -- Build adjacency list: each binding depends on other bindings it references.
    -- Topologically sort bindings for ALL Lisp dialects (all are eager)
    "sortedBindings" <~ (Logic.ifElse (boolean True)
      (lets [
        "allNames">: Sets.fromList (Lists.map (lambda "b" $ Core.bindingName (var "b")) (var "bindings")),
        "adjList">: Lists.map (lambda "b" $
          pair (Core.bindingName (var "b"))
            (Sets.toList (Sets.intersection (var "allNames")
              (Rewriting.freeVariablesInTerm @@ Core.bindingTerm (var "b")))))
          (var "bindings"),
        "sortResult">: Sorting.topologicalSort @@ var "adjList",
        "nameToBinding">: Maps.fromList (Lists.map (lambda "b" $ pair (Core.bindingName (var "b")) (var "b")) (var "bindings"))] $
        -- If sort succeeds, reorder. If cyclic, keep original order.
        Eithers.either_ (constant (var "bindings"))
          (lambda "sorted" $
            Lists.map (lambda "name" $
              Maybes.fromMaybe (Lists.head (var "bindings"))
                (Maps.lookup (var "name") (var "nameToBinding")))
              (var "sorted"))
          (var "sortResult"))
      (var "bindings")) $
    -- Encode each binding, eta-expanding self-referential non-lambda bindings
    -- so that letrec doesn't evaluate the self-reference during initialization.
    -- E.g., `recurse = f(fsub(recurse))` becomes `recurse = (lambda (_arg) ((f (fsub recurse)) _arg))`
    "encodedBindings" <<~ (Eithers.mapList
      (lambda "b" $
        "bname" <~ (Formatting.convertCaseCamelOrUnderscoreToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ LispLanguageSource.lispReservedWords @@ Core.unName (Core.bindingName (var "b")))) $
        "isSelfRef" <~ (Sets.member (Core.bindingName (var "b"))
          (Rewriting.freeVariablesInTerm @@ Core.bindingTerm (var "b"))) $
        "isLambda" <~ (cases _Term (Rewriting.deannotateTerm @@ Core.bindingTerm (var "b"))
          (Just $ boolean False)
          [_Term_function>>: lambda "f" $
            cases _Function (var "f") (Just $ boolean False)
              [_Function_lambda>>: constant (boolean True)]]) $
        "bval" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Core.bindingTerm (var "b")) $
        -- Handle self-referential bindings:
        -- For Clojure: use named fn for self-reference (both lambda and eta-expanded)
        -- For others: use letrec (eta-expand non-lambda self-refs for letrec compat)
        "isClojure" <~ (cases L._Dialect (var "dialect") (Just $ boolean False)
          [L._Dialect_clojure>>: constant $ boolean True]) $
        "wrappedVal" <~ (Logic.ifElse (var "isClojure")
          -- Clojure path: use named fn for all self-referential bindings
          (Logic.ifElse (var "isSelfRef")
            (Logic.ifElse (var "isLambda")
              -- Lambda: add name to the lambda for (fn name [...] ...)
              (cases L._Expression (var "bval") (Just $ var "bval") [
                L._Expression_lambda>>: lambda "lam" $
                  inject L._Expression L._Expression_lambda $
                    record L._Lambda [
                      L._Lambda_name>>: just (wrap L._Symbol (var "bname")),
                      L._Lambda_params>>: project L._Lambda L._Lambda_params @@ var "lam",
                      L._Lambda_restParam>>: project L._Lambda L._Lambda_restParam @@ var "lam",
                      L._Lambda_body>>: project L._Lambda L._Lambda_body @@ var "lam"]])
              -- Non-lambda self-ref: eta-expand with named fn
              (lispNamedLambdaExpr @@ var "bname" @@ list [string "_arg"] @@
                (lispApp @@ var "bval" @@ list [lispVar @@ string "_arg"])))
            (var "bval"))
          -- Non-Clojure path: eta-expand non-lambda self-refs for letrec
          (Logic.ifElse (Logic.and (var "isSelfRef") (Logic.not (var "isLambda")))
            (lispLambdaExpr @@ list [string "_arg"] @@
              (lispApp @@ var "bval" @@ list [lispVar @@ string "_arg"]))
            (var "bval"))) $
          right (pair (var "bname") (var "wrappedVal")))
      (var "sortedBindings")) $
    -- Determine let kind: check if any binding references itself or another binding in the group
    "allBindingNames" <~ Sets.fromList (Lists.map (lambda "b" $ Core.bindingName (var "b")) (var "bindings")) $
    "hasCrossRefs" <~ (Lists.foldl
      (lambda "acc" $ lambda "b" $
        Logic.or (var "acc")
          (Logic.not (Sets.null (Sets.intersection (var "allBindingNames")
            (Rewriting.freeVariablesInTerm @@ Core.bindingTerm (var "b"))))))
      (boolean False)
      (var "bindings")) $
    -- For Clojure: use recursive kind if there are any cross-binding references
    -- (Clojure's let is sequential and can't handle forward references)
    -- For others: only use recursive kind if there's self-reference
    "hasSelfRef" <~ (Lists.foldl
      (lambda "acc" $ lambda "b" $
        Logic.or (var "acc")
          (Sets.member (Core.bindingName (var "b"))
            (Rewriting.freeVariablesInTerm @@ Core.bindingTerm (var "b"))))
      (boolean False)
      (var "bindings")) $
    "isRecursive" <~ (var "hasSelfRef") $
    "letKind" <~ (Logic.ifElse (var "isRecursive")
      (inject L._LetKind L._LetKind_recursive unit)
      (Logic.ifElse (Lists.null (Lists.tail (var "bindings")))
        (inject L._LetKind L._LetKind_parallel unit)
        (inject L._LetKind L._LetKind_sequential unit))) $
    "lispBindings" <~ (Lists.map
      (lambda "eb" $
        inject L._LetBinding L._LetBinding_simple $
          record L._SimpleBinding [
            L._SimpleBinding_name>>: wrap L._Symbol (Pairs.first (var "eb")),
            L._SimpleBinding_value>>: Pairs.second (var "eb")])
      (var "encodedBindings")) $
      right (inject L._Expression L._Expression_let $
        record L._LetExpression [
          L._LetExpression_kind>>: var "letKind",
          L._LetExpression_bindings>>: var "lispBindings",
          L._LetExpression_body>>: list [var "bodyExpr"]])

-- | Encode a Hydra literal as a Lisp expression
encodeLiteral :: TBinding (Literal -> L.Expression)
encodeLiteral = def "encodeLiteral" $
  lambda "lit" $ cases _Literal (var "lit") Nothing [
    _Literal_boolean>>: lambda "b" $
      inject L._Expression L._Expression_literal $
        inject L._Literal L._Literal_boolean (var "b"),
    _Literal_string>>: lambda "s" $
      inject L._Expression L._Expression_literal $
        inject L._Literal L._Literal_string (var "s"),
    _Literal_float>>: lambda "fv" $
      cases _FloatValue (var "fv") Nothing [
        _FloatValue_float32>>: lambda "f" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_float $
              record L._FloatLiteral [
                L._FloatLiteral_value>>: Literals.float32ToBigfloat (var "f"),
                L._FloatLiteral_precision>>: nothing],
        _FloatValue_float64>>: lambda "f" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_float $
              record L._FloatLiteral [
                L._FloatLiteral_value>>: Literals.float64ToBigfloat (var "f"),
                L._FloatLiteral_precision>>: nothing],
        _FloatValue_bigfloat>>: lambda "f" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_float $
              record L._FloatLiteral [
                L._FloatLiteral_value>>: var "f",
                L._FloatLiteral_precision>>: nothing]],
    _Literal_integer>>: lambda "iv" $
      cases _IntegerValue (var "iv") Nothing [
        _IntegerValue_int8>>: lambda "i" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_integer $
              record L._IntegerLiteral [
                L._IntegerLiteral_value>>: Literals.int8ToBigint (var "i"),
                L._IntegerLiteral_bigint>>: boolean False],
        _IntegerValue_int16>>: lambda "i" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_integer $
              record L._IntegerLiteral [
                L._IntegerLiteral_value>>: Literals.int16ToBigint (var "i"),
                L._IntegerLiteral_bigint>>: boolean False],
        _IntegerValue_int32>>: lambda "i" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_integer $
              record L._IntegerLiteral [
                L._IntegerLiteral_value>>: Literals.int32ToBigint (var "i"),
                L._IntegerLiteral_bigint>>: boolean False],
        _IntegerValue_int64>>: lambda "i" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_integer $
              record L._IntegerLiteral [
                L._IntegerLiteral_value>>: Literals.int64ToBigint (var "i"),
                L._IntegerLiteral_bigint>>: boolean False],
        _IntegerValue_uint8>>: lambda "i" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_integer $
              record L._IntegerLiteral [
                L._IntegerLiteral_value>>: Literals.uint8ToBigint (var "i"),
                L._IntegerLiteral_bigint>>: boolean False],
        _IntegerValue_uint16>>: lambda "i" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_integer $
              record L._IntegerLiteral [
                L._IntegerLiteral_value>>: Literals.uint16ToBigint (var "i"),
                L._IntegerLiteral_bigint>>: boolean False],
        _IntegerValue_uint32>>: lambda "i" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_integer $
              record L._IntegerLiteral [
                L._IntegerLiteral_value>>: Literals.uint32ToBigint (var "i"),
                L._IntegerLiteral_bigint>>: boolean False],
        _IntegerValue_uint64>>: lambda "i" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_integer $
              record L._IntegerLiteral [
                L._IntegerLiteral_value>>: Literals.uint64ToBigint (var "i"),
                L._IntegerLiteral_bigint>>: boolean False],
        _IntegerValue_bigint>>: lambda "i" $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_integer $
              record L._IntegerLiteral [
                L._IntegerLiteral_value>>: var "i",
                L._IntegerLiteral_bigint>>: boolean True]],
    _Literal_binary>>: lambda "b" $
      -- Encode binary as a vector of byte values
      "byteValues" <~ Literals.binaryToBytes (var "b") $
      inject L._Expression L._Expression_vector $
        record L._VectorLiteral [
          L._VectorLiteral_elements>>:
            Lists.map (lambda "bv" $
              inject L._Expression L._Expression_literal $
                inject L._Literal L._Literal_integer $
                  record L._IntegerLiteral [
                    L._IntegerLiteral_value>>: Literals.int32ToBigint (var "bv"),
                    L._IntegerLiteral_bigint>>: boolean False])
              (var "byteValues")]]

-- | Encode a Hydra term as a Lisp expression
encodeTerm :: TBinding (L.Dialect -> Context -> Graph -> Term -> Either (InContext Error) L.Expression)
encodeTerm = def "encodeTerm" $
  "dialect" ~> "cx" ~> "g" ~> lambda "term" $
    cases _Term (var "term") Nothing
    [_Term_annotated>>: lambda "at" $
       encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Core.annotatedTermBody (var "at"),

     _Term_application>>: lambda "app" $
       -- Check if this is a fully-applied ifElse: (((ifElse C) T) E) -> (if C T E)
       "rawFun" <~ Core.applicationFunction (var "app") $
       "rawArg" <~ Core.applicationArgument (var "app") $
       encodeApplication @@ var "dialect" @@ var "cx" @@ var "g" @@ var "rawFun" @@ var "rawArg",

     _Term_either>>: lambda "e" $
       Eithers.either_
         (lambda "l" $
           "sl" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "l") $
             -- Left v -> (list :left v)
             right (lispApp @@ (lispVar @@ string "list") @@ list [
               lispKeyword @@ string "left",
               var "sl"]))
         (lambda "r" $
           "sr" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "r") $
             -- Right v -> (list :right v)
             right (lispApp @@ (lispVar @@ string "list") @@ list [
               lispKeyword @@ string "right",
               var "sr"]))
         (var "e"),

     _Term_function>>: lambda "fun" $
       encodeFunction @@ var "dialect" @@ var "cx" @@ var "g" @@ var "fun",

     _Term_let>>: lambda "lt" $
       "bindings" <~ Core.letBindings (var "lt") $
       "body" <~ Core.letBody (var "lt") $
       encodeLetAsNative @@ var "dialect" @@ var "cx" @@ var "g" @@ var "bindings" @@ var "body",

     _Term_list>>: lambda "els" $
       "sels" <<~ (Eithers.mapList (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g") (var "els")) $
         right (lispListExpr @@ var "sels"),

     _Term_literal>>: lambda "lit" $
       right (encodeLiteral @@ var "lit"),

     _Term_map>>: lambda "m" $
       "pairs" <<~ (Eithers.mapList
         (lambda "entry" $
           "k" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Pairs.first (var "entry")) $
           "v" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Pairs.second (var "entry")) $
             right (record L._MapEntry [
               L._MapEntry_key>>: var "k",
               L._MapEntry_value>>: var "v"]))
         (Maps.toList (var "m"))) $
         right (inject L._Expression L._Expression_map $
           record L._MapLiteral [
             L._MapLiteral_entries>>: var "pairs"]),

     _Term_maybe>>: lambda "mt" $
       Maybes.cases (var "mt")
         -- Nothing -> (list :nothing)
         (right (lispApp @@ (lispVar @@ string "list") @@ list [
           lispKeyword @@ string "nothing"]))
         -- Just val -> (list :just encodedVal)
         (lambda "val" $
           "sval" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "val") $
             right (lispApp @@ (lispVar @@ string "list") @@ list [
               lispKeyword @@ string "just",
               var "sval"])),

     _Term_pair>>: lambda "p" $
       "f" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Pairs.first (var "p")) $
       "s" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Pairs.second (var "p")) $
         right (lispListExpr @@ list [var "f", var "s"]),

     _Term_record>>: lambda "rec" $
       "rname" <~ Core.recordTypeName (var "rec") $
       "fields" <~ Core.recordFields (var "rec") $
       "sfields" <<~ (Eithers.mapList
         (lambda "f" $
           encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Core.fieldTerm (var "f"))
         (var "fields")) $
       -- Dialect-aware constructor: (make-TypeName ...) or (->TypeName ...)
       "constructorName" <~ Strings.cat2 (dialectConstructorPrefix @@ var "dialect") (qualifiedSnakeName @@ var "rname") $
         right (lispApp @@ (lispVar @@ var "constructorName") @@ var "sfields"),

     _Term_set>>: lambda "s" $
       "sels" <<~ (Eithers.mapList (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g") (Sets.toList (var "s"))) $
         right (inject L._Expression L._Expression_set $
           record L._SetLiteral [
             L._SetLiteral_elements>>: var "sels"]),

     _Term_union>>: lambda "inj" $
       "tname" <~ (Names.localNameOf @@ Core.injectionTypeName (var "inj")) $
       "field" <~ Core.injectionField (var "inj") $
       "fname" <~ Core.unName (Core.fieldName (var "field")) $
       "fterm" <~ Core.fieldTerm (var "field") $
       "dterm" <~ (Rewriting.deannotateTerm @@ var "fterm") $
       "isUnit" <~ (cases _Term (var "dterm") (Just $ boolean False) [
         _Term_unit>>: constant $ boolean True,
         _Term_record>>: lambda "rt" $ Lists.null (Core.recordFields (var "rt"))]) $
       Logic.ifElse (var "isUnit")
         -- Unit variant: (list :variantName '())
         (right (lispApp @@ (lispVar @@ string "list") @@ list [
           lispKeyword @@ (Formatting.convertCaseCamelToLowerSnake @@ var "fname"),
           asTerm lispNilExpr]))
         -- Non-unit variant: (list :variantName value)
         ("sval" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "fterm") $
           right (lispApp @@ (lispVar @@ string "list") @@ list [
             lispKeyword @@ (Formatting.convertCaseCamelToLowerSnake @@ var "fname"),
             var "sval"])),

     _Term_unit>>: constant $
       right (asTerm lispNilExpr),

     _Term_variable>>: lambda "name" $
       right (lispVar @@ (Formatting.convertCaseCamelOrUnderscoreToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ LispLanguageSource.lispReservedWords @@ Core.unName (var "name")))),

     _Term_typeApplication>>: lambda "ta" $
       encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Core.typeApplicationTermBody (var "ta"),

     _Term_typeLambda>>: lambda "tl" $
       encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Core.typeLambdaBody (var "tl"),

     _Term_wrap>>: lambda "wt" $
       encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ Core.wrappedTermBody (var "wt")]

-- | Encode a Hydra term definition as a Lisp top-level form
encodeTermDefinition :: TBinding (L.Dialect -> Context -> Graph -> TermDefinition -> Either (InContext Error) L.TopLevelFormWithComments)
encodeTermDefinition = def "encodeTermDefinition" $
  "dialect" ~> "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Module.termDefinitionName (var "tdef") $
    "term" <~ Module.termDefinitionTerm (var "tdef") $
    "lname" <~ (qualifiedSnakeName @@ var "name") $
    "dterm" <~ (Rewriting.deannotateTerm @@ var "term") $
    -- Check if the term is a lambda (function) or a value
    cases _Term (var "dterm") (Just $
      -- Non-function: encode as a variable definition
      "sterm" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "term") $
        right (lispTopForm @@ (inject L._TopLevelForm L._TopLevelForm_variable $
          record L._VariableDefinition [
            L._VariableDefinition_name>>: wrap L._Symbol (var "lname"),
            L._VariableDefinition_value>>: var "sterm",
            L._VariableDefinition_doc>>: nothing])))
    [_Term_function>>: lambda "fun" $
       cases _Function (var "fun") (Just $
         "sterm" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "term") $
           right (lispTopForm @@ (inject L._TopLevelForm L._TopLevelForm_variable $
             record L._VariableDefinition [
               L._VariableDefinition_name>>: wrap L._Symbol (var "lname"),
               L._VariableDefinition_value>>: var "sterm",
               L._VariableDefinition_doc>>: nothing])))
       [_Function_lambda>>: lambda "lam" $
          -- Encode as (def name (fn [param] body)) to avoid Clojure compile-time
          -- symbol resolution issues with Y-combinator patterns in recursive bindings
          "sterm" <<~ (encodeTerm @@ var "dialect" @@ var "cx" @@ var "g" @@ var "term") $
            right (lispTopForm @@ (inject L._TopLevelForm L._TopLevelForm_variable $
              record L._VariableDefinition [
                L._VariableDefinition_name>>: wrap L._Symbol (var "lname"),
                L._VariableDefinition_value>>: var "sterm",
                L._VariableDefinition_doc>>: nothing]))]]

-- | Encode a Hydra type as a Lisp type specifier (used for type annotations)
encodeType :: TBinding (Context -> Graph -> Type -> Either (InContext Error) L.TypeSpecifier)
encodeType = def "encodeType" $
  "cx" ~> "g" ~> lambda "t" $
    "typ" <~ (Rewriting.deannotateType @@ var "t") $
    cases _Type (var "typ") (Just $
      -- Default: named type referencing the Hydra type name
      right (inject L._TypeSpecifier L._TypeSpecifier_named $
        wrap L._Symbol (string "Any")))
    [_Type_annotated>>: lambda "at" $
       encodeType @@ var "cx" @@ var "g" @@ Core.annotatedTypeBody (var "at"),
     _Type_application>>: lambda "at" $
       encodeType @@ var "cx" @@ var "g" @@ Core.applicationTypeFunction (var "at"),
     _Type_unit>>: constant $
       right (inject L._TypeSpecifier L._TypeSpecifier_unit unit),
     _Type_literal>>: lambda "lt" $
       right (cases _LiteralType (var "lt") Nothing [
         _LiteralType_binary>>: constant $
           inject L._TypeSpecifier L._TypeSpecifier_named $ wrap L._Symbol (string "ByteArray"),
         _LiteralType_boolean>>: constant $
           inject L._TypeSpecifier L._TypeSpecifier_named $ wrap L._Symbol (string "Boolean"),
         _LiteralType_float>>: constant $
           inject L._TypeSpecifier L._TypeSpecifier_named $ wrap L._Symbol (string "Float"),
         _LiteralType_integer>>: constant $
           inject L._TypeSpecifier L._TypeSpecifier_named $ wrap L._Symbol (string "Integer"),
         _LiteralType_string>>: constant $
           inject L._TypeSpecifier L._TypeSpecifier_named $ wrap L._Symbol (string "String")]),
     _Type_list>>: lambda "inner" $
       Eithers.map (lambda "enc" $
         inject L._TypeSpecifier L._TypeSpecifier_list (var "enc"))
         (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
     _Type_set>>: lambda "inner" $
       Eithers.map (lambda "enc" $
         inject L._TypeSpecifier L._TypeSpecifier_set (var "enc"))
         (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
     _Type_map>>: lambda "mt" $
       right (inject L._TypeSpecifier L._TypeSpecifier_named $
         wrap L._Symbol (string "Map")),
     _Type_maybe>>: lambda "inner" $
       Eithers.map (lambda "enc" $
         inject L._TypeSpecifier L._TypeSpecifier_maybe (var "enc"))
         (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
     _Type_either>>: lambda "et" $
       right (inject L._TypeSpecifier L._TypeSpecifier_named $
         wrap L._Symbol (string "Either")),
     _Type_pair>>: lambda "pt" $
       right (inject L._TypeSpecifier L._TypeSpecifier_named $
         wrap L._Symbol (string "Pair")),
     _Type_function>>: lambda "ft" $
       right (inject L._TypeSpecifier L._TypeSpecifier_named $
         wrap L._Symbol (string "Function")),
     _Type_record>>: lambda "_" $
       right (inject L._TypeSpecifier L._TypeSpecifier_named $
         wrap L._Symbol (string "Record")),
     _Type_union>>: lambda "_" $
       right (inject L._TypeSpecifier L._TypeSpecifier_named $
         wrap L._Symbol (string "Union")),
     _Type_wrap>>: lambda "_" $
       right (inject L._TypeSpecifier L._TypeSpecifier_named $
         wrap L._Symbol (string "Wrapper")),
     _Type_variable>>: lambda "name" $
       right (inject L._TypeSpecifier L._TypeSpecifier_named $
         wrap L._Symbol (Core.unName (var "name"))),
     _Type_forall>>: lambda "fa" $
       encodeType @@ var "cx" @@ var "g" @@ Core.forallTypeBody (var "fa")]

-- | Encode a type body (after stripping annotations and foralls) as a Lisp top-level form.
--   Recurses through forall to reach the underlying record/union/wrap.
encodeTypeBody :: TBinding (String -> Type -> Type -> Either (InContext Error) L.TopLevelFormWithComments)
encodeTypeBody = def "encodeTypeBody" $
  lambda "lname" $ lambda "origTyp" $ lambda "typ" $
    cases _Type (var "typ") (Just $
      -- Default: emit a comment for types we can't yet represent
      right (record L._TopLevelFormWithComments [
        L._TopLevelFormWithComments_doc>>: nothing,
        L._TopLevelFormWithComments_comment>>: just (record L._Comment [
          L._Comment_style>>: inject L._CommentStyle L._CommentStyle_line unit,
          L._Comment_text>>: Strings.cat2 (Strings.cat2 (var "lname") (string " = ")) (ShowCore.type_ @@ var "origTyp")]),
        L._TopLevelFormWithComments_form>>: inject L._TopLevelForm L._TopLevelForm_expression $
          inject L._Expression L._Expression_literal $
            inject L._Literal L._Literal_nil unit]))
    [_Type_forall>>: lambda "ft" $
       -- Strip forall and recurse on the body
       encodeTypeBody @@ var "lname" @@ var "origTyp" @@ Core.forallTypeBody (var "ft"),
     _Type_record>>: lambda "rt" $
       "fields" <~ (Lists.map encodeFieldDef (var "rt")) $
         right (lispTopForm @@ (inject L._TopLevelForm L._TopLevelForm_recordType $
           record L._RecordTypeDefinition [
             L._RecordTypeDefinition_name>>: wrap L._Symbol (var "lname"),
             L._RecordTypeDefinition_fields>>: var "fields",
             L._RecordTypeDefinition_doc>>: nothing])),
     _Type_union>>: lambda "rt" $
       -- Unions become a comment + constructor functions
       -- For now, generate a variable holding the list of variant names
       "variantNames" <~ (Lists.map
         (lambda "f" $
           inject L._Expression L._Expression_literal $
             inject L._Literal L._Literal_keyword $
               record L._Keyword [
                 L._Keyword_name>>: Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.fieldTypeName (var "f")),
                 L._Keyword_namespace>>: nothing])
         (var "rt")) $
         right (lispTopForm @@ (inject L._TopLevelForm L._TopLevelForm_variable $
           record L._VariableDefinition [
             L._VariableDefinition_name>>: wrap L._Symbol (Strings.cat2 (var "lname") (string "-variants")),
             L._VariableDefinition_value>>: lispListExpr @@ var "variantNames",
             L._VariableDefinition_doc>>: just (wrap L._Docstring (Strings.cat2 (string "Variants of the ") (var "lname")))])),
     _Type_wrap>>: lambda "wt" $
       -- Newtypes become single-field records
       right (lispTopForm @@ (inject L._TopLevelForm L._TopLevelForm_recordType $
         record L._RecordTypeDefinition [
           L._RecordTypeDefinition_name>>: wrap L._Symbol (var "lname"),
           L._RecordTypeDefinition_fields>>: list [
             record L._FieldDefinition [
               L._FieldDefinition_name>>: wrap L._Symbol (string "value"),
               L._FieldDefinition_defaultValue>>: nothing]],
           L._RecordTypeDefinition_doc>>: nothing]))]

-- | Encode a Hydra type definition as a Lisp top-level form
encodeTypeDefinition :: TBinding (Context -> Graph -> TypeDefinition -> Either (InContext Error) L.TopLevelFormWithComments)
encodeTypeDefinition = def "encodeTypeDefinition" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Module.typeDefinitionName (var "tdef") $
    "typ" <~ Module.typeDefinitionType (var "tdef") $
    "lname" <~ (qualifiedSnakeName @@ var "name") $
    "dtyp" <~ (Rewriting.deannotateType @@ var "typ") $
    encodeTypeBody @@ var "lname" @@ var "typ" @@ var "dtyp"

-- | Check if a name is maybes.cases (3 args, arg 2 is lazy).
isCasesPrimitive :: TBinding (Name -> Bool)
isCasesPrimitive = def "isCasesPrimitive" $
  "name" ~>
    Equality.equal (var "name") (Core.name $ string "hydra.lib.maybes.cases")

-- | Check if a name is a 2-arg lazy primitive (default value is arg 1, i.e. the first applied arg).
-- These primitives take a default value that should only be evaluated when needed.
isLazy2ArgPrimitive :: TBinding (Name -> Bool)
isLazy2ArgPrimitive = def "isLazy2ArgPrimitive" $
  "name" ~>
    Logic.or
      (Equality.equal (var "name") (Core.name $ string "hydra.lib.eithers.fromLeft"))
      (Logic.or
        (Equality.equal (var "name") (Core.name $ string "hydra.lib.eithers.fromRight"))
        (Equality.equal (var "name") (Core.name $ string "hydra.lib.maybes.fromMaybe")))

-- | Check if a name is a 3-arg lazy primitive where arg 1 (the first applied arg) should be thunked.
isLazy3ArgPrimitive :: TBinding (Name -> Bool)
isLazy3ArgPrimitive = def "isLazy3ArgPrimitive" $
  "name" ~>
    Equality.equal (var "name") (Core.name $ string "hydra.lib.maybes.maybe")

-- | Check if a term is a reference to a specific primitive, stripping type
-- applications, type lambdas, and annotations to find the underlying primitive.
isPrimitiveRef :: TBinding (String -> Term -> Bool)
isPrimitiveRef = def "isPrimitiveRef" $
  lambda "primName" $ lambda "term" $
    cases _Term (var "term") (Just $ boolean False) [
      _Term_function>>: lambda "f" $
        cases _Function (var "f") (Just $ boolean False) [
          _Function_primitive>>: lambda "name" $
            Equality.equal (Core.unName (var "name")) (var "primName")],
      _Term_variable>>: lambda "name" $
        Equality.equal (Core.unName (var "name")) (var "primName"),
      _Term_annotated>>: lambda "at" $
        isPrimitiveRef @@ var "primName" @@ Core.annotatedTermBody (var "at"),
      _Term_typeApplication>>: lambda "ta" $
        isPrimitiveRef @@ var "primName" @@ Core.typeApplicationTermBody (var "ta"),
      _Term_typeLambda>>: lambda "tl" $
        isPrimitiveRef @@ var "primName" @@ Core.typeLambdaBody (var "tl")]

-- | Function application expression
lispApp :: TBinding (L.Expression -> [L.Expression] -> L.Expression)
lispApp = def "lispApp" $
  lambda "fun" $ lambda "args" $
    inject L._Expression L._Expression_application $
      record L._Application [
        L._Application_function>>: var "fun",
        L._Application_arguments>>: var "args"]

-- | Construct a Lisp keyword from a string
lispKeyword :: TBinding (String -> L.Expression)
lispKeyword = def "lispKeyword" $
  lambda "name" $
    inject L._Expression L._Expression_literal $
      inject L._Literal L._Literal_keyword $
        record L._Keyword [
          L._Keyword_name>>: var "name",
          L._Keyword_namespace>>: nothing]

-- | Lambda expression (unnamed)
lispLambdaExpr :: TBinding ([String] -> L.Expression -> L.Expression)
lispLambdaExpr = def "lispLambdaExpr" $
  lambda "params" $ lambda "body" $
    inject L._Expression L._Expression_lambda $
      record L._Lambda [
        L._Lambda_name>>: nothing,
        L._Lambda_params>>: Lists.map (lambda "p" $ wrap L._Symbol (var "p")) (var "params"),
        L._Lambda_restParam>>: nothing,
        L._Lambda_body>>: list [var "body"]]

-- | Construct a Lisp list expression
lispListExpr :: TBinding ([L.Expression] -> L.Expression)
lispListExpr = def "lispListExpr" $
  lambda "elements" $
    inject L._Expression L._Expression_list $
      record L._ListLiteral [
        L._ListLiteral_elements>>: var "elements",
        L._ListLiteral_quoted>>: boolean False]

-- | Wrap a literal as an expression
lispLitExpr :: TBinding (L.Literal -> L.Expression)
lispLitExpr = def "lispLitExpr" $
  lambda "lit" $
    inject L._Expression L._Expression_literal (var "lit")

-- | Named lambda expression (for Clojure self-referential fn)
lispNamedLambdaExpr :: TBinding (String -> [String] -> L.Expression -> L.Expression)
lispNamedLambdaExpr = def "lispNamedLambdaExpr" $
  lambda "name" $ lambda "params" $ lambda "body" $
    inject L._Expression L._Expression_lambda $
      record L._Lambda [
        L._Lambda_name>>: just (wrap L._Symbol (var "name")),
        L._Lambda_params>>: Lists.map (lambda "p" $ wrap L._Symbol (var "p")) (var "params"),
        L._Lambda_restParam>>: nothing,
        L._Lambda_body>>: list [var "body"]]

-- | Nil expression
lispNilExpr :: TBinding L.Expression
lispNilExpr = def "lispNilExpr" $
  inject L._Expression L._Expression_literal $
    inject L._Literal L._Literal_nil unit

-- | Construct a Lisp symbol from a string
lispSymbol :: TBinding (String -> L.Symbol)
lispSymbol = def "lispSymbol" $
  lambda "name" $
    wrap L._Symbol (var "name")

-- | Wrap a top-level form (no doc, no comment)
lispTopForm :: TBinding (L.TopLevelForm -> L.TopLevelFormWithComments)
lispTopForm = def "lispTopForm" $
  lambda "form" $
    record L._TopLevelFormWithComments [
      L._TopLevelFormWithComments_doc>>: nothing,
      L._TopLevelFormWithComments_comment>>: nothing,
      L._TopLevelFormWithComments_form>>: var "form"]

-- | Wrap a top-level form with an optional docstring
lispTopFormWithComments :: TBinding (Maybe String -> L.TopLevelForm -> L.TopLevelFormWithComments)
lispTopFormWithComments = def "lispTopFormWithComments" $
  lambda "mdoc" $ lambda "form" $
    record L._TopLevelFormWithComments [
      L._TopLevelFormWithComments_doc>>: Maybes.map (lambda "d" $ wrap L._Docstring (var "d")) (var "mdoc"),
      L._TopLevelFormWithComments_comment>>: nothing,
      L._TopLevelFormWithComments_form>>: var "form"]

-- | Variable reference expression (Lisp-1 style, function namespace = false)
lispVar :: TBinding (String -> L.Expression)
lispVar = def "lispVar" $
  lambda "name" $
    inject L._Expression L._Expression_variable $
      record L._VariableReference [
        L._VariableReference_name>>: wrap L._Symbol (var "name"),
        L._VariableReference_functionNamespace>>: boolean False]

-- | Generate export declarations for all symbols defined in a module.
--   For record type definitions: the type name, constructor (make-X), predicate (X?), and field accessors.
--   For variable definitions: the variable name.
moduleExports :: TBinding ([L.TopLevelFormWithComments] -> [L.ExportDeclaration])
moduleExports = def "moduleExports" $
  "forms" ~>
    "symbols" <~ Lists.concat (Lists.map ("fwc" ~>
      "form" <~ (project L._TopLevelFormWithComments L._TopLevelFormWithComments_form @@ var "fwc") $
      cases L._TopLevelForm (var "form") (Just (list ([] :: [TTerm L.Symbol]))) [
        L._TopLevelForm_variable>>: "vd" ~>
          list [project L._VariableDefinition L._VariableDefinition_name @@ var "vd"],
        L._TopLevelForm_recordType>>: "rdef" ~>
          "rname" <~ (unwrap L._Symbol @@ (project L._RecordTypeDefinition L._RecordTypeDefinition_name @@ var "rdef")) $
          "fields" <~ (project L._RecordTypeDefinition L._RecordTypeDefinition_fields @@ var "rdef") $
          "fieldSyms" <~ Lists.map ("f" ~>
            "fn" <~ (unwrap L._Symbol @@ (project L._FieldDefinition L._FieldDefinition_name @@ var "f")) $
            wrap L._Symbol (Strings.cat (list [var "rname", string "-", var "fn"])))
            (var "fields") $
          Lists.concat (list [
            list [
              wrap L._Symbol (Strings.cat2 (string "make-") (var "rname")),
              wrap L._Symbol (Strings.cat2 (var "rname") (string "?"))],
            var "fieldSyms"])])
      (var "forms")) $
    Logic.ifElse (Lists.null (var "symbols"))
      (list ([] :: [TTerm L.ExportDeclaration]))
      (list [record L._ExportDeclaration [
        L._ExportDeclaration_symbols>>: var "symbols"]])

-- | Reorder definitions: type definitions first, then term definitions in topological order.
--   This ensures that all forward references are resolved, making the generated code

-- | Generate import declarations from the dependency namespaces of a module's definitions.
moduleImports :: TBinding (Namespace -> [Definition] -> [L.ImportDeclaration])
moduleImports = def "moduleImports" $
  "focusNs" ~> "defs" ~>
    "depNss" <~ Sets.toList (Sets.delete (var "focusNs")
      (Schemas.definitionDependencyNamespaces @@ var "defs")) $
    Lists.map ("ns" ~>
      record L._ImportDeclaration [
        L._ImportDeclaration_module>>: wrap L._NamespaceName (Module.unNamespace (var "ns")),
        L._ImportDeclaration_spec>>: inject L._ImportSpec L._ImportSpec_all unit])
      (var "depNss")

-- | Convert a Hydra module to a Lisp program.
moduleToLisp :: TBinding (L.Dialect -> Module -> [Definition] -> Context -> Graph -> Either (InContext Error) L.Program)
moduleToLisp = def "moduleToLisp" $
  "dialect" ~> "mod" ~> "defs0" ~> "cx" ~> "g" ~>
    -- Reorder definitions: types first, then topologically sorted terms
    "defs" <~ (CoderUtils.reorderDefs @@ var "defs0") $
    "partitioned" <~ (Schemas.partitionDefinitions @@ var "defs") $
    "allTypeDefs" <~ Pairs.first (var "partitioned") $
    "termDefs" <~ Pairs.second (var "partitioned") $
    -- Filter out type aliases (non-nominal types)
    "typeDefs" <~ Lists.filter (lambda "td" $
      Schemas.isNominalType @@ Module.typeDefinitionType (var "td"))
      (var "allTypeDefs") $
    "typeItems" <<~ (Eithers.mapList (encodeTypeDefinition @@ var "cx" @@ var "g") (var "typeDefs")) $
    "termItems" <<~ (Eithers.mapList (encodeTermDefinition @@ var "dialect" @@ var "cx" @@ var "g") (var "termDefs")) $
    "allItems" <~ Lists.concat2 (var "typeItems") (var "termItems") $
    "nsName" <~ Module.unNamespace (Module.moduleNamespace (var "mod")) $
    "focusNs" <~ Module.moduleNamespace (var "mod") $
    -- Generate imports from cross-module dependencies
    "imports" <~ (moduleImports @@ var "focusNs" @@ var "defs") $
    -- Generate exports from all forms
    "exports" <~ (moduleExports @@ var "allItems") $
      right (record L._Program [
        L._Program_dialect>>: var "dialect",
        L._Program_module>>: just (record L._ModuleDeclaration [
          L._ModuleDeclaration_name>>: wrap L._NamespaceName (var "nsName"),
          L._ModuleDeclaration_doc>>: nothing]),
        L._Program_imports>>: var "imports",
        L._Program_exports>>: var "exports",
        L._Program_forms>>: var "allItems"])

-- | Convert a fully-qualified Hydra Name to a snake_case identifier string.
-- E.g. Name "hydra.reduction.alphaConvert" -> "hydra_reduction_alpha_convert"
-- E.g. Name "hydra.core.AnnotatedTerm" -> "hydra_core_annotated_term"
-- Splits on dots, converts each part to snake_case, joins with underscore.
-- Reserved words get a trailing underscore.
qualifiedSnakeName :: TBinding (Name -> String)
qualifiedSnakeName = def "qualifiedSnakeName" $
  lambda "name" $
    "raw" <~ Core.unName (var "name") $
    "parts" <~ Strings.splitOn (string ".") (var "raw") $
    "snakeParts" <~ Lists.map (lambda "p" $ Formatting.convertCaseCamelOrUnderscoreToLowerSnake @@ var "p") (var "parts") $
    "joined" <~ Strings.intercalate (string "_") (var "snakeParts") $
    Formatting.sanitizeWithUnderscores @@ LispLanguageSource.lispReservedWords @@ var "joined"

-- | Convert a fully-qualified Hydra Name to a PascalCase type identifier string.
-- E.g. Name "hydra.core.AnnotatedTerm" -> "AnnotatedTerm"
-- Type names keep PascalCase for the local part, since they are used with define-record-type.
qualifiedTypeName :: TBinding (Name -> String)
qualifiedTypeName = def "qualifiedTypeName" $
  lambda "name" $
    Formatting.capitalize @@ (Names.localNameOf @@ var "name")

-- | Wrap an expression in a zero-argument lambda for lazy evaluation.
-- Produces (fn [] expr) in Clojure, (lambda () expr) in Scheme, etc.
wrapInThunk :: TBinding (L.Expression -> L.Expression)
wrapInThunk = def "wrapInThunk" $
  "expr" ~>
    inject L._Expression L._Expression_lambda $
      record L._Lambda [
        L._Lambda_name>>: nothing,
        L._Lambda_params>>: list ([] :: [TTerm L.Symbol]),
        L._Lambda_restParam>>: nothing,
        L._Lambda_body>>: list [var "expr"]]
