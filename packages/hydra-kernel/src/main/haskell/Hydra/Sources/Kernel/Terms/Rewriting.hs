
module Hydra.Sources.Kernel.Terms.Rewriting where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  applyInsideTypeLambdasAndAnnotations,
  foldOverTerm,
  foldOverType,
  foldTermWithGraphAndPath,
  mapBeneathTypeAnnotations,
  rewriteAndFoldTerm,
  rewriteAndFoldTermWithGraph,
  rewriteAndFoldTermWithGraphAndPath,
  rewriteAndFoldTermWithPath,
  rewriteTerm,
  rewriteTermM,
  rewriteTermWithContext,
  rewriteTermWithContextM,
  rewriteTermWithGraph,
  rewriteType,
  rewriteTypeM,
  subterms,
  subtermsWithSteps,
  subtypes)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths        as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
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
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Scoping as Scoping


ns :: Namespace
ns = Namespace "hydra.rewriting"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    [Scoping.ns]
    kernelTypesNamespaces $
    Just "Core rewrite and fold combinators for terms and types"
  where
   definitions = [
     toDefinition applyInsideTypeLambdasAndAnnotations,
     toDefinition foldOverTerm,
     toDefinition foldOverType,
     toDefinition foldTermWithGraphAndPath,
     toDefinition mapBeneathTypeAnnotations,
     toDefinition rewriteAndFoldTerm,
     toDefinition rewriteAndFoldTermWithGraph,
     toDefinition rewriteAndFoldTermWithGraphAndPath,
     toDefinition rewriteAndFoldTermWithPath,
     toDefinition rewriteTerm,
     toDefinition rewriteTermM,
     toDefinition rewriteTermWithContext,
     toDefinition rewriteTermWithContextM,
     toDefinition rewriteTermWithGraph,
     toDefinition rewriteType,
     toDefinition rewriteTypeM,
     toDefinition subterms,
     toDefinition subtermsWithSteps,
     toDefinition subtypes]

applyInsideTypeLambdasAndAnnotations :: TTermDefinition ((Term -> Term) -> Term -> Term)
applyInsideTypeLambdasAndAnnotations = define "applyInsideTypeLambdasAndAnnotations" $
  doc "Apply a term-level function inside any leading type lambdas" $
  "f" ~> "term0" ~> cases _Term (var "term0")
    (Just $ var "f" @@ var "term0") [
    _Term_annotated>>: "at" ~> Core.termAnnotated $ Core.annotatedTermWithBody (var "at")
      (applyInsideTypeLambdasAndAnnotations @@ var "f" @@ (Core.annotatedTermBody $ var "at")),
    _Term_typeLambda>>: "tl" ~> Core.termTypeLambda $ Core.typeLambdaWithBody (var "tl")
      (applyInsideTypeLambdasAndAnnotations @@ var "f" @@ (Core.typeLambdaBody $ var "tl"))]

foldOverTerm :: TTermDefinition (TraversalOrder -> (x -> Term -> x) -> x -> Term -> x)
foldOverTerm = define "foldOverTerm" $
  doc "Fold over a term, traversing its subterms in the specified order" $
  "order" ~> "fld" ~> "b0" ~> "term" ~> cases _TraversalOrder (var "order") Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (foldOverTerm @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "term")
      @@ (subterms @@ var "term")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (foldOverTerm @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (subterms @@ var "term"))
      @@ var "term")]

foldOverType :: TTermDefinition (TraversalOrder -> (x -> Type -> x) -> x -> Type -> x)
foldOverType = define "foldOverType" $
  doc "Fold over a type, traversing its subtypes in the specified order" $
  "order" ~> "fld" ~> "b0" ~> "typ" ~> cases _TraversalOrder (var "order") Nothing [
    _TraversalOrder_pre>>: constant (Phantoms.fold (foldOverType @@ var "order" @@ var "fld")
      @@ (var "fld" @@ var "b0" @@ var "typ")
      @@ (subtypes @@ var "typ")),
    _TraversalOrder_post>>: constant (var "fld"
      @@ (Phantoms.fold (foldOverType @@ var "order" @@ var "fld")
        @@ (var "b0")
        @@ (subtypes @@ var "typ"))
      @@ var "typ")]


mapBeneathTypeAnnotations :: TTermDefinition ((Type -> Type) -> Type -> Type)
mapBeneathTypeAnnotations = define "mapBeneathTypeAnnotations" $
  doc "Apply a transformation to the first type beneath a chain of annotations" $
  "f" ~> "t" ~> cases _Type (var "t")
    (Just $ var "f" @@ var "t") [
    _Type_annotated>>: "at" ~> Core.typeAnnotated $ Core.annotatedType
      (mapBeneathTypeAnnotations @@ var "f" @@ (Core.annotatedTypeBody $ var "at"))
      (Core.annotatedTypeAnnotation $ var "at")]

rewriteAndFoldTerm :: TTermDefinition (((a -> Term -> (a, Term)) -> a -> Term -> (a, Term)) -> a -> Term -> (a, Term))
rewriteAndFoldTerm = define "rewriteAndFoldTerm" $
  doc "Rewrite a term, and at the same time, fold a function over it, accumulating a value" $
  "f" ~> "term0" ~>
  "fsub" <~ ("recurse" ~> "val0" ~> "term0" ~>
    "forSingle" <~ ("rec" ~> "cons" ~> "val" ~> "term" ~>
      "r" <~ var "rec" @@ var "val" @@ var "term" $
      pair (Pairs.first $ var "r") (var "cons" @@ (Pairs.second $ var "r"))) $
    "forMany" <~ ("rec" ~> "cons" ~> "val" ~> "els" ~>
      "rr" <~ Lists.foldl
        ("r" ~> "el" ~>
          "r2" <~ var "rec" @@ (Pairs.first $ var "r") @@ var "el" $
          pair (Pairs.first $ var "r2") (Lists.cons (Pairs.second $ var "r2") (Pairs.second $ var "r")))
        (pair (var "val") (list ([] :: [TTerm Term])))
        (var "els") $
      pair (Pairs.first $ var "rr") (var "cons" @@ (Lists.reverse $ Pairs.second $ var "rr"))) $
    "forField" <~ ("val" ~> "field" ~>
      "r" <~ var "recurse" @@ var "val" @@ Core.fieldTerm (var "field") $
      pair (Pairs.first $ var "r") (Core.field (Core.fieldName $ var "field") (Pairs.second $ var "r"))) $
    "forFields" <~ var "forMany" @@ var "forField" @@ ("x" ~> var "x") $
    "forPair" <~ ("val" ~> "kv" ~>
      "rk" <~ var "recurse" @@ var "val" @@ (Pairs.first $ var "kv") $
      "rv" <~ var "recurse" @@ (Pairs.first $ var "rk") @@ (Pairs.second $ var "kv") $
      pair
        (Pairs.first $ var "rv")
        (pair (Pairs.second $ var "rk") (Pairs.second $ var "rv"))) $
    "forBinding" <~ ("val" ~> "binding" ~>
      "r" <~ var "recurse" @@ var "val" @@ Core.bindingTerm (var "binding") $
      pair
        (Pairs.first $ var "r")
        (Core.binding
          (Core.bindingName $ var "binding")
          (Pairs.second $ var "r")
          (Core.bindingType $ var "binding"))) $
    "dflt" <~ pair (var "val0") (var "term0") $
    cases _Term (var "term0")
      (Just $ var "dflt") [
      _Term_annotated>>: "at" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termAnnotated $ Core.annotatedTerm (var "t") (Core.annotatedTermAnnotation $ var "at"))
        @@ var "val0"
        @@ (Core.annotatedTermBody $ var "at"),
      _Term_application>>: "a" ~>
        "rlhs" <~ var "recurse" @@ var "val0" @@ (Core.applicationFunction $ var "a") $
        "rrhs" <~ var "recurse" @@ (Pairs.first $ var "rlhs") @@ (Core.applicationArgument $ var "a") $
        pair
          (Pairs.first $ var "rrhs")
          (Core.termApplication $ Core.application
            (Pairs.second $ var "rlhs")
            (Pairs.second $ var "rrhs")),
      _Term_cases>>: "cs" ~>
        "rmd" <~ Maybes.map (var "recurse" @@ var "val0") (Core.caseStatementDefault $ var "cs") $
        "val1" <~ optCases (var "rmd")
          (var "val0")
          (unaryFunction Pairs.first) $
        "rcases" <~ var "forFields" @@ var "val1" @@ (Core.caseStatementCases $ var "cs") $
        pair
          (Pairs.first $ var "rcases")
          (Core.termCases $ Core.caseStatement
            (Core.caseStatementTypeName $ var "cs")
            (Maybes.map (unaryFunction Pairs.second) (var "rmd"))
            (Pairs.second $ var "rcases")),
      _Term_either>>: "e" ~> Eithers.either_
        ("l" ~>
          "rl" <~ var "recurse" @@ var "val0" @@ var "l" $
          pair (Pairs.first $ var "rl") (Core.termEither $ left $ Pairs.second $ var "rl"))
        ("r" ~>
          "rr" <~ var "recurse" @@ var "val0" @@ var "r" $
          pair (Pairs.first $ var "rr") (Core.termEither $ right $ Pairs.second $ var "rr"))
        (var "e"),
      _Term_lambda>>: "l" ~>
        "rl" <~ var "recurse" @@ var "val0" @@ (Core.lambdaBody $ var "l") $
        pair
          (Pairs.first $ var "rl")
          (Core.termLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Core.lambdaDomain $ var "l")
            (Pairs.second $ var "rl")),
      _Term_let>>: "l" ~>
        "renv" <~ var "recurse" @@ var "val0" @@ (Core.letBody $ var "l") $
        var "forMany" @@ var "forBinding"
          @@ ("bins" ~> Core.termLet $ Core.let_ (var "bins") (Pairs.second $ var "renv"))
          @@ Pairs.first (var "renv") @@ (Core.letBindings $ var "l"),
      _Term_list>>: "els" ~> var "forMany" @@ var "recurse" @@ (unaryFunction Core.termList) @@ var "val0" @@ var "els",
      _Term_map>>: "m" ~> var "forMany" @@ var "forPair"
        @@ ("pairs" ~> Core.termMap $ Maps.fromList $ var "pairs") @@ var "val0" @@ Maps.toList (var "m"),
      _Term_maybe>>: "mt" ~> optCases (var "mt")
        (var "dflt")
        ("t" ~> var "forSingle"
          @@ var "recurse"
          @@ ("t1" ~> Core.termMaybe $ just $ var "t1")
          @@ var "val0"
          @@ var "t"),
      _Term_pair>>: "p" ~>
        "rf" <~ var "recurse" @@ var "val0" @@ (Pairs.first $ var "p") $
        "rs" <~ var "recurse" @@ (Pairs.first $ var "rf") @@ (Pairs.second $ var "p") $
        pair (Pairs.first $ var "rs") (Core.termPair $ pair (Pairs.second $ var "rf") (Pairs.second $ var "rs")),
      _Term_project>>: "p" ~> pair (var "val0") (Core.termProject $ var "p"),
      _Term_record>>: "r" ~> var "forMany"
        @@ var "forField"
        @@ ("fields" ~> Core.termRecord $ Core.record (Core.recordTypeName $ var "r") (var "fields"))
        @@ var "val0"
        @@ Core.recordFields (var "r" ),
      _Term_set>>: "els" ~> var "forMany"
        @@ var "recurse"
        @@ ("e" ~> Core.termSet $ Sets.fromList $ var "e")
        @@ var "val0"
        @@ (Sets.toList $ var "els"),
      _Term_typeApplication>>: "ta" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeApplicationTermType $ var "ta"))
        @@ var "val0"
        @@ (Core.typeApplicationTermBody $ var "ta"),
      _Term_typeLambda>>: "tl" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeLambda $ Core.typeLambda (Core.typeLambdaParameter $ var "tl") (var "t"))
        @@ var "val0"
        @@ (Core.typeLambdaBody $ var "tl"),
      _Term_union>>: "inj" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termUnion $ Core.injection
          (Core.injectionTypeName $ var "inj")
          (Core.field (Core.fieldName $ Core.injectionField $ var "inj") (var "t")))
        @@ var "val0"
        @@ (Core.fieldTerm $ Core.injectionField $ var "inj"),
      _Term_unwrap>>: "n" ~> pair (var "val0") (Core.termUnwrap $ var "n"),
      _Term_wrap>>: "wt" ~> var "forSingle"
        @@ var "recurse"
        @@ ("t" ~> Core.termWrap $ Core.wrappedTerm (Core.wrappedTermTypeName $ var "wt") (var "t"))
        @@ var "val0"
        @@ (Core.wrappedTermBody $ var "wt")]) $
--  rewrite @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "term0"

-- | Rewrite a term with path tracking, and fold a function over it.
-- The path is the list of accessors from the root to the current term.
-- The function f receives: (recurse path acc term -> (acc', term')) -> path -> acc -> term -> (acc', term')
rewriteAndFoldTermWithPath :: TTermDefinition ((([SubtermStep] -> a -> Term -> (a, Term)) -> [SubtermStep] -> a -> Term -> (a, Term)) -> a -> Term -> (a, Term))
rewriteAndFoldTermWithPath = define "rewriteAndFoldTermWithPath" $
  doc "Rewrite a term with path tracking, and fold a function over it, accumulating a value. The path is a list of SubtermSteps from root to current position." $
  "f" ~> "term0" ~>
  "fsub" <~ ("recurse" ~> "path" ~> "val0" ~> "term0" ~>
    -- Helper to recurse into a single subterm with a given accessor
    "forSingleWithAccessor" <~ ("rec" ~> "cons" ~> "accessor" ~> "val" ~> "term" ~>
      "r" <~ var "rec" @@ Lists.concat2 (var "path") (list [var "accessor"]) @@ var "val" @@ var "term" $
      pair (Pairs.first $ var "r") (var "cons" @@ (Pairs.second $ var "r"))) $
    -- Helper to recurse into multiple subterms, each with its own accessor
    "forManyWithAccessors" <~ ("rec" ~> "cons" ~> "val" ~> "accessorTermPairs" ~>
      "rr" <~ Lists.foldl
        ("r" ~> "atp" ~>
          "r2" <~ var "rec"
            @@ Lists.concat2 (var "path") (list [Pairs.first $ var "atp"])
            @@ (Pairs.first $ var "r")
            @@ (Pairs.second $ var "atp") $
          pair (Pairs.first $ var "r2") (Lists.cons (Pairs.second $ var "r2") (Pairs.second $ var "r")))
        (pair (var "val") (list ([] :: [TTerm Term])))
        (var "accessorTermPairs") $
      pair (Pairs.first $ var "rr") (var "cons" @@ (Lists.reverse $ Pairs.second $ var "rr"))) $
    -- Helper for record/case fields with accessors
    "forFieldWithAccessor" <~ ("mkAccessor" ~> "val" ~> "field" ~>
      "r" <~ var "recurse"
        @@ Lists.concat2 (var "path") (list [var "mkAccessor" @@ (Core.fieldName $ var "field")])
        @@ var "val"
        @@ Core.fieldTerm (var "field") $
      pair (Pairs.first $ var "r") (Core.field (Core.fieldName $ var "field") (Pairs.second $ var "r"))) $
    "forFieldsWithAccessor" <~ ("mkAccessor" ~> var "forManyWithAccessors" @@
      ("path1" ~> "val1" ~> "field1" ~> var "forFieldWithAccessor" @@ var "mkAccessor" @@ var "val1" @@ var "field1")
      @@ ("x" ~> var "x")) $
    -- Helper for map key/value pairs
    "forPairWithAccessors" <~ ("keyAccessor" ~> "valAccessor" ~> "val" ~> "kv" ~>
      "rk" <~ var "recurse"
        @@ Lists.concat2 (var "path") (list [var "keyAccessor"])
        @@ var "val"
        @@ (Pairs.first $ var "kv") $
      "rv" <~ var "recurse"
        @@ Lists.concat2 (var "path") (list [var "valAccessor"])
        @@ (Pairs.first $ var "rk")
        @@ (Pairs.second $ var "kv") $
      pair
        (Pairs.first $ var "rv")
        (pair (Pairs.second $ var "rk") (Pairs.second $ var "rv"))) $
    -- Helper for let bindings
    "forBindingWithAccessor" <~ ("val" ~> "binding" ~>
      "r" <~ var "recurse"
        @@ Lists.concat2 (var "path") (list [Paths.subtermStepLetBinding $ Core.bindingName $ var "binding"])
        @@ var "val"
        @@ Core.bindingTerm (var "binding") $
      pair
        (Pairs.first $ var "r")
        (Core.binding
          (Core.bindingName $ var "binding")
          (Pairs.second $ var "r")
          (Core.bindingType $ var "binding"))) $
    "dflt" <~ pair (var "val0") (var "term0") $
    cases _Term (var "term0")
      (Just $ var "dflt") [
      _Term_annotated>>: "at" ~> var "forSingleWithAccessor"
        @@ var "recurse"
        @@ ("t" ~> Core.termAnnotated $ Core.annotatedTerm (var "t") (Core.annotatedTermAnnotation $ var "at"))
        @@ Paths.subtermStepAnnotatedBody
        @@ var "val0"
        @@ (Core.annotatedTermBody $ var "at"),
      _Term_application>>: "a" ~>
        "rlhs" <~ var "recurse"
          @@ Lists.concat2 (var "path") (list [Paths.subtermStepApplicationFunction])
          @@ var "val0"
          @@ (Core.applicationFunction $ var "a") $
        "rrhs" <~ var "recurse"
          @@ Lists.concat2 (var "path") (list [Paths.subtermStepApplicationArgument])
          @@ (Pairs.first $ var "rlhs")
          @@ (Core.applicationArgument $ var "a") $
        pair
          (Pairs.first $ var "rrhs")
          (Core.termApplication $ Core.application
            (Pairs.second $ var "rlhs")
            (Pairs.second $ var "rrhs")),
      _Term_cases>>: "cs" ~>
        "rmd" <~ Maybes.map
          ("def" ~> var "recurse"
            @@ Lists.concat2 (var "path") (list [Paths.subtermStepUnionCasesDefault])
            @@ var "val0"
            @@ var "def")
          (Core.caseStatementDefault $ var "cs") $
        "val1" <~ optCases (var "rmd")
          (var "val0")
          (unaryFunction Pairs.first) $
        "rcases" <~ var "forManyWithAccessors"
          @@ var "recurse"
          @@ ("x" ~> var "x")
          @@ var "val1"
          @@ Lists.map
            ("f" ~> pair
              (Paths.subtermStepUnionCasesBranch $ Core.fieldName $ var "f")
              (Core.fieldTerm $ var "f"))
            (Core.caseStatementCases $ var "cs") $
        pair
          (Pairs.first $ var "rcases")
          (Core.termCases $ Core.caseStatement
            (Core.caseStatementTypeName $ var "cs")
            (Maybes.map (unaryFunction Pairs.second) (var "rmd"))
            (Lists.map
              ("ft" ~> Core.field
                (Pairs.first $ var "ft")
                (Pairs.second $ var "ft"))
              (Lists.zip
                (Lists.map (unaryFunction Core.fieldName) (Core.caseStatementCases $ var "cs"))
                (Pairs.second $ var "rcases")))),
      _Term_either>>: "e" ~> Eithers.either_
        ("l" ~>
          "rl" <~ var "recurse"
            @@ Lists.concat2 (var "path") (list [Paths.subtermStepSumTerm])
            @@ var "val0"
            @@ var "l" $
          pair (Pairs.first $ var "rl") (Core.termEither $ left $ Pairs.second $ var "rl"))
        ("r" ~>
          "rr" <~ var "recurse"
            @@ Lists.concat2 (var "path") (list [Paths.subtermStepSumTerm])
            @@ var "val0"
            @@ var "r" $
          pair (Pairs.first $ var "rr") (Core.termEither $ right $ Pairs.second $ var "rr"))
        (var "e"),
      _Term_lambda>>: "l" ~>
        "rl" <~ var "recurse"
          @@ Lists.concat2 (var "path") (list [Paths.subtermStepLambdaBody])
          @@ var "val0"
          @@ (Core.lambdaBody $ var "l") $
        pair
          (Pairs.first $ var "rl")
          (Core.termLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (Core.lambdaDomain $ var "l")
            (Pairs.second $ var "rl")),
      _Term_let>>: "l" ~>
        "renv" <~ var "recurse"
          @@ Lists.concat2 (var "path") (list [Paths.subtermStepLetBody])
          @@ var "val0"
          @@ (Core.letBody $ var "l") $
        "rbindings" <~ Lists.foldl
          ("r" ~> "binding" ~>
            "rb" <~ var "forBindingWithAccessor" @@ (Pairs.first $ var "r") @@ var "binding" $
            pair (Pairs.first $ var "rb") (Lists.cons (Pairs.second $ var "rb") (Pairs.second $ var "r")))
          (pair (Pairs.first $ var "renv") (list ([] :: [TTerm Binding])))
          (Core.letBindings $ var "l") $
        pair
          (Pairs.first $ var "rbindings")
          (Core.termLet $ Core.let_ (Lists.reverse $ Pairs.second $ var "rbindings") (Pairs.second $ var "renv")),
      _Term_list>>: "els" ~>
        "idx" <~ int32 0 $
        "rr" <~ Lists.foldl
          ("r" ~> "el" ~>
            "r2" <~ var "recurse"
              @@ Lists.concat2 (var "path") (list [Paths.subtermStepListElement $ Pairs.first $ var "r"])
              @@ (Pairs.first $ Pairs.second $ var "r")
              @@ var "el" $
            pair
              (Math.add (Pairs.first $ var "r") (int32 1))
              (pair (Pairs.first $ var "r2") (Lists.cons (Pairs.second $ var "r2") (Pairs.second $ Pairs.second $ var "r"))))
          (pair (var "idx") (pair (var "val0") (list ([] :: [TTerm Term]))))
          (var "els") $
        pair (Pairs.first $ Pairs.second $ var "rr") (Core.termList $ Lists.reverse $ Pairs.second $ Pairs.second $ var "rr"),
      _Term_map>>: "m" ~>
        "idx" <~ int32 0 $
        "rr" <~ Lists.foldl
          ("r" ~> "kv" ~>
            "rk" <~ var "recurse"
              @@ Lists.concat2 (var "path") (list [Paths.subtermStepMapKey $ Pairs.first $ var "r"])
              @@ (Pairs.first $ Pairs.second $ var "r")
              @@ (Pairs.first $ var "kv") $
            "rv" <~ var "recurse"
              @@ Lists.concat2 (var "path") (list [Paths.subtermStepMapValue $ Pairs.first $ var "r"])
              @@ (Pairs.first $ var "rk")
              @@ (Pairs.second $ var "kv") $
            pair
              (Math.add (Pairs.first $ var "r") (int32 1))
              (pair
                (Pairs.first $ var "rv")
                (Lists.cons (pair (Pairs.second $ var "rk") (Pairs.second $ var "rv")) (Pairs.second $ Pairs.second $ var "r"))))
          (pair (var "idx") (pair (var "val0") (list ([] :: [TTerm (Term, Term)]))))
          (Maps.toList $ var "m") $
        pair (Pairs.first $ Pairs.second $ var "rr") (Core.termMap $ Maps.fromList $ Lists.reverse $ Pairs.second $ Pairs.second $ var "rr"),
      _Term_maybe>>: "mt" ~> optCases (var "mt")
        (var "dflt")
        ("t" ~> var "forSingleWithAccessor"
          @@ var "recurse"
          @@ ("t1" ~> Core.termMaybe $ just $ var "t1")
          @@ Paths.subtermStepMaybeTerm
          @@ var "val0"
          @@ var "t"),
      _Term_pair>>: "p" ~>
        "rf" <~ var "recurse"
          @@ Lists.concat2 (var "path") (list [Paths.subtermStepProductTerm $ int32 0])
          @@ var "val0"
          @@ (Pairs.first $ var "p") $
        "rs" <~ var "recurse"
          @@ Lists.concat2 (var "path") (list [Paths.subtermStepProductTerm $ int32 1])
          @@ (Pairs.first $ var "rf")
          @@ (Pairs.second $ var "p") $
        pair (Pairs.first $ var "rs") (Core.termPair $ pair (Pairs.second $ var "rf") (Pairs.second $ var "rs")),
      _Term_project>>: "p" ~> pair (var "val0") (Core.termProject $ var "p"),
      _Term_record>>: "r" ~>
        "rfields" <~ var "forManyWithAccessors"
          @@ var "recurse"
          @@ ("x" ~> var "x")
          @@ var "val0"
          @@ Lists.map
            ("f" ~> pair
              (Paths.subtermStepRecordField $ Core.fieldName $ var "f")
              (Core.fieldTerm $ var "f"))
            (Core.recordFields $ var "r") $
        pair
          (Pairs.first $ var "rfields")
          (Core.termRecord $ Core.record
            (Core.recordTypeName $ var "r")
            (Lists.map
              ("ft" ~> Core.field (Pairs.first $ var "ft") (Pairs.second $ var "ft"))
              (Lists.zip
                (Lists.map (unaryFunction Core.fieldName) (Core.recordFields $ var "r"))
                (Pairs.second $ var "rfields")))),
      _Term_set>>: "els" ~>
        "idx" <~ int32 0 $
        "rr" <~ Lists.foldl
          ("r" ~> "el" ~>
            "r2" <~ var "recurse"
              @@ Lists.concat2 (var "path") (list [Paths.subtermStepSetElement $ Pairs.first $ var "r"])
              @@ (Pairs.first $ Pairs.second $ var "r")
              @@ var "el" $
            pair
              (Math.add (Pairs.first $ var "r") (int32 1))
              (pair (Pairs.first $ var "r2") (Lists.cons (Pairs.second $ var "r2") (Pairs.second $ Pairs.second $ var "r"))))
          (pair (var "idx") (pair (var "val0") (list ([] :: [TTerm Term]))))
          (Sets.toList $ var "els") $
        pair (Pairs.first $ Pairs.second $ var "rr") (Core.termSet $ Sets.fromList $ Lists.reverse $ Pairs.second $ Pairs.second $ var "rr"),
      _Term_typeApplication>>: "ta" ~> var "forSingleWithAccessor"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeApplicationTermType $ var "ta"))
        @@ Paths.subtermStepTypeApplicationTerm
        @@ var "val0"
        @@ (Core.typeApplicationTermBody $ var "ta"),
      _Term_typeLambda>>: "tl" ~> var "forSingleWithAccessor"
        @@ var "recurse"
        @@ ("t" ~> Core.termTypeLambda $ Core.typeLambda (Core.typeLambdaParameter $ var "tl") (var "t"))
        @@ Paths.subtermStepTypeLambdaBody
        @@ var "val0"
        @@ (Core.typeLambdaBody $ var "tl"),
      _Term_union>>: "inj" ~> var "forSingleWithAccessor"
        @@ var "recurse"
        @@ ("t" ~> Core.termUnion $ Core.injection
          (Core.injectionTypeName $ var "inj")
          (Core.field (Core.fieldName $ Core.injectionField $ var "inj") (var "t")))
        @@ Paths.subtermStepInjectionTerm
        @@ var "val0"
        @@ (Core.fieldTerm $ Core.injectionField $ var "inj"),
      _Term_unwrap>>: "n" ~> pair (var "val0") (Core.termUnwrap $ var "n"),
      _Term_wrap>>: "wt" ~> var "forSingleWithAccessor"
        @@ var "recurse"
        @@ ("t" ~> Core.termWrap $ Core.wrappedTerm (Core.wrappedTermTypeName $ var "wt") (var "t"))
        @@ Paths.subtermStepWrappedTerm
        @@ var "val0"
        @@ (Core.wrappedTermBody $ var "wt")]) $
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ list ([] :: [TTerm SubtermStep]) @@ var "term0"

rewriteTerm :: TTermDefinition (((Term -> Term) -> Term -> Term) -> Term -> Term)
rewriteTerm = define "rewriteTerm" $ "f" ~> "term0" ~>
  "fsub" <~ ("recurse" ~> "term" ~>
    "forField" <~ ("f" ~> Core.fieldWithTerm (var "f") (var "recurse" @@ (Core.fieldTerm $ var "f"))) $
    "forLet" <~ ("lt" ~>
      "mapBinding" <~ ("b" ~> Core.binding
        (Core.bindingName $ var "b")
        (var "recurse" @@ (Core.bindingTerm $ var "b"))
        (Core.bindingType $ var "b")) $
      Core.let_
        (Lists.map (var "mapBinding") (Core.letBindings $ var "lt"))
        (var "recurse" @@ (Core.letBody $ var "lt"))) $
    "forMap" <~ ("m" ~>
      "forPair" <~ ("p" ~> pair (var "recurse" @@ (Pairs.first $ var "p")) (var "recurse" @@ (Pairs.second $ var "p"))) $
      Maps.fromList $ Lists.map (var "forPair") $ Maps.toList $ var "m") $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~> Core.termAnnotated $ Core.annotatedTerm
        (var "recurse" @@ (Core.annotatedTermBody $ var "at"))
        (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "a" ~> Core.termApplication $ Core.application
        (var "recurse" @@ (Core.applicationFunction $ var "a"))
        (var "recurse" @@ (Core.applicationArgument $ var "a")),
      _Term_cases>>: "cs" ~> Core.termCases $ Core.caseStatement
        (Core.caseStatementTypeName $ var "cs")
        (Maybes.map (var "recurse") (Core.caseStatementDefault $ var "cs"))
        (Lists.map (var "forField") (Core.caseStatementCases $ var "cs")),
      _Term_either>>: "e" ~> Core.termEither $ Eithers.either_
        ("l" ~> left $ var "recurse" @@ var "l")
        ("r" ~> right $ var "recurse" @@ var "r")
        (var "e"),
      _Term_lambda>>: "l" ~> Core.termLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        (Core.lambdaDomain $ var "l")
        (var "recurse" @@ (Core.lambdaBody $ var "l")),
      _Term_let>>: "lt" ~> Core.termLet $ var "forLet" @@ var "lt",
      _Term_list>>: "els" ~> Core.termList $ Lists.map (var "recurse") (var "els"),
      _Term_literal>>: "v" ~> Core.termLiteral $ var "v",
      _Term_map>>: "m" ~> Core.termMap $ var "forMap" @@ var "m",
      _Term_maybe>>: "m" ~> Core.termMaybe $ Maybes.map (var "recurse") (var "m"),
      _Term_pair>>: "p" ~> Core.termPair $ pair
        (var "recurse" @@ (Pairs.first $ var "p"))
        (var "recurse" @@ (Pairs.second $ var "p")),
      _Term_project>>: "p" ~> Core.termProject $ var "p",
      _Term_record>>: "r" ~> Core.termRecord $ Core.record
        (Core.recordTypeName $ var "r")
        (Lists.map (var "forField") (Core.recordFields $ var "r")),
      _Term_set>>: "s" ~> Core.termSet $ Sets.fromList $ Lists.map (var "recurse") $ Sets.toList (var "s"),
      _Term_typeApplication>>: "tt" ~> Core.termTypeApplication $ Core.typeApplicationTerm
        (var "recurse" @@ (Core.typeApplicationTermBody $ var "tt"))
        (Core.typeApplicationTermType $ var "tt"),
      _Term_typeLambda>>: "ta" ~> Core.termTypeLambda $ Core.typeLambda
        (Core.typeLambdaParameter $ var "ta")
        (var "recurse" @@ (Core.typeLambdaBody $ var "ta")),
      _Term_union>>: "i" ~> Core.termUnion $ Core.injection
        (Core.injectionTypeName $ var "i")
        (var "forField" @@ (Core.injectionField $ var "i")),
      _Term_unit>>: constant Core.termUnit,
      _Term_unwrap>>: "n" ~> Core.termUnwrap $ var "n",
      _Term_variable>>: "v" ~> Core.termVariable $ var "v",
      _Term_wrap>>: "wt" ~> Core.termWrap $ Core.wrappedTerm
        (Core.wrappedTermTypeName $ var "wt")
        (var "recurse" @@ (Core.wrappedTermBody $ var "wt"))]) $
--  rewrite @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "term0"

rewriteTermM :: TTermDefinition (((Term -> Prelude.Either e Term) -> Term -> Prelude.Either e Term) -> Term -> Prelude.Either e Term)
rewriteTermM = define "rewriteTermM" $
  doc "Either-based term rewriting with custom transformation function" $
  "f" ~> "term0" ~>
  "fsub" <~ ("recurse" ~> "term" ~>
    "forField" <~ ("field" ~>
      "t" <<~ var "recurse" @@ Core.fieldTerm (var "field") $
      right $ Core.fieldWithTerm (var "field") (var "t")) $
    "forPair" <~ ("kv" ~>
      "k" <<~ var "recurse" @@ (Pairs.first $ var "kv") $
      "v" <<~ var "recurse" @@ (Pairs.second $ var "kv") $
      right $ pair (var "k") (var "v")) $
    "mapBinding" <~ ("b" ~>
      "v" <<~ var "recurse" @@ (Core.bindingTerm $ var "b") $
      right $ Core.binding (Core.bindingName $ var "b") (var "v") (Core.bindingType $ var "b")) $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~>
        "ex" <<~ var "recurse" @@ Core.annotatedTermBody (var "at") $
        right $ Core.termAnnotated $ Core.annotatedTerm (var "ex") (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "app" ~>
        "lhs" <<~ var "recurse" @@ Core.applicationFunction (var "app") $
        "rhs" <<~ var "recurse" @@ Core.applicationArgument (var "app") $
        right $ Core.termApplication $ Core.application (var "lhs") (var "rhs"),
      _Term_cases>>: "cs" ~>
        "n" <~ Core.caseStatementTypeName (var "cs") $
        "def" <~ Core.caseStatementDefault (var "cs") $
        "csCases" <~ Core.caseStatementCases (var "cs") $
        "rdef" <<~ Maybes.maybe (right nothing)
          ("t" ~> Eithers.map (unaryFunction just) $ var "recurse" @@ var "t")
          (var "def") $
        Eithers.map
          ("rcases" ~> Core.termCases $
            Core.caseStatement (var "n") (var "rdef") (var "rcases"))
          (Eithers.mapList (var "forField") (var "csCases")),
      _Term_either>>: "e" ~>
        "re" <<~ Eithers.either_
          ("l" ~> Eithers.map (unaryFunction left) $ var "recurse" @@ var "l")
          ("r" ~> Eithers.map (unaryFunction right) $ var "recurse" @@ var "r")
          (var "e") $
        right $ Core.termEither $ var "re",
      _Term_lambda>>: "l" ~>
        "v" <~ Core.lambdaParameter (var "l") $
        "d" <~ Core.lambdaDomain (var "l") $
        "body" <~ Core.lambdaBody (var "l") $
        "rbody" <<~ var "recurse" @@ var "body" $
        right $ Core.termLambda $ Core.lambda (var "v") (var "d") (var "rbody"),
      _Term_let>>: "lt" ~>
        "bindings" <~ Core.letBindings (var "lt") $
        "env" <~ Core.letBody (var "lt") $
        "rbindings" <<~ Eithers.mapList (var "mapBinding") (var "bindings") $
        "renv" <<~ var "recurse" @@ var "env" $
        right $ Core.termLet $ Core.let_ (var "rbindings") (var "renv"),
      _Term_list>>: "els" ~>
        "rels" <<~ Eithers.mapList (var "recurse") (var "els") $
        right $ Core.termList $ var "rels",
      _Term_literal>>: "v" ~> right $ Core.termLiteral $ var "v",
      _Term_map>>: "m" ~>
        "pairs" <<~ Eithers.mapList (var "forPair") (Maps.toList $ var "m") $
        right $ Core.termMap $ Maps.fromList $ var "pairs",
      _Term_maybe>>: "m" ~>
        "rm" <<~ Eithers.mapMaybe (var "recurse") (var "m") $
        right $ Core.termMaybe $ var "rm",
      _Term_pair>>: "p" ~>
        "rf" <<~ var "recurse" @@ (Pairs.first $ var "p") $
        "rs" <<~ var "recurse" @@ (Pairs.second $ var "p") $
        right $ Core.termPair $ pair (var "rf") (var "rs"),
      _Term_project>>: "p" ~> right $ Core.termProject $ var "p",
      _Term_record>>: "r" ~>
        "n" <~ Core.recordTypeName (var "r") $
        "fields" <~ Core.recordFields (var "r") $
        Eithers.map
          ("rfields" ~> Core.termRecord $ Core.record (var "n") (var "rfields"))
          (Eithers.mapList (var "forField") (var "fields")),
      _Term_set>>: "s" ~>
        "rlist" <<~ Eithers.mapList (var "recurse") (Sets.toList $ var "s") $
        right $ Core.termSet $ Sets.fromList $ var "rlist",
      _Term_typeApplication>>: "tt" ~>
        "t" <<~ var "recurse" @@ Core.typeApplicationTermBody (var "tt") $
        right $ Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeApplicationTermType (var "tt")),
      _Term_typeLambda>>: "tl" ~>
        "v" <~ Core.typeLambdaParameter (var "tl") $
        "body" <~ Core.typeLambdaBody (var "tl") $
        "rbody" <<~ var "recurse" @@ var "body" $
        right $ Core.termTypeLambda $ Core.typeLambda (var "v") (var "rbody"),
      _Term_union>>: "i" ~>
        "n" <~ Core.injectionTypeName (var "i") $
        "field" <~ Core.injectionField (var "i") $
        Eithers.map
          ("rfield" ~> Core.termUnion $ Core.injection (var "n") (var "rfield"))
          (var "forField" @@ var "field"),
      _Term_unit>>: constant $ right $ Core.termUnit,
      _Term_unwrap>>: "n" ~> right $ Core.termUnwrap $ var "n",
      _Term_variable>>: "v" ~> right $ Core.termVariable $ var "v",
      _Term_wrap>>: "wt" ~>
        "name" <~ Core.wrappedTermTypeName (var "wt") $
        "t" <~ Core.wrappedTermBody (var "wt") $
        "rt" <<~ var "recurse" @@ var "t" $
        right $ Core.termWrap $ Core.wrappedTerm (var "name") (var "rt")]) $
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "term0"

rewriteTermWithContext :: TTermDefinition (((a -> Term -> Term) -> a -> Term -> Term) -> a -> Term -> Term)
rewriteTermWithContext = define "rewriteTermWithContext" $
  doc ("A variant of rewriteTerm which allows a context (e.g. a TypeContext)"
    <> " to be passed down to all subterms during rewriting") $
  "f" ~> "cx0" ~> "term0" ~>
  "forSubterms" <~ ("recurse0" ~> "cx" ~> "term" ~>
    "recurse" <~ var "recurse0" @@ var "cx" $
    "forField" <~ ("field" ~> Core.fieldWithTerm (var "field") (var "recurse" @@ (Core.fieldTerm $ var "field"))) $
    "forLet" <~ ("lt" ~>
      "mapBinding" <~ ("b" ~> Core.binding
        (Core.bindingName $ var "b")
        (var "recurse" @@ (Core.bindingTerm $ var "b"))
        (Core.bindingType $ var "b")) $
      Core.let_
        (Lists.map (var "mapBinding") (Core.letBindings $ var "lt"))
        (var "recurse" @@ (Core.letBody $ var "lt"))) $
    "forMap" <~ ("m" ~>
      "forPair" <~ ("p" ~> pair (var "recurse" @@ (Pairs.first $ var "p")) (var "recurse" @@ (Pairs.second $ var "p"))) $
      Maps.fromList $ Lists.map (var "forPair") $ Maps.toList $ var "m") $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~> Core.termAnnotated $ Core.annotatedTerm
        (var "recurse" @@ (Core.annotatedTermBody $ var "at"))
        (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "a" ~> Core.termApplication $ Core.application
        (var "recurse" @@ (Core.applicationFunction $ var "a"))
        (var "recurse" @@ (Core.applicationArgument $ var "a")),
      _Term_cases>>: "cs" ~> Core.termCases $ Core.caseStatement
        (Core.caseStatementTypeName $ var "cs")
        (Maybes.map (var "recurse") (Core.caseStatementDefault $ var "cs"))
        (Lists.map (var "forField") (Core.caseStatementCases $ var "cs")),
      _Term_either>>: "e" ~> Core.termEither $ Eithers.either_
        ("l" ~> left $ var "recurse" @@ var "l")
        ("r" ~> right $ var "recurse" @@ var "r")
        (var "e"),
      _Term_lambda>>: "l" ~> Core.termLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        (Core.lambdaDomain $ var "l")
        (var "recurse" @@ (Core.lambdaBody $ var "l")),
      _Term_let>>: "lt" ~> Core.termLet $ var "forLet" @@ var "lt",
      _Term_list>>: "els" ~> Core.termList $ Lists.map (var "recurse") (var "els"),
      _Term_literal>>: "v" ~> Core.termLiteral $ var "v",
      _Term_map>>: "m" ~> Core.termMap $ var "forMap" @@ var "m",
      _Term_maybe>>: "m" ~> Core.termMaybe $ Maybes.map (var "recurse") (var "m"),
      _Term_pair>>: "p" ~> Core.termPair $ pair
        (var "recurse" @@ (Pairs.first $ var "p"))
        (var "recurse" @@ (Pairs.second $ var "p")),
      _Term_project>>: "p" ~> Core.termProject $ var "p",
      _Term_record>>: "r" ~> Core.termRecord $ Core.record
        (Core.recordTypeName $ var "r")
        (Lists.map (var "forField") (Core.recordFields $ var "r")),
      _Term_set>>: "s" ~> Core.termSet $ Sets.fromList $ Lists.map (var "recurse") $ Sets.toList (var "s"),
      _Term_typeApplication>>: "tt" ~> Core.termTypeApplication $ Core.typeApplicationTerm
        (var "recurse" @@ (Core.typeApplicationTermBody $ var "tt"))
        (Core.typeApplicationTermType $ var "tt"),
      _Term_typeLambda>>: "ta" ~> Core.termTypeLambda $ Core.typeLambda
        (Core.typeLambdaParameter $ var "ta")
        (var "recurse" @@ (Core.typeLambdaBody $ var "ta")),
      _Term_union>>: "i" ~> Core.termUnion $ Core.injection
        (Core.injectionTypeName $ var "i")
        (var "forField" @@ (Core.injectionField $ var "i")),
      _Term_unit>>: constant Core.termUnit,
      _Term_unwrap>>: "n" ~> Core.termUnwrap $ var "n",
      _Term_variable>>: "v" ~> Core.termVariable $ var "v",
      _Term_wrap>>: "wt" ~> Core.termWrap $ Core.wrappedTerm
        (Core.wrappedTermTypeName $ var "wt")
        (var "recurse" @@ (Core.wrappedTermBody $ var "wt"))]) $
  "rewrite" <~ ("cx" ~> "term" ~> var "f" @@ (var "forSubterms" @@ var "rewrite") @@ var "cx" @@ var "term") $
  var "rewrite" @@ var "cx0" @@ var "term0"

rewriteTermWithContextM :: TTermDefinition (((a -> Term -> Prelude.Either e Term) -> a -> Term -> Prelude.Either e Term) -> a -> Term -> Prelude.Either e Term)
rewriteTermWithContextM = define "rewriteTermWithContextM" $
  doc ("Either-based variant of rewriteTermWithContextM which allows a context (e.g. a TypeContext)"
    <> " to be passed down to all subterms during rewriting") $
  "f" ~> "cx0" ~> "term0" ~>
  "forSubterms" <~ ("recurse0" ~> "cx" ~> "term" ~>
    "recurse" <~ var "recurse0" @@ var "cx" $
    "forField" <~ ("field" ~>
      "t" <<~ var "recurse" @@ Core.fieldTerm (var "field") $
      right $ Core.fieldWithTerm (var "field") (var "t")) $
    "forPair" <~ ("kv" ~>
      "k" <<~ var "recurse" @@ (Pairs.first $ var "kv") $
      "v" <<~ var "recurse" @@ (Pairs.second $ var "kv") $
      right $ pair (var "k") (var "v")) $
    "mapBinding" <~ ("b" ~>
      "v" <<~ var "recurse" @@ (Core.bindingTerm $ var "b") $
      right $ Core.binding (Core.bindingName $ var "b") (var "v") (Core.bindingType $ var "b")) $
    cases _Term (var "term") Nothing [
      _Term_annotated>>: "at" ~>
        "ex" <<~ var "recurse" @@ Core.annotatedTermBody (var "at") $
        right $ Core.termAnnotated $ Core.annotatedTerm (var "ex") (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "app" ~>
        "lhs" <<~ var "recurse" @@ Core.applicationFunction (var "app") $
        "rhs" <<~ var "recurse" @@ Core.applicationArgument (var "app") $
        right $ Core.termApplication $ Core.application (var "lhs") (var "rhs"),
      _Term_cases>>: "cs" ~>
        "n" <~ Core.caseStatementTypeName (var "cs") $
        "def" <~ Core.caseStatementDefault (var "cs") $
        "csCases" <~ Core.caseStatementCases (var "cs") $
        "rdef" <<~ Maybes.maybe (right nothing)
          ("t" ~> Eithers.map (unaryFunction just) $ var "recurse" @@ var "t")
          (var "def") $
        Eithers.map
          ("rcases" ~> Core.termCases $
            Core.caseStatement (var "n") (var "rdef") (var "rcases"))
          (Eithers.mapList (var "forField") (var "csCases")),
      _Term_either>>: "e" ~>
        "re" <<~ Eithers.either_
          ("l" ~> Eithers.map (unaryFunction left) $ var "recurse" @@ var "l")
          ("r" ~> Eithers.map (unaryFunction right) $ var "recurse" @@ var "r")
          (var "e") $
        right $ Core.termEither $ var "re",
      _Term_lambda>>: "l" ~>
        "v" <~ Core.lambdaParameter (var "l") $
        "d" <~ Core.lambdaDomain (var "l") $
        "body" <~ Core.lambdaBody (var "l") $
        "rbody" <<~ var "recurse" @@ var "body" $
        right $ Core.termLambda $ Core.lambda (var "v") (var "d") (var "rbody"),
      _Term_let>>: "lt" ~>
        "bindings" <~ Core.letBindings (var "lt") $
        "body" <~ Core.letBody (var "lt") $
        "rbindings" <<~ Eithers.mapList (var "mapBinding") (var "bindings") $
        "rbody" <<~ var "recurse" @@ var "body" $
        right $ Core.termLet $ Core.let_ (var "rbindings") (var "rbody"),
      _Term_list>>: "els" ~>
        "rels" <<~ Eithers.mapList (var "recurse") (var "els") $
        right $ Core.termList $ var "rels",
      _Term_literal>>: "v" ~> right $ Core.termLiteral $ var "v",
      _Term_map>>: "m" ~>
        "pairs" <<~ Eithers.mapList (var "forPair") (Maps.toList $ var "m") $
        right $ Core.termMap $ Maps.fromList $ var "pairs",
      _Term_maybe>>: "m" ~>
        "rm" <<~ Eithers.mapMaybe (var "recurse") (var "m") $
        right $ Core.termMaybe $ var "rm",
      _Term_pair>>: "p" ~>
        "rfirst" <<~ var "recurse" @@ Pairs.first (var "p") $
        "rsecond" <<~ var "recurse" @@ Pairs.second (var "p") $
        right $ Core.termPair $ pair (var "rfirst") (var "rsecond"),
      _Term_project>>: "p" ~> right $ Core.termProject $ var "p",
      _Term_record>>: "r" ~>
        "n" <~ Core.recordTypeName (var "r") $
        "fields" <~ Core.recordFields (var "r") $
        Eithers.map
          ("rfields" ~> Core.termRecord $ Core.record (var "n") (var "rfields"))
          (Eithers.mapList (var "forField") (var "fields")),
      _Term_set>>: "s" ~>
        "rlist" <<~ Eithers.mapList (var "recurse") (Sets.toList $ var "s") $
        right $ Core.termSet $ Sets.fromList $ var "rlist",
      _Term_typeApplication>>: "tt" ~>
        "t" <<~ var "recurse" @@ Core.typeApplicationTermBody (var "tt") $
        right $ Core.termTypeApplication $ Core.typeApplicationTerm (var "t") (Core.typeApplicationTermType (var "tt")),
      _Term_typeLambda>>: "tl" ~>
        "v" <~ Core.typeLambdaParameter (var "tl") $
        "body" <~ Core.typeLambdaBody (var "tl") $
        "rbody" <<~ var "recurse" @@ var "body" $
        right $ Core.termTypeLambda $ Core.typeLambda (var "v") (var "rbody"),
      _Term_union>>: "i" ~>
        "n" <~ Core.injectionTypeName (var "i") $
        "field" <~ Core.injectionField (var "i") $
        Eithers.map
          ("rfield" ~> Core.termUnion $ Core.injection (var "n") (var "rfield"))
          (var "forField" @@ var "field"),
      _Term_unit>>: constant $ right Core.termUnit,
      _Term_unwrap>>: "n" ~> right $ Core.termUnwrap $ var "n",
      _Term_variable>>: "v" ~> right $ Core.termVariable $ var "v",
      _Term_wrap>>: "wt" ~>
        "name" <~ Core.wrappedTermTypeName (var "wt") $
        "t" <~ Core.wrappedTermBody (var "wt") $
        "rt" <<~ var "recurse" @@ var "t" $
        right $ Core.termWrap $ Core.wrappedTerm (var "name") (var "rt")]) $

  "rewrite" <~ ("cx" ~> "term" ~> var "f" @@ (var "forSubterms" @@ var "rewrite") @@ var "cx" @@ var "term") $
  var "rewrite" @@ var "cx0" @@ var "term0"

rewriteType :: TTermDefinition (((Type -> Type) -> Type -> Type) -> Type -> Type)
rewriteType = define "rewriteType" $ "f" ~> "typ0" ~>
  "fsub" <~ ("recurse" ~> "typ" ~>
    "forField" <~ ("field" ~> Core.fieldTypeWithType (var "field") (var "recurse" @@ (Core.fieldTypeType $ var "field"))) $
    cases _Type (var "typ") Nothing [
      _Type_annotated>>: "at" ~> Core.typeAnnotated $ Core.annotatedType
        (var "recurse" @@ (Core.annotatedTypeBody $ var "at"))
        (Core.annotatedTypeAnnotation $ var "at"),
      _Type_application>>: "app" ~> Core.typeApplication $ Core.applicationType
        (var "recurse" @@ (Core.applicationTypeFunction $ var "app"))
        (var "recurse" @@ (Core.applicationTypeArgument $ var "app")),
      _Type_either>>: "et" ~> Core.typeEither $ Core.eitherType
        (var "recurse" @@ (Core.eitherTypeLeft $ var "et"))
        (var "recurse" @@ (Core.eitherTypeRight $ var "et")),
      _Type_pair>>: "pt" ~> Core.typePair $ Core.pairType
        (var "recurse" @@ (Core.pairTypeFirst $ var "pt"))
        (var "recurse" @@ (Core.pairTypeSecond $ var "pt")),
      _Type_function>>: "fun" ~> Core.typeFunction $ Core.functionType
        (var "recurse" @@ (Core.functionTypeDomain $ var "fun"))
        (var "recurse" @@ (Core.functionTypeCodomain $ var "fun")),
      _Type_forall>>: "lt" ~> Core.typeForall $ Core.forallType
        (Core.forallTypeParameter $ var "lt")
        (var "recurse" @@ (Core.forallTypeBody $ var "lt")),
      _Type_list>>: "t" ~> Core.typeList $ var "recurse" @@ var "t",
      _Type_literal>>: "lt" ~> Core.typeLiteral $ var "lt",
      _Type_map>>: "mt" ~> Core.typeMap $ Core.mapType
        (var "recurse" @@ (Core.mapTypeKeys $ var "mt"))
        (var "recurse" @@ (Core.mapTypeValues $ var "mt")),
      _Type_maybe>>: "t" ~> Core.typeMaybe $ var "recurse" @@ var "t",
      _Type_record>>: "rt" ~> Core.typeRecord $
        Lists.map (var "forField") (var "rt"),
      _Type_set>>: "t" ~> Core.typeSet $ var "recurse" @@ var "t",
      _Type_union>>: "rt" ~> Core.typeUnion $
        Lists.map (var "forField") (var "rt"),
      _Type_unit>>: constant Core.typeUnit,
      _Type_variable>>: "v" ~> Core.typeVariable $ var "v",
      _Type_void>>: constant Core.typeVoid,
      _Type_wrap>>: "wt" ~> Core.typeWrap $
        var "recurse" @@ var "wt"]) $
--  rewrite @@ var "fsub" @@ var "f" -- TODO: restore global rewrite/fix instead of the local definition
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "typ0"

rewriteTypeM :: TTermDefinition (((Type -> Prelude.Either e Type) -> Type -> Prelude.Either e Type) -> Type -> Prelude.Either e Type)
rewriteTypeM = define "rewriteTypeM" $
  doc "Either-based type rewriting" $
  "f" ~> "typ0" ~>
  "fsub" <~ ("recurse" ~> "typ" ~> cases _Type (var "typ") Nothing [
    _Type_annotated>>: "at" ~>
      "t" <<~ var "recurse" @@ (Core.annotatedTypeBody $ var "at") $
      right $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTypeAnnotation $ var "at"),
    _Type_application>>: "at" ~>
      "lhs" <<~ var "recurse" @@ (Core.applicationTypeFunction $ var "at") $
      "rhs" <<~ var "recurse" @@ (Core.applicationTypeArgument $ var "at") $
      right $ Core.typeApplication $ Core.applicationType (var "lhs") (var "rhs"),
    _Type_either>>: "et" ~>
      "left" <<~ var "recurse" @@ (Core.eitherTypeLeft $ var "et") $
      "right" <<~ var "recurse" @@ (Core.eitherTypeRight $ var "et") $
      right $ Core.typeEither $ Core.eitherType (var "left") (var "right"),
    _Type_pair>>: "pt" ~>
      "pairFirst" <<~ var "recurse" @@ (Core.pairTypeFirst $ var "pt") $
      "pairSecond" <<~ var "recurse" @@ (Core.pairTypeSecond $ var "pt") $
      right $ Core.typePair $ Core.pairType (var "pairFirst") (var "pairSecond"),
    _Type_function>>: "ft" ~>
      "dom" <<~ var "recurse" @@ (Core.functionTypeDomain $ var "ft") $
      "cod" <<~ var "recurse" @@ (Core.functionTypeCodomain $ var "ft") $
      right $ Core.typeFunction $ Core.functionType (var "dom") (var "cod"),
    _Type_forall>>: "ft" ~>
      "b" <<~ var "recurse" @@ (Core.forallTypeBody $ var "ft") $
      right $ Core.typeForall $ Core.forallType (Core.forallTypeParameter $ var "ft") (var "b"),
    _Type_list>>: "t" ~>
      "rt" <<~ var "recurse" @@ var "t" $
      right $ Core.typeList $ var "rt",
    _Type_literal>>: "lt" ~> right $ Core.typeLiteral $ var "lt",
    _Type_map>>: "mt" ~>
      "kt" <<~ var "recurse" @@ (Core.mapTypeKeys $ var "mt") $
      "vt" <<~ var "recurse" @@ (Core.mapTypeValues $ var "mt") $
      right $ Core.typeMap $ Core.mapType (var "kt") (var "vt"),
    _Type_maybe>>: "t" ~>
      "rt" <<~ var "recurse" @@ var "t" $
      right $ Core.typeMaybe $ var "rt",
    _Type_record>>: "rt" ~>
      "forField" <~ ("f" ~>
        "t" <<~ var "recurse" @@ (Core.fieldTypeType $ var "f") $
        right $ Core.fieldTypeWithType (var "f") (var "t")) $
      "rfields" <<~ Eithers.mapList (var "forField") (var "rt") $
      right $ Core.typeRecord $ var "rfields",
    _Type_set>>: "t" ~>
      "rt" <<~ var "recurse" @@ var "t" $
      right $ Core.typeSet $ var "rt",
    _Type_union>>: "rt" ~>
      "forField" <~ ("f" ~>
        "t" <<~ var "recurse" @@ (Core.fieldTypeType $ var "f") $
        right $ Core.fieldTypeWithType (var "f") (var "t")) $
      "rfields" <<~ Eithers.mapList (var "forField") (var "rt") $
      right $ Core.typeUnion $ var "rfields",
    _Type_unit>>: constant $ right $ Core.typeUnit,
    _Type_variable>>: "v" ~> right $ Core.typeVariable $ var "v",
    _Type_void>>: constant $ right $ Core.typeVoid,
    _Type_wrap>>: "wt" ~>
      "t" <<~ var "recurse" @@ var "wt" $
      right $ Core.typeWrap $ var "t"]) $
  "recurse" <~ var "f" @@ (var "fsub" @@ var "recurse") $
  var "recurse" @@ var "typ0"

subterms :: TTermDefinition (Term -> [Term])
subterms = define "subterms" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: "at" ~> list [Core.annotatedTermBody $ var "at"],
    _Term_application>>: "p" ~> list [
      Core.applicationFunction $ var "p",
      Core.applicationArgument $ var "p"],
    _Term_cases>>: "cs" ~> Lists.concat2
      (Maybes.maybe (list ([] :: [TTerm Term])) ("t" ~> list [var "t"]) (Core.caseStatementDefault $ var "cs"))
      (Lists.map (unaryFunction Core.fieldTerm) (Core.caseStatementCases $ var "cs")),
    _Term_either>>: "e" ~> Eithers.either_
      ("l" ~> list [var "l"])
      ("r" ~> list [var "r"])
      (var "e"),
    _Term_lambda>>: "l" ~> list [Core.lambdaBody $ var "l"],
    _Term_let>>: "lt" ~> Lists.cons
      (Core.letBody $ var "lt")
      (Lists.map (unaryFunction Core.bindingTerm) (Core.letBindings $ var "lt")),
    _Term_list>>: "l" ~> var "l",
    _Term_literal>>: constant $ list ([] :: [TTerm Term]),
    _Term_map>>: "m" ~> Lists.concat $ Lists.map
      ("p" ~> list [Pairs.first $ var "p", Pairs.second $ var "p"])
      (Maps.toList $ var "m"),
    _Term_maybe>>: "m" ~> Maybes.maybe (list ([] :: [TTerm Term])) ("t" ~> list [var "t"]) (var "m"),
    _Term_pair>>: "p" ~> list [Pairs.first $ var "p", Pairs.second $ var "p"],
    _Term_project>>: constant $ list ([] :: [TTerm Term]),
    _Term_record>>: "rt" ~> Lists.map (unaryFunction Core.fieldTerm) (Core.recordFields $ var "rt"),
    _Term_set>>: "l" ~> Sets.toList $ var "l",
    _Term_typeApplication>>: "ta" ~> list [Core.typeApplicationTermBody $ var "ta"],
    _Term_typeLambda>>: "ta" ~> list [Core.typeLambdaBody $ var "ta"],
    _Term_union>>: "ut" ~> list [Core.fieldTerm $ (Core.injectionField $ var "ut")],
    _Term_unit>>: constant $ list ([] :: [TTerm Term]),
    _Term_unwrap>>: constant $ list ([] :: [TTerm Term]),
    _Term_variable>>: constant $ list ([] :: [TTerm Term]),
    _Term_wrap>>: "n" ~> list [Core.wrappedTermBody $ var "n"]]

subtermsWithSteps :: TTermDefinition (Term -> [(SubtermStep, Term)])
subtermsWithSteps = define "subtermsWithSteps" $
  doc "Find the children of a given term" $
  match _Term Nothing [
    _Term_annotated>>: "at" ~> single Paths.subtermStepAnnotatedBody $ Core.annotatedTermBody $ var "at",
    _Term_application>>: "p" ~> list [
      result Paths.subtermStepApplicationFunction $ Core.applicationFunction $ var "p",
      result Paths.subtermStepApplicationArgument $ Core.applicationArgument $ var "p"],
    _Term_cases>>: "cs" ~> Lists.concat2
      (Maybes.maybe none
        ("t" ~> single Paths.subtermStepUnionCasesDefault $ var "t")
        (Core.caseStatementDefault $ var "cs"))
      (Lists.map
        ("f" ~> result (Paths.subtermStepUnionCasesBranch $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
        (Core.caseStatementCases $ var "cs")),
    _Term_either>>: "e" ~> none, -- TODO: add steps when SubtermStep type is updated
    _Term_lambda>>: "l" ~> single Paths.subtermStepLambdaBody $ Core.lambdaBody $ var "l",
    _Term_let>>: "lt" ~> Lists.cons
      (result Paths.subtermStepLetBody $ Core.letBody $ var "lt")
      (Lists.map
        ("b" ~> result (Paths.subtermStepLetBinding $ Core.bindingName $ var "b") $ Core.bindingTerm $ var "b")
        (Core.letBindings $ var "lt")),
    _Term_list>>: "l" ~> Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      ("e" ~> result (Paths.subtermStepListElement $ int32 0) $ var "e")
      (var "l"),
    _Term_literal>>: constant none,
    _Term_map>>: "m" ~> Lists.concat
      (Lists.map
        ("p" ~> list [
          -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
          result (Paths.subtermStepMapKey $ int32 0) $ Pairs.first $ var "p",
          result (Paths.subtermStepMapValue $ int32 0) $ Pairs.second $ var "p"])
        (Maps.toList $ var "m")),
    _Term_maybe>>: "m" ~> Maybes.maybe none
      ("t" ~> single Paths.subtermStepMaybeTerm $ var "t")
      (var "m"),
    _Term_pair>>: "p" ~> none, -- TODO: add steps when SubtermStep type is updated
    _Term_project>>: constant none,
    _Term_record>>: "rt" ~> Lists.map
      ("f" ~> result (Paths.subtermStepRecordField $ Core.fieldName $ var "f") $ Core.fieldTerm $ var "f")
      (Core.recordFields $ var "rt"),
    _Term_set>>: "s" ~> Lists.map
      -- TODO: use a range of indexes from 0 to len(l)-1, rather than just 0
      ("e" ~> result (Paths.subtermStepListElement $ int32 0) $ var "e")
      (Sets.toList $ var "s"),
    _Term_typeApplication>>: "ta" ~>
      single Paths.subtermStepTypeApplicationTerm $
      Core.typeApplicationTermBody $ var "ta",
    _Term_typeLambda>>: "ta" ~>
      single Paths.subtermStepTypeLambdaBody $
      Core.typeLambdaBody $ var "ta",
    _Term_union>>: "ut" ~>
      single Paths.subtermStepInjectionTerm $
      Core.fieldTerm $ (Core.injectionField $ var "ut"),
    _Term_unit>>: constant none,
    _Term_unwrap>>: constant none,
    _Term_variable>>: constant none,
    _Term_wrap>>: "n" ~> single Paths.subtermStepWrappedTerm $ Core.wrappedTermBody $ var "n"]
  where
    none = list ([] :: [TTerm (SubtermStep, Term)])
    single step term = list [result step term]
    result step term = pair step term

subtypes :: TTermDefinition (Type -> [Type])
subtypes = define "subtypes" $
  doc "Find the children of a given type expression" $
  match _Type Nothing [
    _Type_annotated>>: "at" ~> list [Core.annotatedTypeBody $ var "at"],
    _Type_application>>: "at" ~> list [
      Core.applicationTypeFunction $ var "at",
      Core.applicationTypeArgument $ var "at"],
    _Type_either>>: "et" ~> list [
      Core.eitherTypeLeft $ var "et",
      Core.eitherTypeRight $ var "et"],
    _Type_pair>>: "pt" ~> list [
      Core.pairTypeFirst $ var "pt",
      Core.pairTypeSecond $ var "pt"],
    _Type_function>>: "ft" ~> list [
      Core.functionTypeDomain $ var "ft",
      Core.functionTypeCodomain $ var "ft"],
    _Type_forall>>: "lt" ~> list [Core.forallTypeBody $ var "lt"],
    _Type_list>>: "lt" ~> list [var "lt"],
    _Type_literal>>: constant $ list ([] :: [TTerm Type]),
    _Type_map>>: "mt" ~> list [
      Core.mapTypeKeys $ var "mt",
      Core.mapTypeValues $ var "mt"],
    _Type_maybe>>: "ot" ~> list [var "ot"],
    _Type_record>>: "rt" ~> Lists.map (unaryFunction Core.fieldTypeType) (var "rt"),
    _Type_set>>: "st" ~> list [var "st"],
    _Type_union>>: "rt" ~> Lists.map (unaryFunction Core.fieldTypeType) (var "rt"),
    _Type_unit>>: constant $ list ([] :: [TTerm Type]),
    _Type_variable>>: constant $ list ([] :: [TTerm Type]),
    _Type_void>>: constant $ list ([] :: [TTerm Type]),
    _Type_wrap>>: "nt" ~> list [var "nt"]]

rewriteAndFoldTermWithGraph :: TTermDefinition (((a -> Term -> (a, Term)) -> Graph -> a -> Term -> (a, Term)) -> Graph -> a -> Term -> (a, Term))
rewriteAndFoldTermWithGraph = define "rewriteAndFoldTermWithGraph" $
  doc ("Rewrite a term while folding to produce a value, with Graph updated as we descend into subterms."
    <> " Combines the features of rewriteAndFoldTerm and rewriteTermWithGraph."
    <> " The user function f receives a recurse function that handles subterm traversal and Graph management.") $
  "f" ~> "cx0" ~> "val0" ~> "term0" ~>
  "wrapper" <~ ("lowLevelRecurse" ~> "valAndCx" ~> "term" ~>
    "val" <~ Pairs.first (var "valAndCx") $
    "cx" <~ Pairs.second (var "valAndCx") $
    "cx1" <~ (cases _Term (var "term")
      (Just $ var "cx") [
      _Term_lambda>>: "l" ~> Scoping.extendGraphForLambda @@ var "cx" @@ var "l",
      _Term_let>>: "l" ~> Scoping.extendGraphForLet @@ constant (constant nothing) @@ var "cx" @@ var "l",
      _Term_typeLambda>>: "tl" ~> Scoping.extendGraphForTypeLambda @@ var "cx" @@ var "tl"]) $
    "recurseForUser" <~ ("newVal" ~> "subterm" ~>
      "result" <~ var "lowLevelRecurse" @@ pair (var "newVal") (var "cx1") @@ var "subterm" $
      pair (Pairs.first $ Pairs.first $ var "result") (Pairs.second $ var "result")) $
    "fResult" <~ var "f" @@ var "recurseForUser" @@ var "cx1" @@ var "val" @@ var "term" $
    pair (pair (Pairs.first $ var "fResult") (var "cx")) (Pairs.second $ var "fResult")) $
  "result" <~ rewriteAndFoldTerm @@ var "wrapper" @@ pair (var "val0") (var "cx0") @@ var "term0" $
  pair (Pairs.first $ Pairs.first $ var "result") (Pairs.second $ var "result")

rewriteAndFoldTermWithGraphAndPath :: TTermDefinition (
  ((a -> Term -> (a, Term)) -> [SubtermStep] -> Graph -> a -> Term -> (a, Term))
  -> Graph -> a -> Term -> (a, Term))
rewriteAndFoldTermWithGraphAndPath = define "rewriteAndFoldTermWithGraphAndPath" $
  doc ("Rewrite a term while folding to produce a value, with both Graph and accessor path tracked."
    <> " The path is a list of SubtermSteps representing the position from the root to the current term."
    <> " Combines the features of rewriteAndFoldTermWithPath and Graph tracking."
    <> " The Graph is automatically updated when descending into lambdas, lets, and type lambdas.") $
  "f" ~> "cx0" ~> "val0" ~> "term0" ~>
  "wrapper" <~ ("recurse" ~> "path" ~> "cxAndVal" ~> "term" ~>
    "cx" <~ Pairs.first (var "cxAndVal") $
    "val" <~ Pairs.second (var "cxAndVal") $
    "cx1" <~ (cases _Term (var "term")
      (Just $ var "cx") [
      _Term_lambda>>: "l" ~> Scoping.extendGraphForLambda @@ var "cx" @@ var "l",
      _Term_let>>: "l" ~> Scoping.extendGraphForLet @@ constant (constant nothing) @@ var "cx" @@ var "l",
      _Term_typeLambda>>: "tl" ~> Scoping.extendGraphForTypeLambda @@ var "cx" @@ var "tl"]) $
    "recurseForUser" <~ ("valIn" ~> "termIn" ~>
      "result" <~ var "recurse" @@ var "path" @@ pair (var "cx1") (var "valIn") @@ var "termIn" $
      pair (Pairs.second $ Pairs.first $ var "result") (Pairs.second $ var "result")) $
    "fResult" <~ var "f" @@ var "recurseForUser" @@ var "path" @@ var "cx1" @@ var "val" @@ var "term" $
    pair (pair (var "cx") (Pairs.first $ var "fResult")) (Pairs.second $ var "fResult")) $
  "result" <~ rewriteAndFoldTermWithPath @@ var "wrapper" @@ pair (var "cx0") (var "val0") @@ var "term0" $
  pair (Pairs.second $ Pairs.first $ var "result") (Pairs.second $ var "result")

rewriteTermWithGraph :: TTermDefinition (((Term -> Term) -> Graph -> Term -> Term) -> Graph -> Term -> Term)
rewriteTermWithGraph = define "rewriteTermWithGraph" $
  doc "Rewrite a term with the help of a Graph which is updated as we descend into subterms" $
  "f" ~> "cx0" ~> "term0" ~>
  "f2" <~ ("recurse" ~> "cx" ~> "term" ~>
    "recurse1" <~ ("term" ~> var "recurse" @@ var "cx" @@ var "term") $
    cases _Term (var "term") (Just $ var "f" @@ var "recurse1" @@ var "cx" @@ var "term") [
      _Term_lambda>>: "l" ~>
        "cx1" <~ Scoping.extendGraphForLambda @@ var "cx" @@ var "l" $
        "recurse2" <~ ("term" ~> var "recurse" @@ var "cx1" @@ var "term") $
        var "f" @@ var "recurse2" @@ var "cx1" @@ var "term",
      _Term_let>>: "l" ~>
        "cx1" <~ Scoping.extendGraphForLet @@ constant (constant nothing) @@ var "cx" @@ var "l" $
        "recurse2" <~ ("term" ~> var "recurse" @@ var "cx1" @@ var "term") $
        var "f" @@ var "recurse2" @@ var "cx1" @@ var "term",
      _Term_typeLambda>>: "tl" ~>
        "cx1" <~ Scoping.extendGraphForTypeLambda @@ var "cx" @@ var "tl" $
        "recurse2" <~ ("term" ~> var "recurse" @@ var "cx1" @@ var "term") $
        var "f" @@ var "recurse2" @@ var "cx1" @@ var "term"]) $
  "rewrite" <~ ("cx" ~> "term" ~> var "f2" @@ (var "rewrite") @@ var "cx" @@ var "term") $
  var "rewrite" @@ var "cx0" @@ var "term0"

foldTermWithGraphAndPath :: TTermDefinition (
  ((a -> Term -> a) -> [SubtermStep] -> Graph -> a -> Term -> a)
  -> Graph -> a -> Term -> a)
foldTermWithGraphAndPath = define "foldTermWithGraphAndPath" $
  doc ("Fold over a term to produce a value, with both Graph and accessor path tracked."
    <> " Like rewriteAndFoldTermWithGraphAndPath, but only folds without rewriting."
    <> " The Graph is automatically updated when descending into lambdas, lets, and type lambdas.") $
  "f" ~> "cx0" ~> "val0" ~> "term0" ~>
  -- Wrap the user's fold function to also return the original term unchanged
  "wrapper" <~ ("recurse" ~> "path" ~> "cx" ~> "val" ~> "term" ~>
    "recurseForUser" <~ ("valIn" ~> "subterm" ~>
      "r" <~ var "recurse" @@ var "valIn" @@ var "subterm" $
      Pairs.first $ var "r") $
    pair (var "f" @@ var "recurseForUser" @@ var "path" @@ var "cx" @@ var "val" @@ var "term") (var "term")) $
  "result" <~ rewriteAndFoldTermWithGraphAndPath @@ var "wrapper" @@ var "cx0" @@ var "val0" @@ var "term0" $
  Pairs.first $ var "result"
