-- | Test cases for hydra.annotations functions
module Hydra.Sources.Test.Annotations where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Maps as Maps
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Dsl.Meta.Graph as Graph
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical



ns :: ModuleName
ns = ModuleName "hydra.test.annotations"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [Annotations.ns, Lexical.ns, ModuleName "hydra.reduction", ModuleName "hydra.show.core", ModuleName "hydra.core", ModuleName "hydra.errors", ModuleName "hydra.test.testGraph", ModuleName "hydra.testing"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.annotations functions")}
  where
    definitions = [Phantoms.toDefinition allTests]


allTests :: TypedTermDefinition TestGroup
allTests = Phantoms.definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.annotations functions" $
    supergroup "annotations" [
      arbitraryAnnotationTests,
      descriptionTests,
      layeredAnnotationTests]

-- | Annotation eval case: like annEvalCase but tagged as disabled because these tests
-- require kernel term bindings in the test graph (via reduceTerm), which not all
-- implementations provide yet. See hydra.test.environment in the branch plan.
annEvalCase :: String -> TypedTerm Term -> TypedTerm Term -> TypedTerm TestCaseWithMetadata
annEvalCase name = evalCase name

-- | Build the test-side 'expected' / 'input' annotated-term: takes a
-- term-encoded body and a list of (key, term-encoded value) annotation
-- entries, returns a `TypedTerm Term` whose post-pipeline shape matches
-- the LHS reducer's singly-encoded `inject(Term){annotated: TermRecord ...}`.
--
-- Built entirely from meta-DSL constructors (`annotatedTerm`, `mapTerm1`,
-- and a local doubly-encoded `varTerm`) so that each layer is
-- doubly-encoded; the pipeline's one-level decode strip lands every layer
-- at the singly-encoded form the LHS reducer produces.
annotatedExp :: TypedTerm Term -> [(String, TypedTerm Term)] -> TypedTerm Term
annotatedExp body kvs = annotatedTerm body (mapTerm1 mapEntries)
  where
    mapEntries = Maps.fromList $ Phantoms.list
      [ Phantoms.pair (varTerm k) v | (k, v) <- kvs ]
    -- Doubly-encoded `inject(Term){variable: wrap(Name){...}}` so that
    -- after the pipeline strip we get the singly-encoded variable shape
    -- the LHS reducer's annotation map uses for keys.
    varTerm k = inject (Core.nameLift _Term) "variable" (nameTerm k)

-- | Convenience for one-entry annotation maps.
annotatedExp1 :: TypedTerm Term -> String -> TypedTerm Term -> TypedTerm Term
annotatedExp1 body k v = annotatedExp body [(k, v)]

-- | Test cases for getTermAnnotation and setTermAnnotation
arbitraryAnnotationTests :: TypedTerm TestGroup
arbitraryAnnotationTests = subgroup "arbitrary annotations" [
  -- Set a single key/value pair (multiple cases for property test coverage)
  -- Note: These tests require interpretation because setTermAnnotation uses maps.alter which has no interpreter impl
  annEvalCase "set single annotation #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ int32Term 42) @@ stringTerm "foo")
    (annotatedExp1 (stringTerm "foo") "k1" (int32Term 42)),
  annEvalCase "set single annotation #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "myKey" @@ (optional $ just $ int32Term (-17)) @@ stringTerm "bar")
    (annotatedExp1 (stringTerm "bar") "myKey" (int32Term (-17))),
  annEvalCase "set single annotation #3"
    (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ stringTerm "hello") @@ int32Term 0)
    (annotatedExp1 (int32Term 0) "x" (stringTerm "hello")),

  -- Retrieve a single value (multiple cases)
  -- Note: These tests require the interpreter because getTermAnnotation uses pattern matching on Term
  annEvalCase "get existing annotation #1"
    (metaref Annotations.getTermAnnotation @@ nameTerm "k1"
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "value") @@ int32Term 42))
    (optional $ just $ stringTerm "value"),
  annEvalCase "get existing annotation #2"
    (metaref Annotations.getTermAnnotation @@ nameTerm "foo"
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "foo" @@ (optional $ just $ stringTerm "") @@ int32Term 99))
    (optional $ just $ stringTerm ""),
  annEvalCase "get existing annotation #3"
    (metaref Annotations.getTermAnnotation @@ nameTerm "key"
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "key" @@ (optional $ just $ int32Term 123) @@ stringTerm "test"))
    (optional $ just $ int32Term 123),

  -- Retrieve a null value (annotation not present) - multiple cases
  annEvalCase "get missing annotation #1"
    (metaref Annotations.getTermAnnotation @@ nameTerm "k1" @@ int16Term 42)
    (optional nothing),
  annEvalCase "get missing annotation #2"
    (metaref Annotations.getTermAnnotation @@ nameTerm "nonexistent" @@ stringTerm "hello")
    (optional nothing),
  annEvalCase "get missing annotation #3"
    (metaref Annotations.getTermAnnotation @@ nameTerm "k1"
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k2" @@ (optional $ just $ int32Term 1) @@ int32Term 42))
    (optional nothing),

  -- Set multiple values (multiple cases)
  annEvalCase "set multiple annotations #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k2" @@ (optional $ just $ int32Term 200)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "first") @@ booleanTerm True))
    (annotatedExp (booleanTerm True) [("k1", stringTerm "first"), ("k2", int32Term 200)]),
  annEvalCase "set multiple annotations #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "b" @@ (optional $ just $ int32Term 0)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "a" @@ (optional $ just $ int32Term (-5)) @@ stringTerm "test"))
    (annotatedExp (stringTerm "test") [("a", int32Term (-5)), ("b", int32Term 0)]),

  -- An outer annotation overrides an inner one (multiple cases)
  annEvalCase "outer annotation overrides inner #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "outer")
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "inner") @@ stringTerm "bar"))
    (annotatedExp1 (stringTerm "bar") "k1" (stringTerm "outer")),
  annEvalCase "outer annotation overrides inner #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ stringTerm "new")
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ stringTerm "old") @@ int32Term 42))
    (annotatedExp1 (int32Term 42) "x" (stringTerm "new")),
  annEvalCase "outer annotation overrides inner #3"
    (metaref Annotations.setTermAnnotation @@ nameTerm "key" @@ (optional $ just $ int32Term 999)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "key" @@ (optional $ just $ int32Term 1) @@ booleanTerm False))
    (annotatedExp1 (booleanTerm False) "key" (int32Term 999)),

  -- Unset a single annotation (multiple cases)
  annEvalCase "unset single annotation #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional nothing)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "foo") @@ int64Term 137))
    (int64Term 137),
  annEvalCase "unset single annotation #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional nothing)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ int32Term 42) @@ stringTerm "test"))
    (stringTerm "test"),

  -- Unset one of multiple annotations (multiple cases)
  annEvalCase "unset one of multiple annotations #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional nothing)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k2" @@ (optional $ just $ int32Term 200)
        @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "first") @@ int64Term 137)))
    (annotatedExp1 (int64Term 137) "k2" (int32Term 200)),
  annEvalCase "unset one of multiple annotations #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "b" @@ (optional nothing)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "b" @@ (optional $ just $ int32Term 2)
        @@ (metaref Annotations.setTermAnnotation @@ nameTerm "a" @@ (optional $ just $ int32Term 1) @@ stringTerm "x")))
    (annotatedExp1 (stringTerm "x") "a" (int32Term 1))]

-- | Test cases for getTermDescription and setTermDescription
descriptionTests :: TypedTerm TestGroup
descriptionTests = subgroup "descriptions" [
  -- Set a single description (multiple cases)
  -- Note: These tests require interpretation because setTermDescription uses maps.alter which has no interpreter impl
  annEvalCase "set description #1"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "my description") @@ stringTerm "foo")
    (annotatedExp1 (stringTerm "foo") "description" (stringTerm "my description")),
  annEvalCase "set description #2"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "") @@ int32Term 42)
    (annotatedExp1 (int32Term 42) "description" (stringTerm "")),
  annEvalCase "set description #3"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "A longer description with spaces") @@ booleanTerm True)
    (annotatedExp1 (booleanTerm True) "description" (stringTerm "A longer description with spaces")),

  -- Get existing description (returns Either Error (Maybe String))
  annEvalCase "get existing description #1"
    (metaref Annotations.getTermDescription @@ testContext @@ testState
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "hello") @@ int32Term 42))
    (right (optional $ just $ string "hello")),
  annEvalCase "get existing description #2"
    (metaref Annotations.getTermDescription @@ testContext @@ testState
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "") @@ stringTerm "test"))
    (right (optional $ just $ string "")),
  annEvalCase "get existing description #3"
    (metaref Annotations.getTermDescription @@ testContext @@ testState
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "desc") @@ booleanTerm False))
    (right (optional $ just $ string "desc")),

  -- Get missing description (no description annotation present)
  annEvalCase "get missing description #1"
    (metaref Annotations.getTermDescription @@ testContext @@ testState @@ int16Term 42)
    (right (optional nothing)),
  annEvalCase "get missing description #2"
    (metaref Annotations.getTermDescription @@ testContext @@ testState @@ stringTerm "no description here")
    (right (optional nothing)),
  annEvalCase "get missing description #3"
    (metaref Annotations.getTermDescription @@ testContext @@ testState @@ int32Term 0)
    (right (optional nothing)),

  -- An outer description overrides an inner one (multiple cases)
  annEvalCase "outer description overrides inner #1"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "outer")
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "inner") @@ stringTerm "bar"))
    (annotatedExp1 (stringTerm "bar") "description" (stringTerm "outer")),
  annEvalCase "outer description overrides inner #2"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "new")
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "old") @@ int32Term 99))
    (annotatedExp1 (int32Term 99) "description" (stringTerm "new")),

  -- Unset a description (multiple cases)
  annEvalCase "unset description #1"
    (metaref Annotations.setTermDescription @@ (optional nothing)
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "desc") @@ int64Term 137))
    (int64Term 137),
  annEvalCase "unset description #2"
    (metaref Annotations.setTermDescription @@ (optional nothing)
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "to be removed") @@ stringTerm "test"))
    (stringTerm "test")]

-- | Test cases for layered (non-compact) annotations
-- Note: These tests require interpretation because they call getTermAnnotation which is not a primitive
layeredAnnotationTests :: TypedTerm TestGroup
layeredAnnotationTests = subgroup "layered annotations" [
  -- Annotations at different levels, with different keys, are all available
  annEvalCase "get annotation from unannotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "one" @@ int32Term 42)
    (optional nothing),

  -- Inputs for these tests must reach the LHS reducer in the singly-encoded
  -- inject(Term){annotated: TermRecord ...} form so the kernel functions'
  -- `cases _Term` dispatchers can match the annotation/map variants. The
  -- typed `annotatedExp1` / `annotatedExp` builder produces exactly that
  -- shape after the pipeline's one-level decode strip.
  annEvalCase "get annotation from singly annotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "one"
      @@ annotatedExp1 (int32Term 42) "one" (int32Term 1))
    (optional $ just $ int32Term 1),

  annEvalCase "get inner annotation from doubly annotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "one"
      @@ annotatedExp1
           (annotatedExp1 (int32Term 42) "one" (int32Term 1))
           "two" (int32Term 2))
    (optional $ just $ int32Term 1),

  annEvalCase "get outer annotation from doubly annotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "two"
      @@ annotatedExp1
           (annotatedExp1 (int32Term 42) "one" (int32Term 1))
           "two" (int32Term 2))
    (optional $ just $ int32Term 2),

  -- Non-overridden annotation still accessible in triply annotated term
  annEvalCase "get non-overridden annotation from triply annotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "two"
      @@ annotatedExp1
           (annotatedExp1
             (annotatedExp1 (int32Term 42) "one" (int32Term 1))
             "two" (int32Term 2))
           "one" (int32Term 99))
    (optional $ just $ int32Term 2),

  -- Outer annotations override inner ones
  annEvalCase "outer annotation overrides inner in layered term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "one"
      @@ annotatedExp1
           (annotatedExp1
             (annotatedExp1 (int32Term 42) "one" (int32Term 1))
             "two" (int32Term 2))
           "one" (int32Term 99))
    (optional $ just $ int32Term 99)]

-- | Empty InferenceContext for Either-based function tests
testContext :: TypedTerm Term
testContext = metaref Lexical.emptyInferenceContext



-- | Test state (an empty Graph)
testState :: TypedTerm Term
testState = metaref Lexical.emptyGraph
