-- | Test cases for hydra.annotations functions
module Hydra.Sources.Test.Annotations where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
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



-- | Test state (an empty Graph)
testState :: TTerm Term
testState = metaref Lexical.emptyGraph

-- | Empty Context for Either-based function tests
testContext :: TTerm Term
testContext = metaref Lexical.emptyContext



ns :: Namespace
ns = Namespace "hydra.test.annotations"

module_ :: Module
module_ = Module ns definitions [Annotations.ns, Lexical.ns, Namespace "hydra.reduction", Namespace "hydra.show.core"] [] $
    Just "Test cases for hydra.annotations functions"
  where
    definitions = [Phantoms.toDefinition allTests]


-- | Annotation eval case: like annEvalCase but tagged as disabled because these tests
-- require kernel term bindings in the test graph (via reduceTerm), which not all
-- implementations provide yet. See hydra.test.environment in the branch plan.
annEvalCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
annEvalCase name = evalCase name

-- | Test cases for getTermAnnotation and setTermAnnotation
arbitraryAnnotationTests :: TTerm TestGroup
arbitraryAnnotationTests = subgroup "arbitrary annotations" [
  -- Set a single key/value pair (multiple cases for property test coverage)
  -- Note: These tests require interpretation because setTermAnnotation uses maps.alter which has no interpreter impl
  annEvalCase "set single annotation #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ int32Term 42) @@ stringTerm "foo")
    (annotatedTerm (stringTerm "foo") $ Terms.map $ Maps.singleton (nameTerm "k1") (int32Term 42)),
  annEvalCase "set single annotation #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "myKey" @@ (optional $ just $ int32Term (-17)) @@ stringTerm "bar")
    (annotatedTerm (stringTerm "bar") $ Terms.map $ Maps.singleton (nameTerm "myKey") (int32Term (-17))),
  annEvalCase "set single annotation #3"
    (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ stringTerm "hello") @@ int32Term 0)
    (annotatedTerm (int32Term 0) $ Terms.map $ Maps.singleton (nameTerm "x") (stringTerm "hello")),

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
    (annotatedTerm (booleanTerm True) $ Terms.map $ Maps.fromList $ Phantoms.list [
      Phantoms.pair (nameTerm "k1") (stringTerm "first"),
      Phantoms.pair (nameTerm "k2") (int32Term 200)]),
  annEvalCase "set multiple annotations #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "b" @@ (optional $ just $ int32Term 0)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "a" @@ (optional $ just $ int32Term (-5)) @@ stringTerm "test"))
    (annotatedTerm (stringTerm "test") $ Terms.map $ Maps.fromList $ Phantoms.list [
      Phantoms.pair (nameTerm "a") (int32Term (-5)),
      Phantoms.pair (nameTerm "b") (int32Term 0)]),

  -- An outer annotation overrides an inner one (multiple cases)
  annEvalCase "outer annotation overrides inner #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "outer")
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "inner") @@ stringTerm "bar"))
    (annotatedTerm (stringTerm "bar") $ Terms.map $ Maps.singleton (nameTerm "k1") (stringTerm "outer")),
  annEvalCase "outer annotation overrides inner #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ stringTerm "new")
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ stringTerm "old") @@ int32Term 42))
    (annotatedTerm (int32Term 42) $ Terms.map $ Maps.singleton (nameTerm "x") (stringTerm "new")),
  annEvalCase "outer annotation overrides inner #3"
    (metaref Annotations.setTermAnnotation @@ nameTerm "key" @@ (optional $ just $ int32Term 999)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "key" @@ (optional $ just $ int32Term 1) @@ booleanTerm False))
    (annotatedTerm (booleanTerm False) $ Terms.map $ Maps.singleton (nameTerm "key") (int32Term 999)),

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
    (annotatedTerm (int64Term 137) $ Terms.map $ Maps.singleton (nameTerm "k2") (int32Term 200)),
  annEvalCase "unset one of multiple annotations #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "b" @@ (optional nothing)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "b" @@ (optional $ just $ int32Term 2)
        @@ (metaref Annotations.setTermAnnotation @@ nameTerm "a" @@ (optional $ just $ int32Term 1) @@ stringTerm "x")))
    (annotatedTerm (stringTerm "x") $ Terms.map $ Maps.singleton (nameTerm "a") (int32Term 1))]

-- | Test cases for getTermDescription and setTermDescription
descriptionTests :: TTerm TestGroup
descriptionTests = subgroup "descriptions" [
  -- Set a single description (multiple cases)
  -- Note: These tests require interpretation because setTermDescription uses maps.alter which has no interpreter impl
  annEvalCase "set description #1"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "my description") @@ stringTerm "foo")
    (annotatedTerm (stringTerm "foo") $ Terms.map $ Maps.singleton (nameTerm "description") (stringTerm "my description")),
  annEvalCase "set description #2"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "") @@ int32Term 42)
    (annotatedTerm (int32Term 42) $ Terms.map $ Maps.singleton (nameTerm "description") (stringTerm "")),
  annEvalCase "set description #3"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "A longer description with spaces") @@ booleanTerm True)
    (annotatedTerm (booleanTerm True) $ Terms.map $ Maps.singleton (nameTerm "description") (stringTerm "A longer description with spaces")),

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
    (annotatedTerm (stringTerm "bar") $ Terms.map $ Maps.singleton (nameTerm "description") (stringTerm "outer")),
  annEvalCase "outer description overrides inner #2"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "new")
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "old") @@ int32Term 99))
    (annotatedTerm (int32Term 99) $ Terms.map $ Maps.singleton (nameTerm "description") (stringTerm "new")),

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
layeredAnnotationTests :: TTerm TestGroup
layeredAnnotationTests = subgroup "layered annotations" [
  -- Annotations at different levels, with different keys, are all available
  annEvalCase "get annotation from unannotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "one" @@ int32Term 42)
    (optional nothing),

  annEvalCase "get annotation from singly annotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "one"
      @@ (annotatedTerm (int32Term 42) $ Terms.map $ Maps.singleton (nameTerm "one") (int32Term 1)))
    (optional $ just $ int32Term 1),

  annEvalCase "get inner annotation from doubly annotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "one"
      @@ (annotatedTerm
           (annotatedTerm (int32Term 42) $ Terms.map $ Maps.singleton (nameTerm "one") (int32Term 1))
           $ Terms.map $ Maps.singleton (nameTerm "two") (int32Term 2)))
    (optional $ just $ int32Term 1),

  annEvalCase "get outer annotation from doubly annotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "two"
      @@ (annotatedTerm
           (annotatedTerm (int32Term 42) $ Terms.map $ Maps.singleton (nameTerm "one") (int32Term 1))
           $ Terms.map $ Maps.singleton (nameTerm "two") (int32Term 2)))
    (optional $ just $ int32Term 2),

  -- Non-overridden annotation still accessible in triply annotated term
  annEvalCase "get non-overridden annotation from triply annotated term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "two"
      @@ (annotatedTerm
           (annotatedTerm
             (annotatedTerm (int32Term 42) $ Terms.map $ Maps.singleton (nameTerm "one") (int32Term 1))
             $ Terms.map $ Maps.singleton (nameTerm "two") (int32Term 2))
           $ Terms.map $ Maps.singleton (nameTerm "one") (int32Term 99)))
    (optional $ just $ int32Term 2),

  -- Outer annotations override inner ones
  annEvalCase "outer annotation overrides inner in layered term"
    (metaref Annotations.getTermAnnotation @@ nameTerm "one"
      @@ (annotatedTerm
           (annotatedTerm
             (annotatedTerm (int32Term 42) $ Terms.map $ Maps.singleton (nameTerm "one") (int32Term 1))
             $ Terms.map $ Maps.singleton (nameTerm "two") (int32Term 2))
           $ Terms.map $ Maps.singleton (nameTerm "one") (int32Term 99)))
    (optional $ just $ int32Term 99)]

allTests :: TTermDefinition TestGroup
allTests = Phantoms.definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.annotations functions" $
    supergroup "annotations" [
      arbitraryAnnotationTests,
      descriptionTests,
      layeredAnnotationTests]
