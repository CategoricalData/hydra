-- | Test cases for hydra.annotations functions
module Hydra.Sources.Test.Annotations where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
--import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Dsl.Meta.Lib.Maps as Maps
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads

import qualified Data.Map as M


-- | Test trace for Flow-based tests
testTrace :: TTerm Term
testTrace = metaref Monads.emptyTrace

-- | Test state for Flow-based tests (using Graph as the state type, since
-- functions like getTermDescription return Flow Graph ...)
-- Note: the type is TTerm Term because at the meta level, everything is a Term
testState :: TTerm Term
testState = metaref Lexical.emptyGraph


ns :: Namespace
ns = Namespace "hydra.test.annotations"

module_ :: Module
module_ = Module ns elements [Annotations.ns, Lexical.ns] [] $
    Just "Test cases for hydra.annotations functions"
  where
    elements = [Phantoms.toBinding allTests]


-- | Test cases for getTermAnnotation and setTermAnnotation
arbitraryAnnotationTests :: TTerm TestGroup
arbitraryAnnotationTests = subgroup "arbitrary annotations" [
  -- Set a single key/value pair (multiple cases for property test coverage)
  -- Note: These tests require interpretation because setTermAnnotation uses maps.alter which has no interpreter impl
  evalCase "set single annotation #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ int32Term 42) @@ stringTerm "foo")
    (annotatedTerm (stringTerm "foo") $ MetaTerms.map $ Maps.singleton (nameTerm "k1") (int32Term 42)),
  evalCase "set single annotation #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "myKey" @@ (optional $ just $ int32Term (-17)) @@ stringTerm "bar")
    (annotatedTerm (stringTerm "bar") $ MetaTerms.map $ Maps.singleton (nameTerm "myKey") (int32Term (-17))),
  evalCase "set single annotation #3"
    (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ stringTerm "hello") @@ int32Term 0)
    (annotatedTerm (int32Term 0) $ MetaTerms.map $ Maps.singleton (nameTerm "x") (stringTerm "hello")),

  -- Retrieve a single value (multiple cases)
  -- Note: These tests require the interpreter because getTermAnnotation uses pattern matching on Term
  evalCaseWithTags "get existing annotation #1" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "k1"
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "value") @@ int32Term 42))
    (optional $ just $ stringTerm "value"),
  evalCaseWithTags "get existing annotation #2" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "foo"
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "foo" @@ (optional $ just $ stringTerm "") @@ int32Term 99))
    (optional $ just $ stringTerm ""),
  evalCaseWithTags "get existing annotation #3" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "key"
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "key" @@ (optional $ just $ int32Term 123) @@ stringTerm "test"))
    (optional $ just $ int32Term 123),

  -- Retrieve a null value (annotation not present) - multiple cases
  evalCaseWithTags "get missing annotation #1" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "k1" @@ int16Term 42)
    (optional nothing),
  evalCaseWithTags "get missing annotation #2" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "nonexistent" @@ stringTerm "hello")
    (optional nothing),
  evalCaseWithTags "get missing annotation #3" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "k1"
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k2" @@ (optional $ just $ int32Term 1) @@ int32Term 42))
    (optional nothing),

  -- Set multiple values (multiple cases)
  evalCase "set multiple annotations #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k2" @@ (optional $ just $ int32Term 200)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "first") @@ booleanTerm True))
    (annotatedTerm (booleanTerm True) $ MetaTerms.map $ Maps.fromList $ Phantoms.list [
      Phantoms.pair (nameTerm "k1") (stringTerm "first"),
      Phantoms.pair (nameTerm "k2") (int32Term 200)]),
  evalCase "set multiple annotations #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "b" @@ (optional $ just $ int32Term 0)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "a" @@ (optional $ just $ int32Term (-5)) @@ stringTerm "test"))
    (annotatedTerm (stringTerm "test") $ MetaTerms.map $ Maps.fromList $ Phantoms.list [
      Phantoms.pair (nameTerm "a") (int32Term (-5)),
      Phantoms.pair (nameTerm "b") (int32Term 0)]),

  -- An outer annotation overrides an inner one (multiple cases)
  evalCase "outer annotation overrides inner #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "outer")
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "inner") @@ stringTerm "bar"))
    (annotatedTerm (stringTerm "bar") $ MetaTerms.map $ Maps.singleton (nameTerm "k1") (stringTerm "outer")),
  evalCase "outer annotation overrides inner #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ stringTerm "new")
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ stringTerm "old") @@ int32Term 42))
    (annotatedTerm (int32Term 42) $ MetaTerms.map $ Maps.singleton (nameTerm "x") (stringTerm "new")),
  evalCase "outer annotation overrides inner #3"
    (metaref Annotations.setTermAnnotation @@ nameTerm "key" @@ (optional $ just $ int32Term 999)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "key" @@ (optional $ just $ int32Term 1) @@ booleanTerm False))
    (annotatedTerm (booleanTerm False) $ MetaTerms.map $ Maps.singleton (nameTerm "key") (int32Term 999)),

  -- Unset a single annotation (multiple cases)
  evalCase "unset single annotation #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional nothing)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "foo") @@ int64Term 137))
    (int64Term 137),
  evalCase "unset single annotation #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional nothing)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "x" @@ (optional $ just $ int32Term 42) @@ stringTerm "test"))
    (stringTerm "test"),

  -- Unset one of multiple annotations (multiple cases)
  evalCase "unset one of multiple annotations #1"
    (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional nothing)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k2" @@ (optional $ just $ int32Term 200)
        @@ (metaref Annotations.setTermAnnotation @@ nameTerm "k1" @@ (optional $ just $ stringTerm "first") @@ int64Term 137)))
    (annotatedTerm (int64Term 137) $ MetaTerms.map $ Maps.singleton (nameTerm "k2") (int32Term 200)),
  evalCase "unset one of multiple annotations #2"
    (metaref Annotations.setTermAnnotation @@ nameTerm "b" @@ (optional nothing)
      @@ (metaref Annotations.setTermAnnotation @@ nameTerm "b" @@ (optional $ just $ int32Term 2)
        @@ (metaref Annotations.setTermAnnotation @@ nameTerm "a" @@ (optional $ just $ int32Term 1) @@ stringTerm "x")))
    (annotatedTerm (stringTerm "x") $ MetaTerms.map $ Maps.singleton (nameTerm "a") (int32Term 1))]

-- | Test cases for getTermDescription and setTermDescription
descriptionTests :: TTerm TestGroup
descriptionTests = subgroup "descriptions" [
  -- Set a single description (multiple cases)
  -- Note: These tests require interpretation because setTermDescription uses maps.alter which has no interpreter impl
  evalCase "set description #1"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "my description") @@ stringTerm "foo")
    (annotatedTerm (stringTerm "foo") $ MetaTerms.map $ Maps.singleton (nameTerm "description") (stringTerm "my description")),
  evalCase "set description #2"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "") @@ int32Term 42)
    (annotatedTerm (int32Term 42) $ MetaTerms.map $ Maps.singleton (nameTerm "description") (stringTerm "")),
  evalCase "set description #3"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "A longer description with spaces") @@ booleanTerm True)
    (annotatedTerm (booleanTerm True) $ MetaTerms.map $ Maps.singleton (nameTerm "description") (stringTerm "A longer description with spaces")),

  -- Get existing description (returns Flow Graph (Maybe String))
  -- Note: These tests require the interpreter because getTermDescription uses Flow and pattern matching
  evalCaseWithTags "get existing description #1" [tag_requiresFlowDecoding]
    (unFlowTerm @@ (metaref Annotations.getTermDescription
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "hello") @@ int32Term 42))
      @@ testState @@ testTrace)
    (flowStateTerm (optional $ just $ (optional $ just $ string "hello")) testState testTrace),
  evalCaseWithTags "get existing description #2" [tag_requiresFlowDecoding]
    (unFlowTerm @@ (metaref Annotations.getTermDescription
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "") @@ stringTerm "test"))
      @@ testState @@ testTrace)
    (flowStateTerm (optional $ just $ (optional $ just $ string "")) testState testTrace),
  evalCaseWithTags "get existing description #3" [tag_requiresFlowDecoding]
    (unFlowTerm @@ (metaref Annotations.getTermDescription
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "desc") @@ booleanTerm False))
      @@ testState @@ testTrace)
    (flowStateTerm (optional $ just $ (optional $ just $ string "desc")) testState testTrace),

  -- Get missing description (no description annotation present)
  evalCaseWithTags "get missing description #1" [tag_requiresFlowDecoding]
    (unFlowTerm @@ (metaref Annotations.getTermDescription @@ int16Term 42)
      @@ testState @@ testTrace)
    (flowStateTerm (optional $ just $ (optional nothing)) testState testTrace),
  evalCaseWithTags "get missing description #2" [tag_requiresFlowDecoding]
    (unFlowTerm @@ (metaref Annotations.getTermDescription @@ stringTerm "no description here")
      @@ testState @@ testTrace)
    (flowStateTerm (optional $ just $ (optional nothing)) testState testTrace),
  evalCaseWithTags "get missing description #3" [tag_requiresFlowDecoding]
    (unFlowTerm @@ (metaref Annotations.getTermDescription @@ int32Term 0)
      @@ testState @@ testTrace)
    (flowStateTerm (optional $ just $ (optional nothing)) testState testTrace),

  -- An outer description overrides an inner one (multiple cases)
  evalCase "outer description overrides inner #1"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "outer")
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "inner") @@ stringTerm "bar"))
    (annotatedTerm (stringTerm "bar") $ MetaTerms.map $ Maps.singleton (nameTerm "description") (stringTerm "outer")),
  evalCase "outer description overrides inner #2"
    (metaref Annotations.setTermDescription @@ (optional $ just $ string "new")
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "old") @@ int32Term 99))
    (annotatedTerm (int32Term 99) $ MetaTerms.map $ Maps.singleton (nameTerm "description") (stringTerm "new")),

  -- Unset a description (multiple cases)
  evalCase "unset description #1"
    (metaref Annotations.setTermDescription @@ (optional nothing)
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "desc") @@ int64Term 137))
    (int64Term 137),
  evalCase "unset description #2"
    (metaref Annotations.setTermDescription @@ (optional nothing)
      @@ (metaref Annotations.setTermDescription @@ (optional $ just $ string "to be removed") @@ stringTerm "test"))
    (stringTerm "test")]

-- | Test cases for layered (non-compact) annotations
-- Note: These tests require interpretation because they call getTermAnnotation which is not a primitive
layeredAnnotationTests :: TTerm TestGroup
layeredAnnotationTests = subgroup "layered annotations" [
  -- Annotations at different levels, with different keys, are all available
  evalCaseWithTags "get annotation from unannotated term" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "one" @@ int32Term 42)
    (optional nothing),

  evalCaseWithTags "get annotation from singly annotated term" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "one"
      @@ (annotatedTerm (int32Term 42) $ MetaTerms.map $ Maps.singleton (nameTerm "one") (int32Term 1)))
    (optional $ just $ int32Term 1),

  evalCaseWithTags "get inner annotation from doubly annotated term" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "one"
      @@ (annotatedTerm
           (annotatedTerm (int32Term 42) $ MetaTerms.map $ Maps.singleton (nameTerm "one") (int32Term 1))
           $ MetaTerms.map $ Maps.singleton (nameTerm "two") (int32Term 2)))
    (optional $ just $ int32Term 1),

  evalCaseWithTags "get outer annotation from doubly annotated term" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "two"
      @@ (annotatedTerm
           (annotatedTerm (int32Term 42) $ MetaTerms.map $ Maps.singleton (nameTerm "one") (int32Term 1))
           $ MetaTerms.map $ Maps.singleton (nameTerm "two") (int32Term 2)))
    (optional $ just $ int32Term 2),

  -- Non-overridden annotation still accessible in triply annotated term
  evalCaseWithTags "get non-overridden annotation from triply annotated term" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "two"
      @@ (annotatedTerm
           (annotatedTerm
             (annotatedTerm (int32Term 42) $ MetaTerms.map $ Maps.singleton (nameTerm "one") (int32Term 1))
             $ MetaTerms.map $ Maps.singleton (nameTerm "two") (int32Term 2))
           $ MetaTerms.map $ Maps.singleton (nameTerm "one") (int32Term 99)))
    (optional $ just $ int32Term 2),

  -- Outer annotations override inner ones
  evalCaseWithTags "outer annotation overrides inner in layered term" [tag_requiresFlowDecoding]
    (metaref Annotations.getTermAnnotation @@ nameTerm "one"
      @@ (annotatedTerm
           (annotatedTerm
             (annotatedTerm (int32Term 42) $ MetaTerms.map $ Maps.singleton (nameTerm "one") (int32Term 1))
             $ MetaTerms.map $ Maps.singleton (nameTerm "two") (int32Term 2))
           $ MetaTerms.map $ Maps.singleton (nameTerm "one") (int32Term 99)))
    (optional $ just $ int32Term 99)]

allTests :: TBinding TestGroup
allTests = Phantoms.definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.annotations functions" $
    supergroup "annotations" [
      arbitraryAnnotationTests,
      descriptionTests,
      layeredAnnotationTests]
