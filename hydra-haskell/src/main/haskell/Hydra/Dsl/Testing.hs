-- TODO: merge with Hydra.Dsl.Tests
module Hydra.Dsl.Testing where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core

import qualified Data.Map as M


inferenceTestCase :: TTerm Term -> TTerm TypeScheme -> TTerm InferenceTestCase
inferenceTestCase input output = record _InferenceTestCase [
  _InferenceTestCase_input>>: input,
  _InferenceTestCase_output>>: output]

{-
data InferenceTestCase =
  InferenceTestCase {
    inferenceTestCaseInput :: Core.Term,
    inferenceTestCaseOutput :: Core.TypeScheme}
  deriving (Eq, Ord, Read, Show)

-}

testCaseInference :: TTerm InferenceTestCase -> TTerm TestCase
testCaseInference = variant _TestCase _TestCase_inference

testCaseWithMetadata :: TTerm String -> TTerm TestCase -> TTerm (Maybe String) -> TTerm [Tag] -> TTerm TestCaseWithMetadata
testCaseWithMetadata name tcase description tags = record _TestCaseWithMetadata [
  _TestCaseWithMetadata_name>>: name,
  _TestCaseWithMetadata_case>>: tcase,
  _TestCaseWithMetadata_description>>: description,
  _TestCaseWithMetadata_tags>>: tags]

testGroup :: TTerm String -> TTerm (Maybe String) -> TTerm [TestGroup] -> TTerm [TestCaseWithMetadata] -> TTerm TestGroup
testGroup name description subgroups cases = record _TestGroup [
  _TestGroup_name>>: name,
  _TestGroup_description>>: description,
  _TestGroup_subgroups>>: subgroups,
  _TestGroup_cases>>: cases]
