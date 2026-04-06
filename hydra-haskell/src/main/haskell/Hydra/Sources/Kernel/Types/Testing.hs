module Hydra.Sources.Kernel.Types.Testing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.testing"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [] [Core.ns] $
    Just "A model for unit testing"
  where
    definitions = [
      tag,
      testCase,
      testCaseWithMetadata,
      testGroup,
      universalTestCase]

tag :: Binding
tag = define "Tag" $
  doc "A tag for test cases" $
  T.wrap T.string

testCase :: Binding
testCase = define "TestCase" $
  doc "A test case with an actual and expected string for comparison" $
  T.union [
    "universal">:
      doc "A universal test case (string comparison)"
      universalTestCase]

universalTestCase :: Binding
universalTestCase = define "UniversalTestCase" $
  doc "A universal test case: the actual and expected values are both strings" $
  T.record [
    "actual">:
      doc "The actual result (a string produced by evaluating and showing the test expression)" $
      T.string,
    "expected">:
      doc "The expected result (a string produced by showing the expected value)" $
      T.string]

testCaseWithMetadata :: Binding
testCaseWithMetadata = define "TestCaseWithMetadata" $
  doc "A test case together with metadata" $
  T.record [
    "name">:
      doc "A short name for the test case" $
      T.string,
    "case">:
      doc "The test case itself"
      testCase,
    "description">:
      doc "An optional longer description of the test case" $
      T.optional T.string,
    "tags">:
      doc "Zero or more tags for the test case" $
      T.list tag]

testGroup :: Binding
testGroup = define "TestGroup" $
  doc "A collection of test cases with a name and optional description" $
  T.record [
    "name">:
      doc "A short name for the test group" $
      T.string,
    "description">:
      doc "An optional longer description of the test group" $
      T.optional T.string,
    "subgroups">:
      doc "Zero or more subgroups" $
      T.list testGroup,
    "cases">:
      doc "Zero or more test cases" $
      T.list testCaseWithMetadata]
