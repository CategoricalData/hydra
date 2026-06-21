module Hydra.Sources.Kernel.Types.Testing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.testing"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just "A model for unit testing")}
  where
    definitions = [
      effectfulTestCase,
      tag,
      testCase,
      testCaseWithMetadata,
      testGroup,
      universalTestCase]

effectfulTestCase :: TypeDefinition
effectfulTestCase = define "EffectfulTestCase" $
  doc ("An effectful test case: the actual value is a thunk producing an effect, which the test runner"
    ++ " interprets (performing host interactions, e.g. file I/O within a per-case empty temporary"
    ++ " directory) to obtain a result string; the expected value is a thunk producing a string.") $
  T.record [
    "actual">:
      doc ("A thunk producing the effect to be interpreted by the test runner. The thunk defers"
        ++ " construction of the effect until the runner forces it.") $
      T.unit ~> T.effect T.string,
    "expected">:
      doc "A thunk producing the expected result string." $
      T.unit ~> T.string]

tag :: TypeDefinition
tag = define "Tag" $
  doc "A tag for test cases" $
  T.wrap T.string

testCase :: TypeDefinition
testCase = define "TestCase" $
  doc "A test case with an actual and expected string for comparison" $
  T.union [
    "effectful">:
      doc "An effectful test case (interpret an effect, then string comparison)"
      effectfulTestCase,
    "universal">:
      doc "A universal test case (string comparison)"
      universalTestCase]

testCaseWithMetadata :: TypeDefinition
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

testGroup :: TypeDefinition
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

universalTestCase :: TypeDefinition
universalTestCase = define "UniversalTestCase" $
  doc "A universal test case: the actual and expected values are thunks producing strings." $
  T.record [
    "actual">:
      doc "A thunk producing the actual result string. Wrapping in a thunk defers expression evaluation until the test runner forces it, so eagerly-evaluated hosts measure expression cost inside their timing bracket rather than at test-data load time." $
      T.unit ~> T.string,
    "expected">:
      doc "A thunk producing the expected result string." $
      T.unit ~> T.string]
