module Hydra.Sources.Test.TestSuite (testSuiteModule) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Kernel.Terms.All
import Hydra.Dsl.Phantoms as Base
import Hydra.Dsl.Testing
import qualified Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes

import Hydra.Sources.Test.Lib.Lists
import Hydra.Sources.Test.Lib.Strings
import Hydra.Sources.Test.Formatting
import Hydra.Sources.Test.Inference.InferenceSuite
import Hydra.Sources.Test.TestGraph

import qualified Data.List as L


testSuiteNs = Namespace "hydra.test.testSuite"

testSuiteModule :: Module
testSuiteModule = Module testSuiteNs elements
    [testGraphModule]
    KernelTypes.kernelTypesModules $
    Just ("Hydra's common test suite, which is designed to run identically in each Hydra implementation;"
      <> " the criterion for a true Hydra implementation is that all test cases pass.")
  where
    elements = [
      allTestsEl,
      formattingTestsEl,
      inferenceTestsEl,
      listPrimitiveTestsEl,
      primitiveTestsEl,
      stringPrimitiveTestsEl]

allTestsEl :: Binding
allTestsEl = encodedTestGroupToBinding testSuiteNs "allTests" $ tgroup "All tests" Nothing subgroups []
  where
    subgroups = fmap groupRef [
      formattingTestsEl,
      inferenceTestsEl,
      primitiveTestsEl]

formattingTestsEl = testGroupToBinding testSuiteNs "formattingTests" formattingTests

inferenceTestsEl = encodedTestGroupToBinding testSuiteNs "inferenceTests" inferenceTests

listPrimitiveTestsEl = testGroupToBinding testSuiteNs "listPrimitiveTests" listPrimitiveTests

primitiveTestsEl = encodedTestGroupToBinding testSuiteNs "primitiveTests" $
    tgroup "Primitive functions" (Just "Test cases for primitive functions") primGroups []
  where
    primGroups = fmap groupRef [
      listPrimitiveTestsEl,
      stringPrimitiveTestsEl]

stringPrimitiveTestsEl = testGroupToBinding testSuiteNs "stringPrimitiveTests" stringPrimitiveTests
