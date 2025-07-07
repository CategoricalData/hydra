module Hydra.Sources.Test.TestSuite (testSuiteModule) where

import Hydra.Testing
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Kernel.Terms.All
import Hydra.Dsl.Phantoms as Base
import Hydra.Dsl.Testing
import qualified Hydra.Dsl.TTerms as TTerms
import Hydra.Kernel
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.All as Tier2

import Hydra.Sources.Test.Lib.Lists
import Hydra.Sources.Test.Lib.Strings
import Hydra.Sources.Test.Formatting
import Hydra.Sources.Test.Inference.InferenceSuite
import Hydra.Sources.Test.TestGraph

import qualified Data.List as L


testSuiteNs = Namespace "hydra.test.testSuite"
testSuitePrimitivesNs = Namespace "hydra.test.testSuite.primitives"

testSuiteModule :: Module
testSuiteModule = Module testSuiteNs elements
    [testGraphModule]
    [KernelTypes.hydraCoreModule, KernelTypes.hydraMantleModule, KernelTypes.hydraTestingModule] $
    Just "Test cases for primitive functions"
  where
    elements = [
      allTestsEl,
      formattingTestsEl,
      inferenceTestsEl,
      listPrimitiveTestsEl,
      primitiveTestsEl,
      stringPrimitiveTestsEl]

allTestsEl :: Element
allTestsEl = encodedTestGroupToElement testSuiteNs "allTests" $ tgroup "All tests" Nothing subgroups []
  where
    subgroups = fmap groupRef [
      formattingTestsEl,
      inferenceTestsEl,
      primitiveTestsEl]

formattingTestsEl = testGroupToElement testSuiteNs "formattingTests" formattingTests

inferenceTestsEl = encodedTestGroupToElement testSuiteNs "inferenceTests" inferenceTests

listPrimitiveTestsEl = testGroupToElement testSuiteNs "listPrimitiveTests" listPrimitiveTests

primitiveTestsEl = encodedTestGroupToElement testSuiteNs "primitiveTests" $
    tgroup "Primitive functions" (Just "Test cases for primitive functions") primGroups []
  where
    primGroups = fmap groupRef [
      listPrimitiveTestsEl,
      stringPrimitiveTestsEl]

stringPrimitiveTestsEl = testGroupToElement testSuiteNs "stringPrimitiveTests" stringPrimitiveTests

