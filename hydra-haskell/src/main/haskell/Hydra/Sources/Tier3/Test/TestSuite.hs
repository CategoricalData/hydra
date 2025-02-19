module Hydra.Sources.Tier3.Test.TestSuite (testSuiteModule) where

import Hydra.Testing
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier2.All
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Testing
import qualified Hydra.Dsl.TTerms as TTerms

import Hydra.Sources.Tier3.Test.Lib.Lists
import Hydra.Sources.Tier3.Test.Lib.Strings
import Hydra.Sources.Tier3.Test.Formatting
import Hydra.Sources.Tier3.Test.Inference
import Hydra.Sources.Tier3.Test.TestGraph

import qualified Data.List as L


testSuiteNs = Namespace "hydra.test.testSuite"
testSuitePrimitivesNs = Namespace "hydra.test.testSuite.primitives"

testSuiteModule :: Module
testSuiteModule = Module testSuiteNs elements [testGraphModule] [hydraCoreModule, hydraTestingModule] $
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

