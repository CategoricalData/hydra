{-# LANGUAGE OverloadedStrings #-}

-- | Type checking failure test cases
module Hydra.Sources.Test.Checking.Failures where

-- Standard imports for kernel tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing as Testing
import Hydra.Dsl.Meta.Terms as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Types as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List as L
import qualified Data.Map  as M


module_ :: Module
module_ = Module (Namespace "hydra.test.checking.failures") elements
    [TestGraph.module_]
    kernelTypesModules
    (Just "Type checking failure test cases")
  where
    elements = [
      el allTestsDef,
      el failOnUntypedTestsDef,
      el untypedLambdasTestsDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTestsDef :: TBinding TestGroup
allTestsDef = define "allTests" $
  Phantoms.doc "Type checking failure test cases" $
  supergroup "Failures" [
    ref failOnUntypedTestsDef]

------ Helper functions ------

-- Helper function to create a type checking test case
checkTest :: String -> [Tag] -> TTerm Term -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
checkTest name tags input outputTerm outputType = testCaseWithMetadata (Phantoms.string name)
  (testCaseTypeChecking $ typeCheckingTestCase input outputTerm outputType) Phantoms.nothing (Phantoms.list $ tag . unTag <$> tags)

-- Helper for tests where the term doesn't change during type checking
noChange :: String -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
noChange name term typ = checkTest name [] term term typ

-- Create a TestCase inject for type checking
testCaseTypeChecking :: TTerm TypeCheckingTestCase -> TTerm TestCase
testCaseTypeChecking = Phantoms.inject _TestCase _TestCase_typeChecking

-- Create a TypeCheckingTestCase record
typeCheckingTestCase :: TTerm Term -> TTerm Term -> TTerm Type -> TTerm TypeCheckingTestCase
typeCheckingTestCase input outputTerm outputType = Phantoms.record _TypeCheckingTestCase [
  Phantoms.field _TypeCheckingTestCase_input input,
  Phantoms.field _TypeCheckingTestCase_outputTerm outputTerm,
  Phantoms.field _TypeCheckingTestCase_outputType outputType]

------ Fail on untyped (pre-inference) terms ------

failOnUntypedTestsDef :: TBinding TestGroup
failOnUntypedTestsDef = define "failOnUntypedTests" $
  supergroup "Fail on untyped (pre-inference) terms" [
    ref untypedLambdasTestsDef]

untypedLambdasTestsDef :: TBinding TestGroup
untypedLambdasTestsDef = define "untypedLambdasTests" $
  subgroup "Untyped lambdas" [
    -- Note: The original HSpec test for this section was a failure test (typeOfShouldFail)
    -- which tested that typeOf fails on untyped terms. The TTerm DSL format may need
    -- a different mechanism for representing failure tests.
    ]
