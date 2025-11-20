{-# LANGUAGE OverloadedStrings #-}

-- | Type checking failure test cases
module Hydra.Sources.Test.Checking.Failures where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as T
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Sources.Test.TestGraph

import Prelude hiding (map, product, sum)
import qualified Data.List as L
import qualified Data.Map as M


failuresTests :: TTerm TestGroup
failuresTests = supergroup "Failures" [
  failOnUntypedTests]

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

failOnUntypedTests :: TTerm TestGroup
failOnUntypedTests = supergroup "Fail on untyped (pre-inference) terms" [
  untypedLambdasTests]

untypedLambdasTests :: TTerm TestGroup
untypedLambdasTests = subgroup "Untyped lambdas" [
  -- Note: The original HSpec test for this section was a failure test (typeOfShouldFail)
  -- which tested that typeOf fails on untyped terms. The TTerm DSL format may need
  -- a different mechanism for representing failure tests.
  ]
