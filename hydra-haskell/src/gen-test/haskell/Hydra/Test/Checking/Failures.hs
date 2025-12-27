-- Note: this is an automatically generated file. Do not edit.

-- | Type checking failure test cases

module Hydra.Test.Checking.Failures where

import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Type checking failure test cases
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "Failures",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    failOnUntypedTests],
  Testing.testGroupCases = []}

failOnUntypedTests :: Testing.TestGroup
failOnUntypedTests = Testing.TestGroup {
  Testing.testGroupName = "Fail on untyped (pre-inference) terms",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    untypedLambdasTests],
  Testing.testGroupCases = []}

untypedLambdasTests :: Testing.TestGroup
untypedLambdasTests = Testing.TestGroup {
  Testing.testGroupName = "Untyped lambdas",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = []}
