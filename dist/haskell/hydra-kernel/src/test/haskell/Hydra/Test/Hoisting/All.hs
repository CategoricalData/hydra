-- Note: this is an automatically generated file. Do not edit.
-- | Hydra's hoisting test suite

module Hydra.Test.Hoisting.All where
import qualified Hydra.Test.Hoisting.Cases as Cases
import qualified Hydra.Test.Hoisting.Let as Let
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | The group of all hoisting tests
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hoisting",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Cases.allTests,
        Let.allTests],
      Testing.testGroupCases = []}
