-- Note: this is an automatically generated file. Do not edit.

-- | Hydra's hoisting test suite

module Hydra.Test.Hoisting.All where

import qualified Hydra.Test.Hoisting.Cases as Cases
import qualified Hydra.Test.Hoisting.Let as Let
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The group of all hoisting tests
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "hoisting",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Cases.allTests,
    Let.allTests],
  Testing.testGroupCases = []}
