-- Note: this is an automatically generated file. Do not edit.

-- | Hydra's validation test suite

module Hydra.Test.Validate.All where

import qualified Hydra.Test.Validate.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The group of all validation tests
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "validation",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Core.allTests],
      Testing.testGroupCases = []}
