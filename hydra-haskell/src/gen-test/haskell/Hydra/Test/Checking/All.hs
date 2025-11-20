-- Note: this is an automatically generated file. Do not edit.

-- | Hydra's type checking test suite

module Hydra.Test.Checking.All where

import qualified Hydra.Test.Checking.Advanced as Advanced
import qualified Hydra.Test.Checking.AlgebraicTypes as AlgebraicTypes
import qualified Hydra.Test.Checking.Collections as Collections
import qualified Hydra.Test.Checking.Failures as Failures
import qualified Hydra.Test.Checking.Fundamentals as Fundamentals
import qualified Hydra.Test.Checking.NominalTypes as NominalTypes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The group of all type checking tests
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "checking",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Advanced.allTests,
    AlgebraicTypes.allTests,
    Collections.allTests,
    Failures.allTests,
    Fundamentals.allTests,
    NominalTypes.allTests],
  Testing.testGroupCases = []}
