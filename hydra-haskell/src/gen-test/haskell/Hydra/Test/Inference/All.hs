-- Note: this is an automatically generated file. Do not edit.

-- | Hydra's inference test suite

module Hydra.Test.Inference.All where

import qualified Hydra.Test.Inference.AlgebraicTypes as AlgebraicTypes
import qualified Hydra.Test.Inference.AlgorithmW as AlgorithmW
import qualified Hydra.Test.Inference.Failures as Failures
import qualified Hydra.Test.Inference.Fundamentals as Fundamentals
import qualified Hydra.Test.Inference.KernelExamples as KernelExamples
import qualified Hydra.Test.Inference.NominalTypes as NominalTypes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The group of all inference tests
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "inference",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    AlgebraicTypes.allTests,
    AlgorithmW.allTests,
    Failures.allTests,
    Fundamentals.allTests,
    KernelExamples.allTests,
    NominalTypes.allTests],
  Testing.testGroupCases = []}
