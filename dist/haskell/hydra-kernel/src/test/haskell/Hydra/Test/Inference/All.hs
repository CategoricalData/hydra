-- Note: this is an automatically generated file. Do not edit.
-- | Hydra's inference test suite

module Hydra.Test.Inference.All where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Test.Inference.AlgebraicTypes as AlgebraicTypes
import qualified Hydra.Test.Inference.AlgorithmW as AlgorithmW
import qualified Hydra.Test.Inference.Classes as Classes
import qualified Hydra.Test.Inference.Failures as Failures
import qualified Hydra.Test.Inference.Fundamentals as Fundamentals
import qualified Hydra.Test.Inference.KernelExamples as KernelExamples
import qualified Hydra.Test.Inference.NominalTypes as NominalTypes
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | The group of all inference tests
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "inference",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        AlgebraicTypes.allTests,
        AlgorithmW.allTests,
        Classes.allTests,
        Failures.allTests,
        Fundamentals.allTests,
        KernelExamples.allTests,
        NominalTypes.allTests],
      Testing.testGroupCases = []}
