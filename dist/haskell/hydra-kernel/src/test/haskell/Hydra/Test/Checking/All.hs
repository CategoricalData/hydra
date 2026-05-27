-- Note: this is an automatically generated file. Do not edit.
-- | Hydra's type checking test suite

module Hydra.Test.Checking.All where
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
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Test.Checking.Advanced as Advanced
import qualified Hydra.Test.Checking.AlgebraicTypes as AlgebraicTypes
import qualified Hydra.Test.Checking.Collections as Collections
import qualified Hydra.Test.Checking.Failures as Failures
import qualified Hydra.Test.Checking.Fundamentals as Fundamentals
import qualified Hydra.Test.Checking.NominalTypes as NominalTypes
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | The group of all type checking tests
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
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
