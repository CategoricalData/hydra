-- Note: this is an automatically generated file. Do not edit.
-- | Type checking failure test cases

module Hydra.Test.Checking.Failures where
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
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Type checking failure test cases
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "Failures",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        failOnUntypedTests],
      Testing.testGroupCases = []}
failOnUntypedTests :: Testing.TestGroup
failOnUntypedTests =
    Testing.TestGroup {
      Testing.testGroupName = "Fail on untyped (pre-inference) terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        untypedLambdasTests],
      Testing.testGroupCases = []}
untypedLambdasTests :: Testing.TestGroup
untypedLambdasTests =
    Testing.TestGroup {
      Testing.testGroupName = "Untyped lambdas",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = []}
