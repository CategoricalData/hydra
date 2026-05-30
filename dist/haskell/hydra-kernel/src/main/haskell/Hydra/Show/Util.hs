-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.util types

module Hydra.Show.Util where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
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
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Show a case convention as a string
caseConvention :: Util.CaseConvention -> String
caseConvention c =
    case c of
      Util.CaseConventionLowerSnake -> "lower_snake_case"
      Util.CaseConventionUpperSnake -> "UPPER_SNAKE_CASE"
      Util.CaseConventionCamel -> "camelCase"
      Util.CaseConventionPascal -> "PascalCase"
-- | Show a comparison result as a string
comparison :: Util.Comparison -> String
comparison c =
    case c of
      Util.ComparisonLessThan -> "lessThan"
      Util.ComparisonEqualTo -> "equalTo"
      Util.ComparisonGreaterThan -> "greaterThan"
