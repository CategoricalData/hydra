-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.util types

module Hydra.Show.Util where
import qualified Hydra.Util as Util
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
