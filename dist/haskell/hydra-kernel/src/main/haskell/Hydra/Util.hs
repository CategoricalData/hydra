-- Note: this is an automatically generated file. Do not edit.
-- | General-purpose utility types used across Hydra.

module Hydra.Util where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | A naming convention for symbols, such as camelCase or snake_case
data CaseConvention =
  CaseConventionCamel |
  CaseConventionPascal |
  CaseConventionLowerSnake |
  CaseConventionUpperSnake
  deriving (Eq, Ord, Read, Show)
_CaseConvention = Core.Name "hydra.util.CaseConvention"
_CaseConvention_camel = Core.Name "camel"
_CaseConvention_pascal = Core.Name "pascal"
_CaseConvention_lowerSnake = Core.Name "lowerSnake"
_CaseConvention_upperSnake = Core.Name "upperSnake"
-- | An equality judgement: less than, equal to, or greater than
data Comparison =
  ComparisonLessThan |
  ComparisonEqualTo |
  ComparisonGreaterThan
  deriving (Eq, Ord, Read, Show)
_Comparison = Core.Name "hydra.util.Comparison"
_Comparison_lessThan = Core.Name "lessThan"
_Comparison_equalTo = Core.Name "equalTo"
_Comparison_greaterThan = Core.Name "greaterThan"
-- | A mapping from module names to values of type n, with a focus on one module name
data ModuleNames n =
  ModuleNames {
    -- | The module name in focus, together with its associated value
    moduleNamesFocus :: (Packaging.ModuleName, n),
    -- | A mapping of module names to values
    moduleNamesMapping :: (M.Map Packaging.ModuleName n)}
  deriving (Eq, Ord, Read, Show)
_ModuleNames = Core.Name "hydra.util.ModuleNames"
_ModuleNames_focus = Core.Name "focus"
_ModuleNames_mapping = Core.Name "mapping"
-- | Numeric precision: arbitrary precision, or precision to a specified number of bits
data Precision =
  -- | Arbitrary precision
  PrecisionArbitrary |
  -- | Precision to a specified number of bits
  PrecisionBits Int
  deriving (Eq, Ord, Read, Show)
_Precision = Core.Name "hydra.util.Precision"
_Precision_arbitrary = Core.Name "arbitrary"
_Precision_bits = Core.Name "bits"
