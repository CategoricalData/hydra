-- Note: this is an automatically generated file. Do not edit.

-- | General-purpose utility types used across Hydra.

module Hydra.Util where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A naming convention for symbols, such as camelCase or snake_case
data CaseConvention = 
  CaseConventionCamel  |
  CaseConventionPascal  |
  CaseConventionLowerSnake  |
  CaseConventionUpperSnake 
  deriving (Eq, Ord, Read, Show)

_CaseConvention = (Core.Name "hydra.util.CaseConvention")

_CaseConvention_camel = (Core.Name "camel")

_CaseConvention_pascal = (Core.Name "pascal")

_CaseConvention_lowerSnake = (Core.Name "lowerSnake")

_CaseConvention_upperSnake = (Core.Name "upperSnake")

-- | An equality judgement: less than, equal to, or greater than
data Comparison = 
  ComparisonLessThan  |
  ComparisonEqualTo  |
  ComparisonGreaterThan 
  deriving (Eq, Ord, Read, Show)

_Comparison = (Core.Name "hydra.util.Comparison")

_Comparison_lessThan = (Core.Name "lessThan")

_Comparison_equalTo = (Core.Name "equalTo")

_Comparison_greaterThan = (Core.Name "greaterThan")

-- | An error that occurred during decoding of a term
newtype DecodingError = 
  DecodingError {
    unDecodingError :: String}
  deriving (Eq, Ord, Read, Show)

_DecodingError = (Core.Name "hydra.util.DecodingError")

-- | Numeric precision: arbitrary precision, or precision to a specified number of bits
data Precision = 
  -- | Arbitrary precision
  PrecisionArbitrary  |
  -- | Precision to a specified number of bits
  PrecisionBits Int
  deriving (Eq, Ord, Read, Show)

_Precision = (Core.Name "hydra.util.Precision")

_Precision_arbitrary = (Core.Name "arbitrary")

_Precision_bits = (Core.Name "bits")
