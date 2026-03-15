-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.util

module Hydra.Dsl.Util where

import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

caseConventionCamel :: Util.CaseConvention
caseConventionCamel = Util.CaseConventionCamel

caseConventionPascal :: Util.CaseConvention
caseConventionPascal = Util.CaseConventionPascal

caseConventionLowerSnake :: Util.CaseConvention
caseConventionLowerSnake = Util.CaseConventionLowerSnake

caseConventionUpperSnake :: Util.CaseConvention
caseConventionUpperSnake = Util.CaseConventionUpperSnake

comparisonLessThan :: Util.Comparison
comparisonLessThan = Util.ComparisonLessThan

comparisonEqualTo :: Util.Comparison
comparisonEqualTo = Util.ComparisonEqualTo

comparisonGreaterThan :: Util.Comparison
comparisonGreaterThan = Util.ComparisonGreaterThan

precisionArbitrary :: Util.Precision
precisionArbitrary = Util.PrecisionArbitrary

precisionBits :: (Int -> Util.Precision)
precisionBits x = (Util.PrecisionBits x)
