-- Note: this is an automatically generated file. Do not edit.

-- | Extraction and validation for hydra.util types

module Hydra.Extract.Util where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Extract a comparison from a term
comparison :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Util.Comparison
comparison cx graph term =
    Eithers.bind (Core_.unitVariant (Core.Name "hydra.util.Comparison") graph term) (\fname -> Logic.ifElse (Equality.equal (Core.unName fname) "equalTo") (Right Util.ComparisonEqualTo) (Logic.ifElse (Equality.equal (Core.unName fname) "lessThan") (Right Util.ComparisonLessThan) (Logic.ifElse (Equality.equal (Core.unName fname) "greaterThan") (Right Util.ComparisonGreaterThan) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
      Errors.unexpectedShapeErrorExpected = "comparison",
      Errors.unexpectedShapeErrorActual = (Core.unName fname)})))))))
