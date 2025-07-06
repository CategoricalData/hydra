-- | A DSL for decoding and validating Hydra terms at runtime. This module provides functions to extract typed values from Hydra terms with appropriate error handling.

module Hydra.Extract.Mantle where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Monads as Monads
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Extract a comparison from a term
comparison :: (Core.Term -> Compute.Flow Graph.Graph Mantle.Comparison)
comparison term = (Flows.bind (Core_.unitVariant (Core.Name "hydra.mantle.Comparison") term) (\fname -> Logic.ifElse (Equality.equal (Core.unName fname) "equalTo") (Flows.pure Mantle.ComparisonEqualTo) (Logic.ifElse (Equality.equal (Core.unName fname) "lessThan") (Flows.pure Mantle.ComparisonLessThan) (Logic.ifElse (Equality.equal (Core.unName fname) "greaterThan") (Flows.pure Mantle.ComparisonGreaterThan) (Monads.unexpected "comparison" (Core.unName fname))))))
