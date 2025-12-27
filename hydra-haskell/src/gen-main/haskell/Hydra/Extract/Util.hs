-- Note: this is an automatically generated file. Do not edit.

-- | Extraction and validation for hydra.util types

module Hydra.Extract.Util where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Monads as Monads
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Extract a comparison from a term
comparison :: (Core.Term -> Compute.Flow Graph.Graph Util.Comparison)
comparison term = (Flows.bind (Core_.unitVariant (Core.Name "hydra.util.Comparison") term) (\fname -> Logic.ifElse (Equality.equal (Core.unName fname) "equalTo") (Flows.pure Util.ComparisonEqualTo) (Logic.ifElse (Equality.equal (Core.unName fname) "lessThan") (Flows.pure Util.ComparisonLessThan) (Logic.ifElse (Equality.equal (Core.unName fname) "greaterThan") (Flows.pure Util.ComparisonGreaterThan) (Monads.unexpected "comparison" (Core.unName fname))))))
