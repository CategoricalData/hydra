-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Set functions for the Hydra interpreter.

module Hydra.Eval.Lib.Sets where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Interpreter-friendly map for Set terms.
map :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
map fun setTerm = (Flows.bind (Core_.set setTerm) (\elements -> Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList"))),
  Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
    Core.applicationFunction = fun,
    Core.applicationArgument = el})) (Sets.toList elements)))}))))
