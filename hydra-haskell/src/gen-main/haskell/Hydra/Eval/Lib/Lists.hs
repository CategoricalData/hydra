-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of List functions for the Hydra interpreter.

module Hydra.Eval.Lib.Lists where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Interpreter-friendly applicative apply for List terms.
apply :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
apply funsTerm argsTerm = (Flows.bind (Core_.list funsTerm) (\funs -> Flows.bind (Core_.list argsTerm) (\arguments ->  
  let applyOne = (\f -> Lists.map (\arg -> Core.TermApplication (Core.Application {
          Core.applicationFunction = f,
          Core.applicationArgument = arg})) arguments)
  in (Flows.pure (Core.TermList (Lists.concat (Lists.map applyOne funs)))))))

-- | Interpreter-friendly monadic bind for List terms.
bind :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
bind listTerm funTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
  Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = el})) elements))}))))

-- | Interpreter-friendly map for List terms.
map :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
map funTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
  Core.applicationFunction = funTerm,
  Core.applicationArgument = el})) elements))))
