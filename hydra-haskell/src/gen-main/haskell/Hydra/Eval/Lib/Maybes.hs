-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Maybe functions for the Hydra interpreter.

module Hydra.Eval.Lib.Maybes where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

cases :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
cases optTerm defaultTerm funTerm = ((\x -> case x of
  Core.TermMaybe v1 -> (Flows.pure (Maybes.maybe defaultTerm (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = val})) v1))
  _ -> (Monads.unexpected "optional value" (Core_.term optTerm))) optTerm)

map :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
map funTerm optTerm = ((\x -> case x of
  Core.TermMaybe v1 -> (Flows.pure (Core.TermMaybe (Maybes.map (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = val})) v1)))
  _ -> (Monads.unexpected "optional value" (Core_.term optTerm))) optTerm)

maybe :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
maybe defaultTerm funTerm optTerm = ((\x -> case x of
  Core.TermMaybe v1 -> (Flows.pure (Maybes.maybe defaultTerm (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = val})) v1))
  _ -> (Monads.unexpected "optional value" (Core_.term optTerm))) optTerm)
