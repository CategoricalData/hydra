-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Pair functions for the Hydra interpreter.

module Hydra.Eval.Lib.Pairs where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

bimap :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
bimap firstFun secondFun pairTerm = ((\x -> case x of
  Core.TermPair v1 ->  
    let fst = (Pairs.first v1)
    in  
      let snd = (Pairs.second v1)
      in (Flows.pure (Core.TermPair (Core.TermApplication (Core.Application {
        Core.applicationFunction = firstFun,
        Core.applicationArgument = fst}), (Core.TermApplication (Core.Application {
        Core.applicationFunction = secondFun,
        Core.applicationArgument = snd})))))
  _ -> (Monads.unexpected "pair value" (Core_.term pairTerm))) pairTerm)
