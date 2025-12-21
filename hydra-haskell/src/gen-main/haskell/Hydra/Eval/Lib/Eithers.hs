-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Either functions for the Hydra interpreter.

module Hydra.Eval.Lib.Eithers where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

bimap :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
bimap leftFun rightFun eitherTerm = ((\x -> case x of
  Core.TermEither v1 -> (Flows.pure (Eithers.either (\val -> Core.TermEither (Left (Core.TermApplication (Core.Application {
    Core.applicationFunction = leftFun,
    Core.applicationArgument = val})))) (\val -> Core.TermEither (Right (Core.TermApplication (Core.Application {
    Core.applicationFunction = rightFun,
    Core.applicationArgument = val})))) v1))
  _ -> (Monads.unexpected "either value" (Core_.term eitherTerm))) eitherTerm)

either :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
either leftFun rightFun eitherTerm = ((\x -> case x of
  Core.TermEither v1 -> (Flows.pure (Eithers.either (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = leftFun,
    Core.applicationArgument = val})) (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = rightFun,
    Core.applicationArgument = val})) v1))
  _ -> (Monads.unexpected "either value" (Core_.term eitherTerm))) eitherTerm)

map :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
map rightFun eitherTerm = ((\x -> case x of
  Core.TermEither v1 -> (Flows.pure (Eithers.either (\val -> Core.TermEither (Left (Core.TermEither (Left val)))) (\val -> Core.TermEither (Right (Core.TermApplication (Core.Application {
    Core.applicationFunction = rightFun,
    Core.applicationArgument = val})))) v1))
  _ -> (Monads.unexpected "either value" (Core_.term eitherTerm))) eitherTerm)
