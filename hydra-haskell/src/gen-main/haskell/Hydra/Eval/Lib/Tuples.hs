-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Tuple functions for the Hydra interpreter.

module Hydra.Eval.Lib.Tuples where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Flows as Flows
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

curry :: (Core.Term -> Compute.Flow t0 Core.Term)
curry funTerm = (Flows.pure (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "a"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "b"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermApplication (Core.Application {
      Core.applicationFunction = funTerm,
      Core.applicationArgument = (Core.TermPair (Core.TermVariable (Core.Name "a"), (Core.TermVariable (Core.Name "b"))))}))})))}))))

uncurry :: (Core.Term -> Compute.Flow t0 Core.Term)
uncurry funTerm = (Flows.pure (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "p"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermApplication (Core.Application {
      Core.applicationFunction = funTerm,
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.first"))),
        Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))})),
    Core.applicationArgument = (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.second"))),
      Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}))}))))
