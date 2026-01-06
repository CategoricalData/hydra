-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Maybe functions for the Hydra interpreter.

module Hydra.Eval.Lib.Maybes where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

apply :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
apply funOptTerm argOptTerm = ((\x -> case x of
  Core.TermMaybe v1 -> ((\x -> case x of
    Core.TermMaybe v2 -> (Flows.pure (Core.TermMaybe (Maybes.bind v1 (\f -> Maybes.map (\x -> Core.TermApplication (Core.Application {
      Core.applicationFunction = f,
      Core.applicationArgument = x})) v2))))
    _ -> (Monads.unexpected "optional value" (Core__.term argOptTerm))) argOptTerm)
  _ -> (Monads.unexpected "optional function" (Core__.term funOptTerm))) funOptTerm)

bind :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
bind optTerm funTerm = ((\x -> case x of
  Core.TermMaybe v1 -> (Flows.pure (Maybes.maybe (Core.TermMaybe Nothing) (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = val})) v1))
  _ -> (Monads.unexpected "optional value" (Core__.term optTerm))) optTerm)

cases :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
cases optTerm defaultTerm funTerm = ((\x -> case x of
  Core.TermMaybe v1 -> (Flows.pure (Maybes.maybe defaultTerm (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = val})) v1))
  _ -> (Monads.unexpected "optional value" (Core__.term optTerm))) optTerm)

compose :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
compose funF funG xTerm = (Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.bind"))),
    Core.applicationArgument = (Core.TermApplication (Core.Application {
      Core.applicationFunction = funF,
      Core.applicationArgument = xTerm}))})),
  Core.applicationArgument = funG})))

map :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
map funTerm optTerm = ((\x -> case x of
  Core.TermMaybe v1 -> (Flows.pure (Core.TermMaybe (Maybes.map (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = val})) v1)))
  _ -> (Monads.unexpected "optional value" (Core__.term optTerm))) optTerm)

-- | Interpreter-friendly mapMaybe for List terms.
mapMaybe :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
mapMaybe funTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.cat"))),
  Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = el})) elements))}))))

maybe :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
maybe defaultTerm funTerm optTerm = ((\x -> case x of
  Core.TermMaybe v1 -> (Flows.pure (Maybes.maybe defaultTerm (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = val})) v1))
  _ -> (Monads.unexpected "optional value" (Core__.term optTerm))) optTerm)
