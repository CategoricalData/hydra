-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Equality functions for the Hydra interpreter.

module Hydra.Eval.Lib.Equality where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Interpreter-friendly identity function.
identity :: t0 -> t1 -> t2 -> Either t3 t2
identity cx g x = Right x

-- | Interpreter-friendly max.
max :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
max cx g x y =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gte")),
              Core.applicationArgument = x})),
            Core.applicationArgument = y}))})),
        Core.applicationArgument = x})),
      Core.applicationArgument = y}))

-- | Interpreter-friendly min.
min :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
min cx g x y =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lte")),
              Core.applicationArgument = x})),
            Core.applicationArgument = y}))})),
        Core.applicationArgument = x})),
      Core.applicationArgument = y}))
