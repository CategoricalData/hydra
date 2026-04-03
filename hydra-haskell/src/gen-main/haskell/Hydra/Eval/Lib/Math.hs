-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Math functions for the Hydra interpreter.

module Hydra.Eval.Lib.Math where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Interpreter-friendly even.
even :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
even cx g x =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mod"))),
            Core.applicationArgument = x})),
          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})),
      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))

-- | Interpreter-friendly odd.
odd :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
odd cx g x =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.not"))),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.even"))),
        Core.applicationArgument = x}))}))

-- | Interpreter-friendly predecessor.
pred :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
pred cx g x =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.sub"))),
        Core.applicationArgument = x})),
      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))

-- | Interpreter-friendly successor.
succ :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
succ cx g x =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
        Core.applicationArgument = x})),
      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))
