-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Logic functions for the Hydra interpreter.

module Hydra.Eval.Lib.Logic where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Interpreter-friendly logical AND.
and :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
and cx g a b =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
          Core.applicationArgument = a})),
        Core.applicationArgument = b})),
      Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))}))

-- | Interpreter-friendly logical NOT.
not :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
not cx g a =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
          Core.applicationArgument = a})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
      Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))

-- | Interpreter-friendly logical OR.
or :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
or cx g a b =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
          Core.applicationArgument = a})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
      Core.applicationArgument = b}))
