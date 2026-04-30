-- Note: this is an automatically generated file. Do not edit.
-- | Evaluation-level implementations of Pair functions for the Hydra interpreter.

module Hydra.Eval.Lib.Pairs where
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Show.Core as ShowCore
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Interpreter-friendly bimap for Pair terms.
bimap :: t0 -> t1 -> Core.Term -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
bimap cx g firstFun secondFun pairTerm =
    case pairTerm of
      Core.TermPair v0 ->
        let fst = Pairs.first v0
            snd = Pairs.second v0
        in (Right (Core.TermPair (
          Core.TermApplication (Core.Application {
            Core.applicationFunction = firstFun,
            Core.applicationArgument = fst}),
          (Core.TermApplication (Core.Application {
            Core.applicationFunction = secondFun,
            Core.applicationArgument = snd})))))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "pair value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term pairTerm)})))
-- | Interpreter-friendly first for Pair terms.
first :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
first cx g pairTerm =
    case pairTerm of
      Core.TermPair v0 -> Right (Pairs.first v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "pair value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term pairTerm)})))
-- | Interpreter-friendly second for Pair terms.
second :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
second cx g pairTerm =
    case pairTerm of
      Core.TermPair v0 -> Right (Pairs.second v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "pair value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term pairTerm)})))
