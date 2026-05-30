-- Note: this is an automatically generated file. Do not edit.
-- | Default term-level implementations of Pair functions for the Hydra interpreter.

module Hydra.Lib.Defaults.Pairs where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
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
