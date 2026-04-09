-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Maybe functions for the Hydra interpreter.

module Hydra.Eval.Lib.Maybes where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Interpreter-friendly applicative apply for Maybe terms.
apply :: t0 -> t1 -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
apply cx g funOptTerm argOptTerm =
    case funOptTerm of
      Core.TermMaybe v0 -> case argOptTerm of
        Core.TermMaybe v1 -> Right (Core.TermMaybe (Maybes.bind v0 (\f -> Maybes.map (\x -> Core.TermApplication (Core.Application {
          Core.applicationFunction = f,
          Core.applicationArgument = x})) v1)))
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "optional value",
          Errors.unexpectedShapeErrorActual = (Core__.term argOptTerm)})))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional function",
        Errors.unexpectedShapeErrorActual = (Core__.term funOptTerm)})))

-- | Interpreter-friendly monadic bind for Maybe terms.
bind :: t0 -> t1 -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
bind cx g optTerm funTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe (Core.TermMaybe Nothing) (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))

-- | Interpreter-friendly case analysis for Maybe terms (cases argument order).
cases :: t0 -> t1 -> Core.Term -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
cases cx g optTerm defaultTerm funTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe defaultTerm (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))

-- | Interpreter-friendly cat for list of Maybe terms.
cat :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error [Core.Term]
cat cx g listTerm =
    Eithers.bind (Core_.list g listTerm) (\elements -> Right (Lists.foldl (\acc -> \el -> case el of
      Core.TermMaybe v0 -> Maybes.maybe acc (\v -> Lists.concat2 acc (Lists.pure v)) v0
      _ -> acc) [] elements))

-- | Interpreter-friendly Kleisli composition for Maybe.
compose :: t0 -> t1 -> Core.Term -> Core.Term -> Core.Term -> Either t2 Core.Term
compose cx g funF funG xTerm =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.bind")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = funF,
          Core.applicationArgument = xTerm}))})),
      Core.applicationArgument = funG}))

-- | Interpreter-friendly fromJust for Maybe terms.
fromJust :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
fromJust cx g optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Maybes.maybe (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "Just value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))) (\val -> Right val) v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))

-- | Interpreter-friendly fromMaybe for Maybe terms.
fromMaybe :: t0 -> t1 -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
fromMaybe cx g defaultTerm optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe defaultTerm (\val -> val) v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))

-- | Interpreter-friendly isJust for Maybe terms.
isJust :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
isJust cx g optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe (Core.TermLiteral (Core.LiteralBoolean False)) (\_ -> Core.TermLiteral (Core.LiteralBoolean True)) v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))

-- | Interpreter-friendly isNothing for Maybe terms.
isNothing :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
isNothing cx g optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe (Core.TermLiteral (Core.LiteralBoolean True)) (\_ -> Core.TermLiteral (Core.LiteralBoolean False)) v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))

-- | Interpreter-friendly map for Maybe terms.
map :: t0 -> t1 -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
map cx g funTerm optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Core.TermMaybe (Maybes.map (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))

-- | Interpreter-friendly mapMaybe for List terms.
mapMaybe :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
mapMaybe cx g funTerm listTerm =
    Eithers.bind (Core_.list g listTerm) (\elements -> Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.cat")),
      Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = el})) elements))})))

-- | Interpreter-friendly case analysis for Maybe terms.
maybe :: t0 -> t1 -> Core.Term -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
maybe cx g defaultTerm funTerm optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe defaultTerm (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))

-- | Interpreter-friendly pure for Maybe terms.
pure :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
pure cx g x = Right (Core.TermMaybe (Just x))

-- | Interpreter-friendly toList for Maybe terms.
toList :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
toList cx g optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Core.TermList (Maybes.maybe [] (\val -> Lists.pure val) v0))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (Core__.term optTerm)})))
