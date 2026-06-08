-- Note: this is an automatically generated file. Do not edit.
-- | Default term-level implementations of optional functions for the Hydra interpreter.

module Hydra.Lib.Defaults.Optionals where
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
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Optionals as Optionals
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
-- | Interpreter-friendly applicative apply for optional terms.
apply :: t0 -> t1 -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
apply cx g funOptTerm argOptTerm =
    case funOptTerm of
      Core.TermOptional v0 -> case argOptTerm of
        Core.TermOptional v1 -> Right (Core.TermOptional (Optionals.bind v0 (\f -> Optionals.map (\x -> Core.TermApplication (Core.Application {
          Core.applicationFunction = f,
          Core.applicationArgument = x})) v1)))
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "optional value",
          Errors.unexpectedShapeErrorActual = (ShowCore.term argOptTerm)})))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional function",
        Errors.unexpectedShapeErrorActual = (ShowCore.term funOptTerm)})))
-- | Interpreter-friendly monadic bind for optional terms.
bind :: t0 -> t1 -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
bind cx g optTerm funTerm =
    case optTerm of
      Core.TermOptional v0 -> Right (Optionals.cases v0 (Core.TermOptional Nothing) (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term optTerm)})))
-- | Interpreter-friendly case analysis for optional terms (cases argument order).
cases :: t0 -> t1 -> Core.Term -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
cases cx g optTerm defaultTerm funTerm =
    case optTerm of
      Core.TermOptional v0 -> Right (Optionals.cases v0 defaultTerm (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term optTerm)})))
-- | Interpreter-friendly cat for list of optional terms.
cat :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error [Core.Term]
cat cx g listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Lists.foldl (\acc -> \el -> case el of
      Core.TermOptional v0 -> Optionals.cases v0 acc (\v -> Lists.concat2 acc (Lists.pure v))
      _ -> acc) [] elements))
-- | Interpreter-friendly Kleisli composition for optionals.
compose :: t0 -> t1 -> Core.Term -> Core.Term -> Core.Term -> Either t2 Core.Term
compose cx g funF funG xTerm =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.bind")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = funF,
          Core.applicationArgument = xTerm}))})),
      Core.applicationArgument = funG}))
-- | Interpreter-friendly fromJust for optional terms.
fromJust :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
fromJust cx g optTerm =
    case optTerm of
      Core.TermOptional v0 -> Optionals.cases v0 (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "Just value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term optTerm)})))) (\val -> Right val)
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term optTerm)})))
-- | Interpreter-friendly fromOptional for optional terms.
fromOptional :: t0 -> t1 -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
fromOptional cx g defaultTerm optTerm =
    case optTerm of
      Core.TermOptional v0 -> Right (Optionals.cases v0 defaultTerm (\val -> val))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term optTerm)})))
-- | Interpreter-friendly isGiven for optional terms.
isGiven :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
isGiven cx g optTerm =
    case optTerm of
      Core.TermOptional v0 -> Right (Optionals.cases v0 (Core.TermLiteral (Core.LiteralBoolean False)) (\_ -> Core.TermLiteral (Core.LiteralBoolean True)))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term optTerm)})))
-- | Interpreter-friendly isNone for optional terms.
isNone :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
isNone cx g optTerm =
    case optTerm of
      Core.TermOptional v0 -> Right (Optionals.cases v0 (Core.TermLiteral (Core.LiteralBoolean True)) (\_ -> Core.TermLiteral (Core.LiteralBoolean False)))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term optTerm)})))
-- | Interpreter-friendly map for optional terms.
map :: t0 -> t1 -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
map cx g funTerm optTerm =
    case optTerm of
      Core.TermOptional v0 -> Right (Core.TermOptional (Optionals.map (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term optTerm)})))
-- | Interpreter-friendly mapOptional for List terms.
mapOptional :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
mapOptional cx g funTerm listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.cat")),
      Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = el})) elements))})))
-- | Interpreter-friendly pure for optional terms.
pure :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
pure cx g x = Right (Core.TermOptional (Just x))
-- | Interpreter-friendly toList for optional terms.
toList :: t0 -> t1 -> Core.Term -> Either Errors.Error Core.Term
toList cx g optTerm =
    case optTerm of
      Core.TermOptional v0 -> Right (Core.TermList (Optionals.cases v0 [] (\val -> Lists.pure val)))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "optional value",
        Errors.unexpectedShapeErrorActual = (ShowCore.term optTerm)})))
