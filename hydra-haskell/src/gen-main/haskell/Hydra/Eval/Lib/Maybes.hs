-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Maybe functions for the Hydra interpreter.

module Hydra.Eval.Lib.Maybes where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Interpreter-friendly applicative apply for Maybe terms.
apply :: Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
apply cx g funOptTerm argOptTerm =
    case funOptTerm of
      Core.TermMaybe v0 -> case argOptTerm of
        Core.TermMaybe v1 -> Right (Core.TermMaybe (Maybes.bind v0 (\f -> Maybes.map (\x -> Core.TermApplication (Core.Application {
          Core.applicationFunction = f,
          Core.applicationArgument = x})) v1)))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term argOptTerm)))),
          Context.inContextContext = cx})
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional function") " but found ") (Core__.term funOptTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly monadic bind for Maybe terms.
bind :: Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
bind cx g optTerm funTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe (Core.TermMaybe Nothing) (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly case analysis for Maybe terms (cases argument order).
cases :: Context.Context -> t0 -> Core.Term -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
cases cx g optTerm defaultTerm funTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe defaultTerm (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly cat for list of Maybe terms.
cat :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) [Core.Term]
cat cx g listTerm =
    Eithers.bind (Core_.list cx g listTerm) (\elements -> Right (Lists.foldl (\acc -> \el -> case el of
      Core.TermMaybe v0 -> Maybes.maybe acc (\v -> Lists.concat2 acc (Lists.pure v)) v0
      _ -> acc) [] elements))

-- | Interpreter-friendly Kleisli composition for Maybe.
compose :: t0 -> t1 -> Core.Term -> Core.Term -> Core.Term -> Either t2 Core.Term
compose cx g funF funG xTerm =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.bind"))),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = funF,
          Core.applicationArgument = xTerm}))})),
      Core.applicationArgument = funG}))

-- | Interpreter-friendly fromJust for Maybe terms.
fromJust :: Context.Context -> t0 -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
fromJust cx g optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "Just value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})) (\val -> Right val) v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly fromMaybe for Maybe terms.
fromMaybe :: Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
fromMaybe cx g defaultTerm optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe defaultTerm (\val -> val) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly isJust for Maybe terms.
isJust :: Context.Context -> t0 -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
isJust cx g optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe (Core.TermLiteral (Core.LiteralBoolean False)) (\_ -> Core.TermLiteral (Core.LiteralBoolean True)) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly isNothing for Maybe terms.
isNothing :: Context.Context -> t0 -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
isNothing cx g optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe (Core.TermLiteral (Core.LiteralBoolean True)) (\_ -> Core.TermLiteral (Core.LiteralBoolean False)) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly map for Maybe terms.
map :: Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
map cx g funTerm optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Core.TermMaybe (Maybes.map (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly mapMaybe for List terms.
mapMaybe :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
mapMaybe cx g funTerm listTerm =
    Eithers.bind (Core_.list cx g listTerm) (\elements -> Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.cat"))),
      Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = el})) elements))})))

-- | Interpreter-friendly case analysis for Maybe terms.
maybe :: Context.Context -> t0 -> Core.Term -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
maybe cx g defaultTerm funTerm optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe defaultTerm (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly pure for Maybe terms.
pure :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
pure cx g x = Right (Core.TermMaybe (Just x))

-- | Interpreter-friendly toList for Maybe terms.
toList :: Context.Context -> t0 -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
toList cx g optTerm =
    case optTerm of
      Core.TermMaybe v0 -> Right (Core.TermList (Maybes.maybe [] (\val -> Lists.pure val) v0))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "optional value") " but found ") (Core__.term optTerm)))),
        Context.inContextContext = cx})
