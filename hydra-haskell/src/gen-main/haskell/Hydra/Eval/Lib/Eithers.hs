-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Either functions for the Hydra interpreter.

module Hydra.Eval.Lib.Eithers where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Interpreter-friendly bimap for Either terms.
bimap :: Context.Context -> t0 -> Core.Term -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
bimap cx g leftFun rightFun eitherTerm =
    case eitherTerm of
      Core.TermEither v0 -> Right (Eithers.either (\val -> Core.TermEither (Left (Core.TermApplication (Core.Application {
        Core.applicationFunction = leftFun,
        Core.applicationArgument = val})))) (\val -> Core.TermEither (Right (Core.TermApplication (Core.Application {
        Core.applicationFunction = rightFun,
        Core.applicationArgument = val})))) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either value") " but found ") (Core__.term eitherTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly bind for Either terms.
bind :: Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
bind cx g eitherTerm funTerm =
    case eitherTerm of
      Core.TermEither v0 -> Right (Eithers.either (\val -> Core.TermEither (Left val)) (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = val})) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either value") " but found ") (Core__.term eitherTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly case analysis for Either terms.
either :: Context.Context -> t0 -> Core.Term -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
either cx g leftFun rightFun eitherTerm =
    case eitherTerm of
      Core.TermEither v0 -> Right (Eithers.either (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = leftFun,
        Core.applicationArgument = val})) (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = rightFun,
        Core.applicationArgument = val})) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either value") " but found ") (Core__.term eitherTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly foldl for Either.
foldl :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
foldl cx g funTerm initTerm listTerm =
    Eithers.bind (Core_.list cx g listTerm) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "err"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))})))})),
        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "a"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = funTerm,
              Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
            Core.applicationArgument = el}))})))})),
      Core.applicationArgument = acc})) (Core.TermEither (Right initTerm)) elements))

-- | Interpreter-friendly fromLeft for Either terms.
fromLeft :: Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
fromLeft cx g defaultTerm eitherTerm =
    case eitherTerm of
      Core.TermEither v0 -> Right (Eithers.either (\val -> val) (\_ -> defaultTerm) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either value") " but found ") (Core__.term eitherTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly fromRight for Either terms.
fromRight :: Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
fromRight cx g defaultTerm eitherTerm =
    case eitherTerm of
      Core.TermEither v0 -> Right (Eithers.either (\_ -> defaultTerm) (\val -> val) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either value") " but found ") (Core__.term eitherTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly isLeft for Either terms.
isLeft :: Context.Context -> t0 -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
isLeft cx g eitherTerm =
    case eitherTerm of
      Core.TermEither v0 -> Right (Eithers.either (\_ -> Core.TermLiteral (Core.LiteralBoolean True)) (\_ -> Core.TermLiteral (Core.LiteralBoolean False)) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either value") " but found ") (Core__.term eitherTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly isRight for Either terms.
isRight :: Context.Context -> t0 -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
isRight cx g eitherTerm =
    case eitherTerm of
      Core.TermEither v0 -> Right (Eithers.either (\_ -> Core.TermLiteral (Core.LiteralBoolean False)) (\_ -> Core.TermLiteral (Core.LiteralBoolean True)) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either value") " but found ") (Core__.term eitherTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly lefts for list of Either terms.
lefts :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
lefts cx g listTerm =
    Eithers.bind (Core_.list cx g listTerm) (\elements -> Right (Core.TermList (Lists.foldl (\acc -> \el -> case el of
      Core.TermEither v0 -> Eithers.either (\val -> Lists.concat2 acc (Lists.pure val)) (\_ -> acc) v0
      _ -> acc) [] elements)))

-- | Interpreter-friendly map for Either terms.
map :: Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
map cx g rightFun eitherTerm =
    case eitherTerm of
      Core.TermEither v0 -> Right (Eithers.either (\val -> Core.TermEither (Left val)) (\val -> Core.TermEither (Right (Core.TermApplication (Core.Application {
        Core.applicationFunction = rightFun,
        Core.applicationArgument = val})))) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either value") " but found ") (Core__.term eitherTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly mapList for Either (traverse).
mapList :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
mapList cx g funTerm listTerm =
    Eithers.bind (Core_.list cx g listTerm) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "err"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))})))})),
        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "y"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "accErr"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "accErr"))))})))})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "ys"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.cons")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "ys"))}))))})))})),
            Core.applicationArgument = acc}))})))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = el}))})) (Core.TermEither (Right (Core.TermList []))) (Lists.reverse elements)))

-- | Interpreter-friendly mapMaybe for Either (traverse over Maybe).
mapMaybe :: Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
mapMaybe cx g funTerm maybeTerm =
    case maybeTerm of
      Core.TermMaybe v0 -> Right (Maybes.maybe (Core.TermEither (Right (Core.TermMaybe Nothing))) (\val -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "err"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))})))})),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "y"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermEither (Right (Core.TermMaybe (Just (Core.TermVariable (Core.Name "y"))))))})))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = funTerm,
          Core.applicationArgument = val}))})) v0)
      _ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "maybe value") " but found ") (Core__.term maybeTerm)))),
        Context.inContextContext = cx})

-- | Interpreter-friendly mapSet for Either (traverse over Set).
mapSet :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
mapSet cx g funTerm setTerm =
    Eithers.bind (Core_.set cx g setTerm) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "err"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))})))})),
        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "y"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "accErr"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "accErr"))))})))})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "ys"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "ys"))}))))})))})),
            Core.applicationArgument = acc}))})))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = el}))})) (Core.TermEither (Right (Core.TermSet (Sets.fromList [])))) (Sets.toList elements)))

-- | Interpreter-friendly partitionEithers for list of Either terms.
partitionEithers :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) ([Core.Term], [Core.Term])
partitionEithers cx g listTerm =
    Eithers.bind (Core_.list cx g listTerm) (\elements -> Right (Lists.foldl (\acc -> \el ->
      let ls = Pairs.first acc
          rs = Pairs.second acc
      in case el of
        Core.TermEither v0 -> Eithers.either (\val -> (Lists.concat2 ls (Lists.pure val), rs)) (\val -> (ls, (Lists.concat2 rs (Lists.pure val)))) v0
        _ -> acc) ([], []) elements))

-- | Interpreter-friendly rights for list of Either terms.
rights :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
rights cx g listTerm =
    Eithers.bind (Core_.list cx g listTerm) (\elements -> Right (Core.TermList (Lists.foldl (\acc -> \el -> case el of
      Core.TermEither v0 -> Eithers.either (\_ -> acc) (\val -> Lists.concat2 acc (Lists.pure val)) v0
      _ -> acc) [] elements)))
