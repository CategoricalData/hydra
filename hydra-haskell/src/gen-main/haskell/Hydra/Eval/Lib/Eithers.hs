-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Either functions for the Hydra interpreter.

module Hydra.Eval.Lib.Eithers where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

bind :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
bind eitherTerm funTerm = ((\x -> case x of
  Core.TermEither v1 -> (Flows.pure (Eithers.either (\val -> Core.TermEither (Left val)) (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = val})) v1))
  _ -> (Monads.unexpected "either value" (Core__.term eitherTerm))) eitherTerm)

bimap :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
bimap leftFun rightFun eitherTerm = ((\x -> case x of
  Core.TermEither v1 -> (Flows.pure (Eithers.either (\val -> Core.TermEither (Left (Core.TermApplication (Core.Application {
    Core.applicationFunction = leftFun,
    Core.applicationArgument = val})))) (\val -> Core.TermEither (Right (Core.TermApplication (Core.Application {
    Core.applicationFunction = rightFun,
    Core.applicationArgument = val})))) v1))
  _ -> (Monads.unexpected "either value" (Core__.term eitherTerm))) eitherTerm)

either :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
either leftFun rightFun eitherTerm = ((\x -> case x of
  Core.TermEither v1 -> (Flows.pure (Eithers.either (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = leftFun,
    Core.applicationArgument = val})) (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = rightFun,
    Core.applicationArgument = val})) v1))
  _ -> (Monads.unexpected "either value" (Core__.term eitherTerm))) eitherTerm)

map :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
map rightFun eitherTerm = ((\x -> case x of
  Core.TermEither v1 -> (Flows.pure (Eithers.either (\val -> Core.TermEither (Left val)) (\val -> Core.TermEither (Right (Core.TermApplication (Core.Application {
    Core.applicationFunction = rightFun,
    Core.applicationArgument = val})))) v1))
  _ -> (Monads.unexpected "either value" (Core__.term eitherTerm))) eitherTerm)

-- | Interpreter-friendly mapList for Either (traverse).
mapList :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
mapList funTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "accErr"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "accErr"))))})))})),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "ys"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermEither (Right (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "ys"))}))))})))})),
        Core.applicationArgument = acc}))})))})),
  Core.applicationArgument = (Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = el}))})) (Core.TermEither (Right (Core.TermList []))) (Lists.reverse elements))))

mapMaybe :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
mapMaybe funTerm maybeTerm = ((\x -> case x of
  Core.TermMaybe v1 -> (Flows.pure (Maybes.maybe (Core.TermEither (Right (Core.TermMaybe Nothing))) (\val -> Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
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
      Core.applicationArgument = val}))})) v1))
  _ -> (Monads.unexpected "maybe value" (Core__.term maybeTerm))) maybeTerm)
