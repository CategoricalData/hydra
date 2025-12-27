-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Map functions for the Hydra interpreter.

module Hydra.Eval.Lib.Maps where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alter :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
alter funTerm keyTerm mapTerm = ((\x -> case x of
  Core.TermMap v1 ->  
    let currentVal = (Maps.lookup keyTerm v1)
    in  
      let newVal = (Core.TermApplication (Core.Application {
              Core.applicationFunction = funTerm,
              Core.applicationArgument = (Core.TermMaybe currentVal)}))
      in (Flows.pure (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.maybe"))),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.delete"))),
                Core.applicationArgument = keyTerm})),
              Core.applicationArgument = mapTerm}))})),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "newV"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.insert"))),
                  Core.applicationArgument = keyTerm})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "newV"))})),
              Core.applicationArgument = mapTerm}))})))})),
        Core.applicationArgument = newVal})))
  _ -> (Monads.unexpected "map value" (Core_.term mapTerm))) mapTerm)

bimap :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
bimap keyFun valFun mapTerm = ((\x -> case x of
  Core.TermMap v1 ->  
    let pairs = (Maps.toList v1)
    in (Flows.pure (Core.TermMap (Maps.fromList (Lists.map (\p ->  
      let k = (Pairs.first p)
      in  
        let v = (Pairs.second p)
        in (Core.TermApplication (Core.Application {
          Core.applicationFunction = keyFun,
          Core.applicationArgument = k}), (Core.TermApplication (Core.Application {
          Core.applicationFunction = valFun,
          Core.applicationArgument = v})))) pairs))))
  _ -> (Monads.unexpected "map value" (Core_.term mapTerm))) mapTerm)

filter :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
filter valPred mapTerm = ((\x -> case x of
  Core.TermMap v1 ->  
    let pairs = (Maps.toList v1)
    in (Flows.pure (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
        Core.applicationArgument = (Core.TermList (Lists.map (\p ->  
          let v = (Pairs.second p)
          in (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = valPred,
                  Core.applicationArgument = v}))})),
              Core.applicationArgument = (Core.TermList (Lists.pure (Core.TermPair (Pairs.first p, v))))})),
            Core.applicationArgument = (Core.TermList [])}))) pairs))}))})))
  _ -> (Monads.unexpected "map value" (Core_.term mapTerm))) mapTerm)

filterWithKey :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
filterWithKey pred mapTerm = ((\x -> case x of
  Core.TermMap v1 ->  
    let pairs = (Maps.toList v1)
    in (Flows.pure (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
        Core.applicationArgument = (Core.TermList (Lists.map (\p ->  
          let k = (Pairs.first p)
          in  
            let v = (Pairs.second p)
            in (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = pred,
                      Core.applicationArgument = k})),
                    Core.applicationArgument = v}))})),
                Core.applicationArgument = (Core.TermList (Lists.pure (Core.TermPair (k, v))))})),
              Core.applicationArgument = (Core.TermList [])}))) pairs))}))})))
  _ -> (Monads.unexpected "map value" (Core_.term mapTerm))) mapTerm)

map :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
map valFun mapTerm = ((\x -> case x of
  Core.TermMap v1 ->  
    let pairs = (Maps.toList v1)
    in (Flows.pure (Core.TermMap (Maps.fromList (Lists.map (\p ->  
      let k = (Pairs.first p)
      in  
        let v = (Pairs.second p)
        in (k, (Core.TermApplication (Core.Application {
          Core.applicationFunction = valFun,
          Core.applicationArgument = v})))) pairs))))
  _ -> (Monads.unexpected "map value" (Core_.term mapTerm))) mapTerm)

mapKeys :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
mapKeys keyFun mapTerm = ((\x -> case x of
  Core.TermMap v1 ->  
    let pairs = (Maps.toList v1)
    in (Flows.pure (Core.TermMap (Maps.fromList (Lists.map (\p ->  
      let k = (Pairs.first p)
      in  
        let v = (Pairs.second p)
        in (Core.TermApplication (Core.Application {
          Core.applicationFunction = keyFun,
          Core.applicationArgument = k}), v)) pairs))))
  _ -> (Monads.unexpected "map value" (Core_.term mapTerm))) mapTerm)
