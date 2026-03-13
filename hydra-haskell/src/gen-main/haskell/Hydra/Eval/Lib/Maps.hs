-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Map functions for the Hydra interpreter.

module Hydra.Eval.Lib.Maps where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Interpreter-friendly alter for Map terms.
alter :: (Context.Context -> t0 -> Core.Term -> Core.Term -> Core.Term -> Either (Context.InContext Error.Error) Core.Term)
alter cx g funTerm keyTerm mapTerm = ((\x -> case x of
  Core.TermMap v0 ->  
    let currentVal = (Maps.lookup keyTerm v0)
    in  
      let newVal = (Core.TermApplication (Core.Application {
              Core.applicationFunction = funTerm,
              Core.applicationArgument = (Core.TermMaybe currentVal)}))
      in (Right (Core.TermApplication (Core.Application {
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
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "map value") " but found ") (Core_.term mapTerm)))),
    Context.inContextContext = cx}))) mapTerm)

-- | Interpreter-friendly bimap for Map terms.
bimap :: (Context.Context -> t0 -> Core.Term -> Core.Term -> Core.Term -> Either (Context.InContext Error.Error) Core.Term)
bimap cx g keyFun valFun mapTerm = ((\x -> case x of
  Core.TermMap v0 ->  
    let pairs = (Maps.toList v0)
    in (Right (Core.TermMap (Maps.fromList (Lists.map (\p ->  
      let k = (Pairs.first p)
      in  
        let v = (Pairs.second p)
        in (Core.TermApplication (Core.Application {
          Core.applicationFunction = keyFun,
          Core.applicationArgument = k}), (Core.TermApplication (Core.Application {
          Core.applicationFunction = valFun,
          Core.applicationArgument = v})))) pairs))))
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "map value") " but found ") (Core_.term mapTerm)))),
    Context.inContextContext = cx}))) mapTerm)

-- | Interpreter-friendly filter for Map terms.
filter :: (Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Error.Error) Core.Term)
filter cx g valPred mapTerm = ((\x -> case x of
  Core.TermMap v0 ->  
    let pairs = (Maps.toList v0)
    in (Right (Core.TermApplication (Core.Application {
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
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "map value") " but found ") (Core_.term mapTerm)))),
    Context.inContextContext = cx}))) mapTerm)

-- | Interpreter-friendly filterWithKey for Map terms.
filterWithKey :: (Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Error.Error) Core.Term)
filterWithKey cx g pred mapTerm = ((\x -> case x of
  Core.TermMap v0 ->  
    let pairs = (Maps.toList v0)
    in (Right (Core.TermApplication (Core.Application {
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
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "map value") " but found ") (Core_.term mapTerm)))),
    Context.inContextContext = cx}))) mapTerm)

-- | Interpreter-friendly map for Map terms.
map :: (Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Error.Error) Core.Term)
map cx g valFun mapTerm = ((\x -> case x of
  Core.TermMap v0 ->  
    let pairs = (Maps.toList v0)
    in (Right (Core.TermMap (Maps.fromList (Lists.map (\p ->  
      let k = (Pairs.first p)
      in  
        let v = (Pairs.second p)
        in (k, (Core.TermApplication (Core.Application {
          Core.applicationFunction = valFun,
          Core.applicationArgument = v})))) pairs))))
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "map value") " but found ") (Core_.term mapTerm)))),
    Context.inContextContext = cx}))) mapTerm)

-- | Interpreter-friendly mapKeys for Map terms.
mapKeys :: (Context.Context -> t0 -> Core.Term -> Core.Term -> Either (Context.InContext Error.Error) Core.Term)
mapKeys cx g keyFun mapTerm = ((\x -> case x of
  Core.TermMap v0 ->  
    let pairs = (Maps.toList v0)
    in (Right (Core.TermMap (Maps.fromList (Lists.map (\p ->  
      let k = (Pairs.first p)
      in  
        let v = (Pairs.second p)
        in (Core.TermApplication (Core.Application {
          Core.applicationFunction = keyFun,
          Core.applicationArgument = k}), v)) pairs))))
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "map value") " but found ") (Core_.term mapTerm)))),
    Context.inContextContext = cx}))) mapTerm)
