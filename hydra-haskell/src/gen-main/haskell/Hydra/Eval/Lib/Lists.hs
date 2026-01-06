-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of List functions for the Hydra interpreter.

module Hydra.Eval.Lib.Lists where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Pairs as Pairs
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Interpreter-friendly applicative apply for List terms.
apply :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
apply funsTerm argsTerm = (Flows.bind (Core_.list funsTerm) (\funs -> Flows.bind (Core_.list argsTerm) (\arguments ->  
  let applyOne = (\f -> Lists.map (\arg -> Core.TermApplication (Core.Application {
          Core.applicationFunction = f,
          Core.applicationArgument = arg})) arguments)
  in (Flows.pure (Core.TermList (Lists.concat (Lists.map applyOne funs)))))))

-- | Interpreter-friendly monadic bind for List terms.
bind :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
bind listTerm funTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
  Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = el})) elements))}))))

dropWhile :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
dropWhile predTerm listTerm = (Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.second"))),
  Core.applicationArgument = (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.span"))),
      Core.applicationArgument = predTerm})),
    Core.applicationArgument = listTerm}))})))

-- | Interpreter-friendly filter for List terms.
filter :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
filter predTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
  Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = predTerm,
          Core.applicationArgument = el}))})),
      Core.applicationArgument = (Core.TermList (Lists.pure el))})),
    Core.applicationArgument = (Core.TermList [])})) elements))}))))

-- | Interpreter-friendly left fold for List terms.
foldl :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
foldl funTerm initTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = acc})),
  Core.applicationArgument = el})) initTerm elements)))

-- | Interpreter-friendly map for List terms.
map :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
map funTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Core.TermList (Lists.reverse (Lists.foldl (\acc -> \el -> Lists.cons (Core.TermApplication (Core.Application {
  Core.applicationFunction = funTerm,
  Core.applicationArgument = el})) acc) [] elements)))))

-- | Interpreter-friendly sortOn for List terms.
sortOn :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
sortOn projTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Lists.foldl (\sorted -> \x ->  
  let splitResult = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.span"))),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lte"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = projTerm,
                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = projTerm,
                  Core.applicationArgument = x}))}))})))})),
          Core.applicationArgument = sorted}))
  in  
    let before = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.first"))),
            Core.applicationArgument = splitResult}))
    in  
      let after = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.second"))),
              Core.applicationArgument = splitResult}))
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
          Core.applicationArgument = before})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
            Core.applicationArgument = x})),
          Core.applicationArgument = after}))}))) (Core.TermList []) elements)))

-- | Interpreter-friendly span for List terms.
span :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
span predTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements ->  
  let initialState = (Core.TermPair (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean True), (Core.TermList [])), (Core.TermList [])))
  in  
    let finalState = (Lists.foldl (\acc -> \el ->  
            let takingLeft = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.first"))),
                    Core.applicationArgument = acc}))
            in  
              let right = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.second"))),
                      Core.applicationArgument = acc}))
              in  
                let taking = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.first"))),
                        Core.applicationArgument = takingLeft}))
                in  
                  let left = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.second"))),
                          Core.applicationArgument = takingLeft}))
                  in (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.and"))),
                            Core.applicationArgument = taking})),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = predTerm,
                            Core.applicationArgument = el}))}))})),
                      Core.applicationArgument = (Core.TermPair (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean True), (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                          Core.applicationArgument = left})),
                        Core.applicationArgument = (Core.TermList [
                          el])}))), right))})),
                    Core.applicationArgument = (Core.TermPair (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean False), left), (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                        Core.applicationArgument = right})),
                      Core.applicationArgument = (Core.TermList [
                        el])}))))}))) initialState elements)
    in (Flows.pure (Core.TermPair (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.second"))),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.first"))),
        Core.applicationArgument = finalState}))}), (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.second"))),
      Core.applicationArgument = finalState})))))))

-- | Interpreter-friendly zipWith for List terms.
zipWith :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
zipWith funTerm listTerm1 listTerm2 = (Flows.bind (Core_.list listTerm1) (\elements1 -> Flows.bind (Core_.list listTerm2) (\elements2 -> Flows.pure (Core.TermList (Lists.map (\p ->  
  let a = (Pairs.first p)
  in  
    let b = (Pairs.second p)
    in (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = a})),
      Core.applicationArgument = b}))) (Lists.zip elements1 elements2))))))
