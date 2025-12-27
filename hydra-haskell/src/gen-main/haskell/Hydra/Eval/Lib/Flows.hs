-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Flow functions for the Hydra interpreter.

module Hydra.Eval.Lib.Flows where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

apply :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
apply flowFun flowArg = (Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.bind"))),
    Core.applicationArgument = flowFun})),
  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "f"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.bind"))),
        Core.applicationArgument = flowArg})),
      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.pure"))),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))}))})))})))

bind :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
bind flowTerm funTerm = ((\x -> case x of
  Core.TermWrap v1 ->  
    let innerFun = (Core.wrappedTermBody v1)
    in (Flows.pure (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.compute.Flow"),
      Core.wrappedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "s"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "t"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.maybe"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "value"),
                      Core.fieldTerm = (Core.TermMaybe Nothing)},
                    Core.Field {
                      Core.fieldName = (Core.Name "state"),
                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                          Core.projectionTypeName = (Core.Name "hydra.compute.FlowState"),
                          Core.projectionField = (Core.Name "state")})))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = innerFun,
                            Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "trace"),
                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                          Core.projectionTypeName = (Core.Name "hydra.compute.FlowState"),
                          Core.projectionField = (Core.Name "trace")})))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = innerFun,
                            Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))}]}))})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "v"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = funTerm,
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = (Core.Name "hydra.compute.FlowState"),
                        Core.projectionField = (Core.Name "state")})))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = innerFun,
                          Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = (Core.Name "hydra.compute.FlowState"),
                        Core.projectionField = (Core.Name "trace")})))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = innerFun,
                          Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))}))}))})))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = (Core.Name "hydra.compute.FlowState"),
                Core.projectionField = (Core.Name "value")})))),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = innerFun,
                  Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))}))})))})))})))
  _ -> (Monads.unexpected "flow term" (Core__.term flowTerm))) flowTerm)

-- | Interpreter-friendly foldl for Flow.
foldl :: (Core.Term -> Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
foldl funTerm initTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.bind"))),
    Core.applicationArgument = acc})),
  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "accVal"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = funTerm,
        Core.applicationArgument = (Core.TermVariable (Core.Name "accVal"))})),
      Core.applicationArgument = el}))})))})) (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.pure"))),
  Core.applicationArgument = initTerm})) elements)))

map :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
map funTerm flowTerm = ((\x -> case x of
  Core.TermWrap v1 ->  
    let innerFun = (Core.wrappedTermBody v1)
    in (Flows.pure (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.compute.Flow"),
      Core.wrappedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "s"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "t"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
            Core.recordFields = [
              Core.Field {
                Core.fieldName = (Core.Name "value"),
                Core.fieldTerm = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.map"))),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "v"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = funTerm,
                        Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = (Core.Name "hydra.compute.FlowState"),
                      Core.projectionField = (Core.Name "value")})))),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = innerFun,
                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "state"),
                Core.fieldTerm = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                    Core.projectionTypeName = (Core.Name "hydra.compute.FlowState"),
                    Core.projectionField = (Core.Name "state")})))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = innerFun,
                      Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))},
              Core.Field {
                Core.fieldName = (Core.Name "trace"),
                Core.fieldTerm = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                    Core.projectionTypeName = (Core.Name "hydra.compute.FlowState"),
                    Core.projectionField = (Core.Name "trace")})))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = innerFun,
                      Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))}]}))})))})))})))
  _ -> (Monads.unexpected "flow term" (Core__.term flowTerm))) flowTerm)

mapElems :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
mapElems funTerm mapTerm = ((\x -> case x of
  Core.TermMap v1 ->  
    let pairs = (Maps.toList v1)
    in (Flows.pure (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.bind"))),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.sequence"))),
          Core.applicationArgument = (Core.TermList (Lists.map (\p ->  
            let k = (Pairs.first p)
            in  
              let v = (Pairs.second p)
              in (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.map"))),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v'"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermPair (k, (Core.TermVariable (Core.Name "v'"))))})))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = funTerm,
                  Core.applicationArgument = v}))}))) pairs))}))})),
      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "newPairs"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.pure"))),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
            Core.applicationArgument = (Core.TermVariable (Core.Name "newPairs"))}))}))})))})))
  _ -> (Monads.unexpected "map value" (Core__.term mapTerm))) mapTerm)

mapKeys :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
mapKeys funTerm mapTerm = ((\x -> case x of
  Core.TermMap v1 ->  
    let pairs = (Maps.toList v1)
    in (Flows.pure (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.bind"))),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.sequence"))),
          Core.applicationArgument = (Core.TermList (Lists.map (\p ->  
            let k = (Pairs.first p)
            in  
              let v = (Pairs.second p)
              in (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.map"))),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "k'"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "k'"), v))})))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = funTerm,
                  Core.applicationArgument = k}))}))) pairs))}))})),
      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "newPairs"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.pure"))),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
            Core.applicationArgument = (Core.TermVariable (Core.Name "newPairs"))}))}))})))})))
  _ -> (Monads.unexpected "map value" (Core__.term mapTerm))) mapTerm)

-- | Interpreter-friendly mapList for List with Flow.
mapList :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
mapList funTerm listTerm = (Flows.bind (Core_.list listTerm) (\elements -> Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.sequence"))),
  Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
    Core.applicationFunction = funTerm,
    Core.applicationArgument = el})) elements))}))))

mapMaybe :: (Core.Term -> Core.Term -> Compute.Flow t0 Core.Term)
mapMaybe funTerm maybeTerm = ((\x -> case x of
  Core.TermMaybe v1 -> (Flows.pure (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.isNothing"))),
            Core.applicationArgument = (Core.TermMaybe v1)}))})),
        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "_"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.pure"))),
            Core.applicationArgument = (Core.TermMaybe Nothing)}))})))})),
      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "_"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.map"))),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermMaybe (Just (Core.TermVariable (Core.Name "x"))))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = funTerm,
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.fromJust"))),
              Core.applicationArgument = (Core.TermMaybe v1)}))}))}))})))})),
    Core.applicationArgument = Core.TermUnit})))
  _ -> (Monads.unexpected "optional value" (Core__.term maybeTerm))) maybeTerm)

-- | Interpreter-friendly mapSet for Set with Flow.
mapSet :: (Core.Term -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
mapSet funTerm setTerm = (Flows.bind (Core_.set setTerm) (\elements -> Flows.pure (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.map"))),
    Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList")))})),
  Core.applicationArgument = (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.sequence"))),
    Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = funTerm,
      Core.applicationArgument = el})) (Sets.toList elements)))}))}))))
