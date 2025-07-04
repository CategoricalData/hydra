-- | This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC.

module Hydra.Tarjan where

import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Monads as Monads
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adjacencyListsToGraph :: (Ord t0) => ([(t0, [t0])] -> (M.Map Int [Int], (Int -> t0)))
adjacencyListsToGraph edges0 =  
  let sortedEdges = (Lists.sortOn fst edges0) 
      indexedEdges = (Lists.zip (Math.rangeInt32 0 (Lists.length sortedEdges)) sortedEdges)
      keyToVertex = (Maps.fromList (Lists.map (\vkNeighbors ->  
              let v = (fst vkNeighbors) 
                  kNeighbors = (snd vkNeighbors)
                  k = (fst kNeighbors)
              in (k, v)) indexedEdges))
      vertexMap = (Maps.fromList (Lists.map (\vkNeighbors ->  
              let v = (fst vkNeighbors) 
                  kNeighbors = (snd vkNeighbors)
                  k = (fst kNeighbors)
              in (v, k)) indexedEdges))
      graph = (Maps.fromList (Lists.map (\vkNeighbors ->  
              let v = (fst vkNeighbors) 
                  kNeighbors = (snd vkNeighbors)
                  neighbors = (snd kNeighbors)
              in (v, (Optionals.mapMaybe (\k -> Maps.lookup k keyToVertex) neighbors))) indexedEdges))
      vertexToKey = (\v -> Optionals.fromJust (Maps.lookup v vertexMap))
  in (graph, vertexToKey)

-- | Compute the strongly connected components of the given graph. The components are returned in reverse topological order
stronglyConnectedComponents :: (M.Map Int [Int] -> [[Int]])
stronglyConnectedComponents graph =  
  let verts = (Maps.keys graph) 
      processVertex = (\v -> Flows.bind (Flows.map (\st -> Maps.member v (Topology.tarjanStateIndices st)) Monads.getState) (\visited -> Logic.ifElse (Logic.not visited) (strongConnect graph v) (Flows.pure ())))
      finalState = (Monads.exec (Flows.mapList processVertex verts) initialState)
  in (Lists.reverse (Lists.map Lists.sort (Topology.tarjanStateSccs finalState)))

-- | Initial state for Tarjan's algorithm
initialState :: Topology.TarjanState
initialState = Topology.TarjanState {
  Topology.tarjanStateCounter = 0,
  Topology.tarjanStateIndices = Maps.empty,
  Topology.tarjanStateLowLinks = Maps.empty,
  Topology.tarjanStateStack = [],
  Topology.tarjanStateOnStack = Sets.empty,
  Topology.tarjanStateSccs = []}

-- | Pop vertices off the stack until the given vertex is reached, collecting the current strongly connected component
popStackUntil :: (Int -> Compute.Flow Topology.TarjanState [Int])
popStackUntil v =  
  let go = (\acc -> Flows.bind Monads.getState (\st -> Logic.ifElse (Lists.null (Topology.tarjanStateStack st)) (Flows.fail "popStackUntil: empty stack") ( 
          let x = (Lists.head (Topology.tarjanStateStack st)) 
              xs = (Lists.tail (Topology.tarjanStateStack st))
              newSt = Topology.TarjanState {
                      Topology.tarjanStateCounter = (Topology.tarjanStateCounter st),
                      Topology.tarjanStateIndices = (Topology.tarjanStateIndices st),
                      Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks st),
                      Topology.tarjanStateStack = xs,
                      Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack st),
                      Topology.tarjanStateSccs = (Topology.tarjanStateSccs st)}
              newSt2 = Topology.TarjanState {
                      Topology.tarjanStateCounter = (Topology.tarjanStateCounter newSt),
                      Topology.tarjanStateIndices = (Topology.tarjanStateIndices newSt),
                      Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks newSt),
                      Topology.tarjanStateStack = (Topology.tarjanStateStack newSt),
                      Topology.tarjanStateOnStack = (Sets.delete x (Topology.tarjanStateOnStack st)),
                      Topology.tarjanStateSccs = (Topology.tarjanStateSccs newSt)}
              acc_ = (Lists.cons x acc)
          in (Flows.bind (Monads.putState newSt2) (\_ -> Logic.ifElse (Equality.equalInt32 x v) (Flows.pure (Lists.reverse acc_)) (go acc_))))))
  in (go [])

-- | Visit a vertex and recursively explore its successors
strongConnect :: (M.Map Int [Int] -> Int -> Compute.Flow Topology.TarjanState ())
strongConnect graph v = (Flows.bind Monads.getState (\st ->  
  let i = (Topology.tarjanStateCounter st) 
      newSt = Topology.TarjanState {
              Topology.tarjanStateCounter = (Math.add i 1),
              Topology.tarjanStateIndices = (Maps.insert v i (Topology.tarjanStateIndices st)),
              Topology.tarjanStateLowLinks = (Maps.insert v i (Topology.tarjanStateLowLinks st)),
              Topology.tarjanStateStack = (Lists.cons v (Topology.tarjanStateStack st)),
              Topology.tarjanStateOnStack = (Sets.insert v (Topology.tarjanStateOnStack st)),
              Topology.tarjanStateSccs = (Topology.tarjanStateSccs st)}
      neighbors = (Maps.findWithDefault [] v graph)
      processNeighbor = (\w -> Flows.bind Monads.getState (\st_ -> Logic.ifElse (Logic.not (Maps.member w (Topology.tarjanStateIndices st_))) (Flows.bind (strongConnect graph w) (\_ -> Flows.bind Monads.getState (\stAfter ->  
              let low_v = (Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateLowLinks stAfter)) 
                  low_w = (Maps.findWithDefault Constants.maxInt32 w (Topology.tarjanStateLowLinks stAfter))
              in (Flows.bind (Monads.modify (\s -> Topology.TarjanState {
                Topology.tarjanStateCounter = (Topology.tarjanStateCounter s),
                Topology.tarjanStateIndices = (Topology.tarjanStateIndices s),
                Topology.tarjanStateLowLinks = (Maps.insert v (Math.min low_v low_w) (Topology.tarjanStateLowLinks s)),
                Topology.tarjanStateStack = (Topology.tarjanStateStack s),
                Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack s),
                Topology.tarjanStateSccs = (Topology.tarjanStateSccs s)})) (\_ -> Flows.pure ()))))) (Logic.ifElse (Sets.member w (Topology.tarjanStateOnStack st_)) ( 
              let low_v = (Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateLowLinks st_)) 
                  idx_w = (Maps.findWithDefault Constants.maxInt32 w (Topology.tarjanStateIndices st_))
              in (Flows.bind (Monads.modify (\s -> Topology.TarjanState {
                Topology.tarjanStateCounter = (Topology.tarjanStateCounter s),
                Topology.tarjanStateIndices = (Topology.tarjanStateIndices s),
                Topology.tarjanStateLowLinks = (Maps.insert v (Math.min low_v idx_w) (Topology.tarjanStateLowLinks s)),
                Topology.tarjanStateStack = (Topology.tarjanStateStack s),
                Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack s),
                Topology.tarjanStateSccs = (Topology.tarjanStateSccs s)})) (\_ -> Flows.pure ()))) (Flows.pure ()))))
  in (Flows.bind (Monads.putState newSt) (\_ -> Flows.bind (Flows.mapList processNeighbor neighbors) (\_ -> Flows.bind Monads.getState (\stFinal ->  
    let low_v = (Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateLowLinks stFinal)) 
        idx_v = (Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateIndices stFinal))
    in (Logic.ifElse (Equality.equalInt32 low_v idx_v) (Flows.bind (popStackUntil v) (\comp -> Flows.bind (Monads.modify (\s -> Topology.TarjanState {
      Topology.tarjanStateCounter = (Topology.tarjanStateCounter s),
      Topology.tarjanStateIndices = (Topology.tarjanStateIndices s),
      Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks s),
      Topology.tarjanStateStack = (Topology.tarjanStateStack s),
      Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack s),
      Topology.tarjanStateSccs = (Lists.cons comp (Topology.tarjanStateSccs s))})) (\_ -> Flows.pure ()))) (Flows.pure ()))))))))
