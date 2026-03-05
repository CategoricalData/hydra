-- Note: this is an automatically generated file. Do not edit.

-- | This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC.

module Hydra.Tarjan where

import qualified Hydra.Constants as Constants
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Given a list of adjacency lists represented as (key, [key]) pairs, construct a graph along with a function mapping each vertex (an Int) back to its original key.
adjacencyListsToGraph :: Ord t0 => ([(t0, [t0])] -> (M.Map Int [Int], (Int -> t0)))
adjacencyListsToGraph edges0 =  
  let sortedEdges = (Lists.sortOn Pairs.first edges0)
  in  
    let indexedEdges = (Lists.zip (Math.range 0 (Lists.length sortedEdges)) sortedEdges)
    in  
      let keyToVertex = (Maps.fromList (Lists.map (\vkNeighbors ->  
              let v = (Pairs.first vkNeighbors)
              in  
                let kNeighbors = (Pairs.second vkNeighbors)
                in  
                  let k = (Pairs.first kNeighbors)
                  in (k, v)) indexedEdges))
      in  
        let vertexMap = (Maps.fromList (Lists.map (\vkNeighbors ->  
                let v = (Pairs.first vkNeighbors)
                in  
                  let kNeighbors = (Pairs.second vkNeighbors)
                  in  
                    let k = (Pairs.first kNeighbors)
                    in (v, k)) indexedEdges))
        in  
          let graph = (Maps.fromList (Lists.map (\vkNeighbors ->  
                  let v = (Pairs.first vkNeighbors)
                  in  
                    let kNeighbors = (Pairs.second vkNeighbors)
                    in  
                      let neighbors = (Pairs.second kNeighbors)
                      in (v, (Maybes.mapMaybe (\k -> Maps.lookup k keyToVertex) neighbors))) indexedEdges))
          in  
            let vertexToKey = (\v -> Maybes.fromJust (Maps.lookup v vertexMap))
            in (graph, vertexToKey)

-- | Compute the strongly connected components of the given graph. The components are returned in reverse topological order
stronglyConnectedComponents :: (M.Map Int [Int] -> [[Int]])
stronglyConnectedComponents graph =  
  let verts = (Maps.keys graph)
  in  
    let finalState = (Lists.foldl (\st -> \v -> Logic.ifElse (Maps.member v (Topology.tarjanStateIndices st)) st (strongConnect graph v st)) initialState verts)
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
popStackUntil :: (Int -> Topology.TarjanState -> ([Int], Topology.TarjanState))
popStackUntil v st0 =  
  let go = (\acc -> \st ->  
          let x = (Lists.head (Topology.tarjanStateStack st))
          in  
            let xs = (Lists.tail (Topology.tarjanStateStack st))
            in  
              let newSt = Topology.TarjanState {
                      Topology.tarjanStateCounter = (Topology.tarjanStateCounter st),
                      Topology.tarjanStateIndices = (Topology.tarjanStateIndices st),
                      Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks st),
                      Topology.tarjanStateStack = xs,
                      Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack st),
                      Topology.tarjanStateSccs = (Topology.tarjanStateSccs st)}
              in  
                let newSt2 = Topology.TarjanState {
                        Topology.tarjanStateCounter = (Topology.tarjanStateCounter newSt),
                        Topology.tarjanStateIndices = (Topology.tarjanStateIndices newSt),
                        Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks newSt),
                        Topology.tarjanStateStack = (Topology.tarjanStateStack newSt),
                        Topology.tarjanStateOnStack = (Sets.delete x (Topology.tarjanStateOnStack st)),
                        Topology.tarjanStateSccs = (Topology.tarjanStateSccs newSt)}
                in  
                  let acc_ = (Lists.cons x acc)
                  in (Logic.ifElse (Equality.equal x v) (Lists.reverse acc_, newSt2) (go acc_ newSt2)))
  in (go [] st0)

-- | Visit a vertex and recursively explore its successors
strongConnect :: (M.Map Int [Int] -> Int -> Topology.TarjanState -> Topology.TarjanState)
strongConnect graph v st =  
  let i = (Topology.tarjanStateCounter st)
  in  
    let newSt = Topology.TarjanState {
            Topology.tarjanStateCounter = (Math.add i 1),
            Topology.tarjanStateIndices = (Maps.insert v i (Topology.tarjanStateIndices st)),
            Topology.tarjanStateLowLinks = (Maps.insert v i (Topology.tarjanStateLowLinks st)),
            Topology.tarjanStateStack = (Lists.cons v (Topology.tarjanStateStack st)),
            Topology.tarjanStateOnStack = (Sets.insert v (Topology.tarjanStateOnStack st)),
            Topology.tarjanStateSccs = (Topology.tarjanStateSccs st)}
    in  
      let neighbors = (Maps.findWithDefault [] v graph)
      in  
        let processNeighbor = (\st_ -> \w ->  
                let lowLink = (\s ->  
                        let lowV1 = (Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateLowLinks s))
                        in  
                          let idx_w = (Maps.findWithDefault Constants.maxInt32 w (Topology.tarjanStateIndices s))
                          in Topology.TarjanState {
                            Topology.tarjanStateCounter = (Topology.tarjanStateCounter s),
                            Topology.tarjanStateIndices = (Topology.tarjanStateIndices s),
                            Topology.tarjanStateLowLinks = (Maps.insert v (Equality.min lowV1 idx_w) (Topology.tarjanStateLowLinks s)),
                            Topology.tarjanStateStack = (Topology.tarjanStateStack s),
                            Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack s),
                            Topology.tarjanStateSccs = (Topology.tarjanStateSccs s)})
                in (Logic.ifElse (Logic.not (Maps.member w (Topology.tarjanStateIndices st_))) ( 
                  let stAfter = (strongConnect graph w st_)
                  in  
                    let lowV2 = (Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateLowLinks stAfter))
                    in  
                      let low_w = (Maps.findWithDefault Constants.maxInt32 w (Topology.tarjanStateLowLinks stAfter))
                      in Topology.TarjanState {
                        Topology.tarjanStateCounter = (Topology.tarjanStateCounter stAfter),
                        Topology.tarjanStateIndices = (Topology.tarjanStateIndices stAfter),
                        Topology.tarjanStateLowLinks = (Maps.insert v (Equality.min lowV2 low_w) (Topology.tarjanStateLowLinks stAfter)),
                        Topology.tarjanStateStack = (Topology.tarjanStateStack stAfter),
                        Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack stAfter),
                        Topology.tarjanStateSccs = (Topology.tarjanStateSccs stAfter)}) (Logic.ifElse (Sets.member w (Topology.tarjanStateOnStack st_)) (lowLink st_) st_)))
        in  
          let stAfterNeighbors = (Lists.foldl processNeighbor newSt neighbors)
          in  
            let low_v = (Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateLowLinks stAfterNeighbors))
            in  
              let idx_v = (Maps.findWithDefault Constants.maxInt32 v (Topology.tarjanStateIndices stAfterNeighbors))
              in (Logic.ifElse (Equality.equal low_v idx_v) ( 
                let compResult = (popStackUntil v stAfterNeighbors)
                in  
                  let comp = (Pairs.first compResult)
                  in  
                    let stPopped = (Pairs.second compResult)
                    in Topology.TarjanState {
                      Topology.tarjanStateCounter = (Topology.tarjanStateCounter stPopped),
                      Topology.tarjanStateIndices = (Topology.tarjanStateIndices stPopped),
                      Topology.tarjanStateLowLinks = (Topology.tarjanStateLowLinks stPopped),
                      Topology.tarjanStateStack = (Topology.tarjanStateStack stPopped),
                      Topology.tarjanStateOnStack = (Topology.tarjanStateOnStack stPopped),
                      Topology.tarjanStateSccs = (Lists.cons comp (Topology.tarjanStateSccs stPopped))}) stAfterNeighbors)
