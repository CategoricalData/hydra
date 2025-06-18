-- This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki:
--   https://hackage.haskell.org/package/GraphSCC

module Hydra.Staging.Tarjan (
  Graph,
  Vertex,
  adjacencyListsToGraph,
  stronglyConnectedComponents
) where

import Hydra.Compute
import Hydra.Flows
import Hydra.Errors
import Hydra.Topology

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Maybe as Y

-- | Given a list of adjacency lists represented as (key, [key]) pairs,
--   construct a graph along with a function mapping each vertex (an Int)
--   back to its original key.
adjacencyListsToGraph :: Ord key
  => [(key, [key])]
  -> (Graph, Vertex -> key)
adjacencyListsToGraph edges0 = (graph, vertexToKey)
  where
    sortedEdges = L.sortBy (\(k1, _) (k2, _) -> compare k1 k2) edges0
    indexedEdges = zip [0..] sortedEdges
    keyToVertex = M.fromList [ (k, v) | (v, (k, _)) <- indexedEdges ]
    vertexMap   = M.fromList [ (v, k) | (v, (k, _)) <- indexedEdges ]
    graph       = M.fromList
                    [ (v, Y.mapMaybe (\k -> M.lookup k keyToVertex) neighbors)
                    | (v, (_, neighbors)) <- indexedEdges ]
    vertexToKey v = vertexMap M.! v

initialState :: TarjanState
initialState = TarjanState 0 M.empty M.empty [] S.empty []

-- | Pop vertices off the stack until the given vertex is reached,
--   collecting the current strongly connected component.
popStackUntil :: Vertex -> Flow TarjanState [Vertex]
popStackUntil v = go []
  where
    go acc = do
      st <- getState
      case tarjanStateStack st of
        []     -> error "popStackUntil: empty stack"
        (x:xs) -> do
          modify $ \s -> s { tarjanStateStack = xs, tarjanStateOnStack = S.delete x (tarjanStateOnStack s) }
          let acc' = x : acc
          if x == v
            then return (reverse acc')
            else go acc'

-- | Visit a vertex and recursively explore its successors.
strongConnect :: Graph -> Vertex -> Flow TarjanState ()
strongConnect graph v = do
  st <- getState
  let i = tarjanStateCounter st
  modify $ \s -> s { tarjanStateCounter = i + 1
                       , tarjanStateIndices      = M.insert v i (tarjanStateIndices s)
                       , tarjanStateLowLinks     = M.insert v i (tarjanStateLowLinks s)
                       , tarjanStateStack        = v : tarjanStateStack s
                       , tarjanStateOnStack      = S.insert v (tarjanStateOnStack s)
                       }
  let neighbors = M.findWithDefault [] v graph
  CM.forM_ neighbors $ \w -> do
    st' <- getState
    if not (M.member w (tarjanStateIndices st'))
      then do
         strongConnect graph w
         stAfter <- getState
         let low_v = M.findWithDefault maxBound v (tarjanStateLowLinks stAfter)
             low_w = M.findWithDefault maxBound w (tarjanStateLowLinks stAfter)
         modify $ \s -> s { tarjanStateLowLinks = M.insert v (min low_v low_w) (tarjanStateLowLinks s) }
      else if S.member w (tarjanStateOnStack st')
        then do
         let low_v = M.findWithDefault maxBound v (tarjanStateLowLinks st')
             idx_w = M.findWithDefault maxBound w (tarjanStateIndices st')
         modify $ \s -> s { tarjanStateLowLinks = M.insert v (min low_v idx_w) (tarjanStateLowLinks s) }
        else return ()
  stFinal <- getState
  let low_v = M.findWithDefault maxBound v (tarjanStateLowLinks stFinal)
      idx_v = M.findWithDefault maxBound v (tarjanStateIndices stFinal)
  CM.when (low_v == idx_v) $ do
    comp <- popStackUntil v
    modify $ \s -> s { tarjanStateSccs = comp : tarjanStateSccs s }

-- | Compute the strongly connected components of the given graph.
--   The components are returned in reverse topological order.
stronglyConnectedComponents :: Graph -> [[Vertex]]
stronglyConnectedComponents graph = reverse $ fmap L.sort $ tarjanStateSccs finalState
  where
    verts = M.keys graph
    finalState = exec (mapM_ (\v -> do
                      visited <- (M.member v . tarjanStateIndices) <$> getState
                      if not visited then strongConnect graph v else return ()
                    ) verts) initialState
