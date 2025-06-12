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

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Maybe as Y

type Vertex = Int
type Graph  = M.Map Vertex [Vertex]

data TarjanState = TarjanState
  { indexCounter :: Int            -- next available index
  , indices      :: M.Map Vertex Int -- vertex -> index
  , lowlinks     :: M.Map Vertex Int -- vertex -> lowest index reachable
  , stack        :: [Vertex]         -- current DFS stack (vertices in reverse order)
  , onStack      :: S.Set Vertex    -- quick lookup for vertices on the stack
  , sccs         :: [[Vertex]]       -- accumulated strongly connected components
  }

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
      case stack st of
        []     -> error "popStackUntil: empty stack"
        (x:xs) -> do
          modify $ \s -> s { stack = xs, onStack = S.delete x (onStack s) }
          let acc' = x : acc
          if x == v
            then return (reverse acc')
            else go acc'

-- | Visit a vertex and recursively explore its successors.
strongConnect :: Graph -> Vertex -> Flow TarjanState ()
strongConnect graph v = do
  st <- getState
  let i = indexCounter st
  modify $ \s -> s { indexCounter = i + 1
                       , indices      = M.insert v i (indices s)
                       , lowlinks     = M.insert v i (lowlinks s)
                       , stack        = v : stack s
                       , onStack      = S.insert v (onStack s)
                       }
  let neighbors = M.findWithDefault [] v graph
  CM.forM_ neighbors $ \w -> do
    st' <- getState
    if not (M.member w (indices st'))
      then do
         strongConnect graph w
         stAfter <- getState
         let low_v = M.findWithDefault maxBound v (lowlinks stAfter)
             low_w = M.findWithDefault maxBound w (lowlinks stAfter)
         modify $ \s -> s { lowlinks = M.insert v (min low_v low_w) (lowlinks s) }
      else if S.member w (onStack st')
        then do
         let low_v = M.findWithDefault maxBound v (lowlinks st')
             idx_w = M.findWithDefault maxBound w (indices st')
         modify $ \s -> s { lowlinks = M.insert v (min low_v idx_w) (lowlinks s) }
        else return ()
  stFinal <- getState
  let low_v = M.findWithDefault maxBound v (lowlinks stFinal)
      idx_v = M.findWithDefault maxBound v (indices stFinal)
  CM.when (low_v == idx_v) $ do
    comp <- popStackUntil v
    modify $ \s -> s { sccs = comp : sccs s }

-- | Compute the strongly connected components of the given graph.
--   The components are returned in reverse topological order.
stronglyConnectedComponents :: Graph -> [[Vertex]]
stronglyConnectedComponents graph = reverse $ fmap L.sort $ sccs finalState
  where
    verts = M.keys graph
    finalState = exec (mapM_ (\v -> do
                      visited <- (M.member v . indices) <$> getState
                      if not visited then strongConnect graph v else return ()
                    ) verts) initialState
