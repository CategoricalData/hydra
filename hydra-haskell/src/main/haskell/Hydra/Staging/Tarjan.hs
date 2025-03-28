-- This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki:
--   https://hackage.haskell.org/package/GraphSCC

module Hydra.Staging.Tarjan (
  Graph,
  Vertex,
  adjacencyListsToGraph,
  stronglyConnectedComponents
) where

import qualified Data.IntMap.Strict as IMS
import qualified Data.IntSet as DS
import qualified Control.Monad.State.Strict as CMS
import qualified Control.Monad as CM
import qualified Data.List            as L
import qualified Data.Map.Strict      as MS
import qualified Data.Maybe           as Y


type Vertex = Int
type Graph = IMS.IntMap [Vertex]

data TarjanState = TarjanState
  { indexCounter :: !Int           -- next available index
  , indices      :: IMS.IntMap Int -- vertex -> index
  , lowlinks     :: IMS.IntMap Int -- vertex -> lowest index reachable
  , stack        :: [Vertex]       -- current DFS stack (vertices in reverse order)
  , onStack      :: DS.IntSet      -- quick lookup for vertices on the stack
  , sccs         :: [[Vertex]]     -- accumulated strongly connected components
  }

-- | Given a list of adjacency lists represented as (key, [key]) pairs,
--   constructs a graph along with a function mapping each vertex (an Int)
--   back to its original key.
adjacencyListsToGraph :: Ord key
  => [(key, [key])]
  -> (Graph, Vertex -> key)
adjacencyListsToGraph edges0 = (graph, vertexToKey)
  where
    sortedEdges = L.sortBy (\(k1, _) (k2, _) -> compare k1 k2) edges0
    indexedEdges = zip [0..] sortedEdges
    keyToVertex = MS.fromList [ (k, v) | (v, (k, _)) <- indexedEdges ]
    vertexMap = IMS.fromList [ (v, k) | (v, (k, _)) <- indexedEdges ]
    graph = IMS.fromList
              [ (v, Y.mapMaybe (\k -> MS.lookup k keyToVertex) neighbors)
              | (v, (_, neighbors)) <- indexedEdges ]
    vertexToKey v = vertexMap IMS.! v

initialState :: TarjanState
initialState = TarjanState 0 IMS.empty IMS.empty [] DS.empty []

-- | Pop vertices off the stack until the given vertex is reached,
-- collecting the current strongly connected component.
popStackUntil :: Vertex -> CMS.State TarjanState [Vertex]
popStackUntil v = go []
  where
    go acc = do
      st <- CMS.get
      case stack st of
        [] -> error "popStackUntil: empty stack"
        (x:xs) -> do
          CMS.modify $ \s -> s { stack = xs, onStack = DS.delete x (onStack s) }
          let acc' = x : acc
          if x == v
            then return (reverse acc')
            else go acc'

-- | Visit a vertex and recursively explore its successors.
strongConnect :: Graph -> Vertex -> CMS.State TarjanState ()
strongConnect graph v = do
  st <- CMS.get
  let !i = indexCounter st
  CMS.modify $ \s -> s { indexCounter = i + 1
                   , indices      = IMS.insert v i (indices s)
                   , lowlinks     = IMS.insert v i (lowlinks s)
                   , stack        = v : stack s
                   , onStack      = DS.insert v (onStack s)
                   }
  let neighbors = IMS.findWithDefault [] v graph
  CM.forM_ neighbors $ \w -> do
    st' <- CMS.get
    if not (IMS.member w (indices st'))
      then do
         strongConnect graph w
         stAfter <- CMS.get
         let low_v = IMS.findWithDefault maxBound v (lowlinks stAfter)
             low_w = IMS.findWithDefault maxBound w (lowlinks stAfter)
         CMS.modify $ \s -> s { lowlinks = IMS.insert v (min low_v low_w) (lowlinks s) }
      else if DS.member w (onStack st')
        then do
         let low_v = IMS.findWithDefault maxBound v (lowlinks st')
             idx_w = IMS.findWithDefault maxBound w (indices st')
         CMS.modify $ \s -> s { lowlinks = IMS.insert v (min low_v idx_w) (lowlinks s) }
        else return ()
  stFinal <- CMS.get
  let low_v = IMS.findWithDefault maxBound v (lowlinks stFinal)
      idx_v = IMS.findWithDefault maxBound v (indices stFinal)
  CM.when (low_v == idx_v) $ do
    comp <- popStackUntil v
    CMS.modify $ \s -> s { sccs = comp : sccs s }

-- | Compute the strongly connected components of the given graph.
-- The components are returned in reverse topological order.
stronglyConnectedComponents :: Graph -> [[Vertex]]
stronglyConnectedComponents graph = reverse $ fmap L.sort $ sccs finalState
  where
    verts = IMS.keys graph
    finalState = CMS.execState (mapM_ (\v -> do
                      visited <- CMS.gets (IMS.member v . indices)
                      if not visited then strongConnect graph v else return ()
                    ) verts) initialState
